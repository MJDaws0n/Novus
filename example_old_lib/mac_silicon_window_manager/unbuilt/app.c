// mac_silicon_window_manager (macOS Apple Silicon)
//
// WebKit-based window manager for Novus.
// Exposes a line-based protocol over a UNIX domain socket:
//
//   TITLE <text>\n           → OK\n        — set window title
//   SERVE <root_dir>\n       → PORT <n>\n  — start HTTP static file server
//   NAVIGATE <url>\n         → OK\n        — load URL in WebView
//   JSEVAL <code>\n          → OK\n        — evaluate JS in page
//   SHOW\n                   → OK\n        — show window
//   HIDE\n                   → OK\n        — hide window
//   PING\n                   → PONG\n
//   QUIT\n                   → BYE\n       — terminate
//
// When JS calls window.novusSend(msg), the server pushes:
//   JSMSG <escaped_msg>\n
// to the connected client.
//
// Build (Apple Silicon):
//   clang -O2 -Wall -Wextra -fobjc-arc -x objective-c \
//     lib/mac_silicon_window_manager/unbuilt/app.c \
//     -framework Cocoa -framework WebKit \
//     -o lib/mac_silicon_window_manager/window_manager
//
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <pthread.h>
#include <signal.h>

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>
#include <dispatch/dispatch.h>

#ifndef SUN_LEN
#define SUN_LEN(su) ((socklen_t)(sizeof(*(su)) - sizeof((su)->sun_path) + strlen((su)->sun_path)))
#endif

#define DEFAULT_SOCKET_PATH "/tmp/novus_wm.sock"
#define MAX_PATH_LEN 4096
#define HTTP_BUF_SIZE 65536
#define READ_BUF_SIZE 65536

// ---------------------------------------------------------------------------
// Forward declarations
// ---------------------------------------------------------------------------
static void usage(const char *argv0);
static void strlcpy0(char *dst, const char *src, size_t cap);

// HTTP server
static int start_http_server(const char *root_dir);
static void *http_server_thread(void *arg);
static void handle_http_client(int client_fd, const char *root_dir);
static const char *content_type_for_ext(const char *path);
static bool path_is_safe(const char *path);

// ---------------------------------------------------------------------------
// Global state
// ---------------------------------------------------------------------------
static NSWindow *g_window = nil;
static WKWebView *g_webview = nil;
static int g_client_fd = -1;
static char g_title[256] = "Novus Window";
static char g_http_root[MAX_PATH_LEN] = "";
static int g_http_port = 0;
static int g_http_server_fd = -1;

// ---------------------------------------------------------------------------
// Utility
// ---------------------------------------------------------------------------

static void usage(const char *argv0) {
    fprintf(stderr,
            "Usage: %s [--socket PATH] [--title TITLE] [--auto-show]\n",
            argv0);
}

static void strlcpy0(char *dst, const char *src, size_t cap) {
    if (cap == 0) return;
    if (!src) src = "";
    size_t n = strlen(src);
    if (n >= cap) n = cap - 1;
    memcpy(dst, src, n);
    dst[n] = '\0';
}

static NSString *ns_from_utf8(const char *s) {
    if (!s) return @"";
    return [NSString stringWithUTF8String:s] ?: @"";
}

// ---------------------------------------------------------------------------
// WKWebView + Script Message Handler (JS → Novus bridge)
// ---------------------------------------------------------------------------

@interface NovusMessageHandler : NSObject <WKScriptMessageHandler>
@end

@implementation NovusMessageHandler
- (void)userContentController:(WKUserContentController *)controller
      didReceiveScriptMessage:(WKScriptMessage *)message {
    (void)controller;
    if (![message.name isEqualToString:@"novus"]) return;

    NSString *body = [NSString stringWithFormat:@"%@", message.body];
    const char *utf8 = [body UTF8String];
    if (!utf8) return;

    // Push JSMSG to connected Novus client
    if (g_client_fd >= 0) {
        // Escape the message for the line protocol
        size_t slen = strlen(utf8);
        size_t esc_cap = slen * 2 + 1;
        if (esc_cap < 8192) esc_cap = 8192;
        char *escaped = (char *)malloc(esc_cap);
        size_t w = 0;
        for (size_t i = 0; utf8[i] != '\0'; i++) {
            if (utf8[i] == '\\') {
                if (w + 2 < esc_cap) { escaped[w++] = '\\'; escaped[w++] = '\\'; }
            } else if (utf8[i] == '\n') {
                if (w + 2 < esc_cap) { escaped[w++] = '\\'; escaped[w++] = 'n'; }
            } else if (utf8[i] == '\r') {
                // skip
            } else {
                if (w + 1 < esc_cap) escaped[w++] = utf8[i];
            }
        }
        escaped[w] = '\0';

        char *line = (char *)malloc(w + 16);
        int ln = snprintf(line, w + 16, "JSMSG %s\n", escaped);
        (void)write(g_client_fd, line, (size_t)ln);
        free(escaped);
        free(line);
    }
}
@end

// ---------------------------------------------------------------------------
// Window delegate — detect red close button
// ---------------------------------------------------------------------------

@interface NovusWindowDelegate : NSObject <NSWindowDelegate>
@end

@implementation NovusWindowDelegate
- (BOOL)windowShouldClose:(NSWindow *)sender {
    (void)sender;
    // Notify the connected Novus client that the window is closing
    if (g_client_fd >= 0) {
        const char *msg = "JSMSG CMD:QUIT\n";
        (void)write(g_client_fd, msg, strlen(msg));
    }
    // Small delay so the message is read before we tear down
    usleep(50000);
    [NSApp terminate:nil];
    return NO; // We handle closing via terminate
}
@end

static NovusWindowDelegate *g_win_delegate = nil;

@interface NovusNavDelegate : NSObject <WKNavigationDelegate>
@end

@implementation NovusNavDelegate
- (void)webView:(WKWebView *)webView didFinishNavigation:(WKNavigation *)navigation {
    (void)webView; (void)navigation;
}
- (void)webView:(WKWebView *)webView didFailNavigation:(WKNavigation *)navigation withError:(NSError *)error {
    (void)navigation;
    fprintf(stderr, "[window_manager] navigation failed: %s\n",
            [[error localizedDescription] UTF8String]);
}
@end

static NovusMessageHandler *g_msg_handler = nil;
static NovusNavDelegate *g_nav_delegate = nil;

// ---------------------------------------------------------------------------
// UI (Cocoa + WebKit)
// ---------------------------------------------------------------------------

static void ui_ensure_window(void) {
    if (g_window) return;

    NSRect frame = NSMakeRect(0, 0, 900, 600);
    NSUInteger style = NSWindowStyleMaskTitled
                     | NSWindowStyleMaskClosable
                     | NSWindowStyleMaskMiniaturizable
                     | NSWindowStyleMaskResizable;
    g_window = [[NSWindow alloc] initWithContentRect:frame
                                           styleMask:style
                                             backing:NSBackingStoreBuffered
                                               defer:NO];
    [g_window center];
    [g_window setReleasedWhenClosed:NO];
    [g_window setTitle:ns_from_utf8(g_title)];

    // Set window delegate to detect close button
    g_win_delegate = [[NovusWindowDelegate alloc] init];
    [g_window setDelegate:g_win_delegate];

    // Configure WebView with user content controller for JS bridge
    WKWebViewConfiguration *config = [[WKWebViewConfiguration alloc] init];
    WKUserContentController *ucc = [[WKUserContentController alloc] init];

    g_msg_handler = [[NovusMessageHandler alloc] init];
    [ucc addScriptMessageHandler:g_msg_handler name:@"novus"];

    // Inject the novusSend() / novusOnMessage() bridge into every page
    NSString *bridgeJS =
        @"window.novusSend = function(msg) {"
         "  window.webkit.messageHandlers.novus.postMessage(msg);"
         "};"
         "window._novusCallbacks = [];"
         "window.novusOnMessage = function(cb) {"
         "  window._novusCallbacks.push(cb);"
         "};"
         "window._novusReceive = function(msg) {"
         "  for (var i = 0; i < window._novusCallbacks.length; i++) {"
         "    try { window._novusCallbacks[i](msg); } catch(e) { console.error(e); }"
         "  }"
         "};";
    WKUserScript *script = [[WKUserScript alloc]
        initWithSource:bridgeJS
        injectionTime:WKUserScriptInjectionTimeAtDocumentStart
        forMainFrameOnly:YES];
    [ucc addUserScript:script];

    config.userContentController = ucc;

    // Enable developer extras (right-click → Inspect Element)
    [config.preferences setValue:@YES forKey:@"developerExtrasEnabled"];

    g_webview = [[WKWebView alloc] initWithFrame:[[g_window contentView] bounds]
                                   configuration:config];
    [g_webview setAutoresizingMask:(NSViewWidthSizable | NSViewHeightSizable)];

    g_nav_delegate = [[NovusNavDelegate alloc] init];
    [g_webview setNavigationDelegate:g_nav_delegate];

    [[g_window contentView] addSubview:g_webview];
}

static void ui_set_title_async(const char *title) {
    NSString *t = ns_from_utf8(title);
    dispatch_async(dispatch_get_main_queue(), ^{
        ui_ensure_window();
        [g_window setTitle:t];
    });
}

static void ui_navigate_async(const char *url) {
    NSString *u = ns_from_utf8(url);
    dispatch_async(dispatch_get_main_queue(), ^{
        ui_ensure_window();
        NSURL *nsurl = [NSURL URLWithString:u];
        NSURLRequest *req = [NSURLRequest requestWithURL:nsurl];
        [g_webview loadRequest:req];
    });
}

static void ui_jseval_async(const char *code) {
    NSString *js = ns_from_utf8(code);
    dispatch_async(dispatch_get_main_queue(), ^{
        ui_ensure_window();
        [g_webview evaluateJavaScript:js completionHandler:^(id result, NSError *error) {
            if (error) {
                fprintf(stderr, "[window_manager] JS eval error: %s\n",
                        [[error localizedDescription] UTF8String]);
            }
            (void)result;
        }];
    });
}

static void ui_show_async(void) {
    dispatch_async(dispatch_get_main_queue(), ^{
        ui_ensure_window();
        [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
        [NSApp activateIgnoringOtherApps:YES];
        [g_window makeKeyAndOrderFront:nil];
    });
}

static void ui_hide_async(void) {
    dispatch_async(dispatch_get_main_queue(), ^{
        if (g_window) [g_window orderOut:nil];
    });
}

static void ui_quit_async(void) {
    dispatch_async(dispatch_get_main_queue(), ^{
        [NSApp terminate:nil];
    });
}

// ---------------------------------------------------------------------------
// HTTP static file server
// ---------------------------------------------------------------------------

struct http_server_ctx {
    int server_fd;
    char root_dir[MAX_PATH_LEN];
};

static bool path_is_safe(const char *path) {
    if (strstr(path, "..") != NULL) return false;
    if (path[0] == '/') return false;
    if (strstr(path, "//") != NULL) return false;
    return true;
}

static const char *content_type_for_ext(const char *path) {
    const char *dot = strrchr(path, '.');
    if (!dot) return "application/octet-stream";
    dot++;
    if (strcasecmp(dot, "html") == 0 || strcasecmp(dot, "htm") == 0) return "text/html; charset=utf-8";
    if (strcasecmp(dot, "css") == 0)  return "text/css; charset=utf-8";
    if (strcasecmp(dot, "js") == 0)   return "application/javascript; charset=utf-8";
    if (strcasecmp(dot, "json") == 0) return "application/json; charset=utf-8";
    if (strcasecmp(dot, "png") == 0)  return "image/png";
    if (strcasecmp(dot, "jpg") == 0 || strcasecmp(dot, "jpeg") == 0) return "image/jpeg";
    if (strcasecmp(dot, "gif") == 0)  return "image/gif";
    if (strcasecmp(dot, "svg") == 0)  return "image/svg+xml";
    if (strcasecmp(dot, "ico") == 0)  return "image/x-icon";
    if (strcasecmp(dot, "woff") == 0) return "font/woff";
    if (strcasecmp(dot, "woff2") == 0) return "font/woff2";
    if (strcasecmp(dot, "ttf") == 0)  return "font/ttf";
    if (strcasecmp(dot, "otf") == 0)  return "font/otf";
    if (strcasecmp(dot, "mp4") == 0)  return "video/mp4";
    if (strcasecmp(dot, "webm") == 0) return "video/webm";
    if (strcasecmp(dot, "mp3") == 0)  return "audio/mpeg";
    if (strcasecmp(dot, "wav") == 0)  return "audio/wav";
    if (strcasecmp(dot, "xml") == 0)  return "application/xml";
    if (strcasecmp(dot, "txt") == 0)  return "text/plain; charset=utf-8";
    if (strcasecmp(dot, "wasm") == 0) return "application/wasm";
    if (strcasecmp(dot, "map") == 0)  return "application/json";
    return "application/octet-stream";
}

static void send_http_error(int fd, int status, const char *msg) {
    char buf[512];
    int n = snprintf(buf, sizeof buf,
        "HTTP/1.1 %d %s\r\n"
        "Content-Type: text/plain\r\n"
        "Content-Length: %zu\r\n"
        "Connection: close\r\n"
        "\r\n"
        "%s", status, msg, strlen(msg), msg);
    (void)write(fd, buf, (size_t)n);
}

static void handle_http_client(int client_fd, const char *root_dir) {
    char req_buf[HTTP_BUF_SIZE];
    ssize_t n = read(client_fd, req_buf, sizeof(req_buf) - 1);
    if (n <= 0) { close(client_fd); return; }
    req_buf[n] = '\0';

    // Parse GET request line
    if (strncmp(req_buf, "GET ", 4) != 0) {
        send_http_error(client_fd, 405, "Method Not Allowed");
        close(client_fd);
        return;
    }

    char *path_start = req_buf + 4;
    char *path_end = strchr(path_start, ' ');
    if (!path_end) { send_http_error(client_fd, 400, "Bad Request"); close(client_fd); return; }
    *path_end = '\0';

    // Remove query string
    char *query = strchr(path_start, '?');
    if (query) *query = '\0';

    // URL-decode path (handle %XX)
    char decoded[MAX_PATH_LEN];
    {
        size_t di = 0;
        for (size_t si = 0; path_start[si] && di + 1 < MAX_PATH_LEN; si++) {
            if (path_start[si] == '%' && path_start[si+1] && path_start[si+2]) {
                char hex[3] = { path_start[si+1], path_start[si+2], '\0' };
                decoded[di++] = (char)strtol(hex, NULL, 16);
                si += 2;
            } else {
                decoded[di++] = path_start[si];
            }
        }
        decoded[di] = '\0';
    }

    // Strip leading /
    const char *rel_path = decoded;
    while (*rel_path == '/') rel_path++;

    // Default to index.html
    if (rel_path[0] == '\0') rel_path = "index.html";

    // Safety check
    if (!path_is_safe(rel_path)) {
        send_http_error(client_fd, 403, "Forbidden");
        close(client_fd);
        return;
    }

    // Build full path
    char full_path[MAX_PATH_LEN * 2];
    snprintf(full_path, sizeof full_path, "%s/%s", root_dir, rel_path);

    // Check if directory — serve index.html inside it
    struct stat st;
    if (stat(full_path, &st) == 0 && S_ISDIR(st.st_mode)) {
        size_t flen = strlen(full_path);
        if (flen > 0 && full_path[flen-1] != '/') {
            full_path[flen] = '/';
            full_path[flen+1] = '\0';
        }
        strlcpy0(full_path + strlen(full_path), "index.html",
                  sizeof(full_path) - strlen(full_path));
        if (stat(full_path, &st) != 0) {
            send_http_error(client_fd, 404, "Not Found");
            close(client_fd);
            return;
        }
    }

    // Open file
    FILE *f = fopen(full_path, "rb");
    if (!f) {
        send_http_error(client_fd, 404, "Not Found");
        close(client_fd);
        return;
    }

    fseek(f, 0, SEEK_END);
    long file_size = ftell(f);
    fseek(f, 0, SEEK_SET);

    const char *ctype = content_type_for_ext(full_path);

    // Send headers
    char hdr[512];
    int hdr_len = snprintf(hdr, sizeof hdr,
        "HTTP/1.1 200 OK\r\n"
        "Content-Type: %s\r\n"
        "Content-Length: %ld\r\n"
        "Cache-Control: no-cache\r\n"
        "Connection: close\r\n"
        "\r\n", ctype, file_size);
    (void)write(client_fd, hdr, (size_t)hdr_len);

    // Stream file body
    char file_buf[READ_BUF_SIZE];
    size_t remaining = (size_t)file_size;
    while (remaining > 0) {
        size_t chunk = remaining < sizeof(file_buf) ? remaining : sizeof(file_buf);
        size_t rd = fread(file_buf, 1, chunk, f);
        if (rd == 0) break;
        (void)write(client_fd, file_buf, rd);
        remaining -= rd;
    }
    fclose(f);
    close(client_fd);
}

static void *http_server_thread(void *arg) {
    struct http_server_ctx *ctx = (struct http_server_ctx *)arg;
    int server_fd = ctx->server_fd;
    char root_dir[MAX_PATH_LEN];
    strlcpy0(root_dir, ctx->root_dir, sizeof root_dir);
    free(ctx);

    // fprintf(stderr, "[http] serving %s on port %d\n", root_dir, g_http_port);

    while (1) {
        int client = accept(server_fd, NULL, NULL);
        if (client < 0) {
            if (errno == EINTR) continue;
            break;
        }
        handle_http_client(client, root_dir);
    }

    close(server_fd);
    return NULL;
}

static int start_http_server(const char *root_dir) {
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) { perror("http socket"); return -1; }

    int opt = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof opt);

    struct sockaddr_in addr;
    memset(&addr, 0, sizeof addr);
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    addr.sin_port = 0; // OS picks a free port

    if (bind(fd, (struct sockaddr *)&addr, sizeof addr) < 0) {
        perror("http bind"); close(fd); return -1;
    }

    // Get assigned port
    socklen_t alen = sizeof addr;
    getsockname(fd, (struct sockaddr *)&addr, &alen);
    g_http_port = ntohs(addr.sin_port);
    g_http_server_fd = fd;

    if (listen(fd, 16) < 0) {
        perror("http listen"); close(fd); return -1;
    }

    strlcpy0(g_http_root, root_dir, sizeof g_http_root);

    struct http_server_ctx *ctx = malloc(sizeof *ctx);
    ctx->server_fd = fd;
    strlcpy0(ctx->root_dir, root_dir, sizeof ctx->root_dir);

    pthread_t tid;
    pthread_create(&tid, NULL, http_server_thread, ctx);
    pthread_detach(tid);

    return g_http_port;
}

// ---------------------------------------------------------------------------
// UNIX socket protocol
// ---------------------------------------------------------------------------

static int make_unix_server(const char *sock_path) {
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) { perror("socket"); return -1; }

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof addr);
    addr.sun_family = AF_UNIX;
    strlcpy0(addr.sun_path, sock_path, sizeof addr.sun_path);

    unlink(sock_path);
    if (bind(fd, (struct sockaddr *)&addr, SUN_LEN(&addr)) < 0) {
        perror("bind"); close(fd); return -1;
    }
    if (listen(fd, 4) < 0) {
        perror("listen"); close(fd); return -1;
    }
    return fd;
}

static ssize_t read_line(int fd, char *buf, size_t cap) {
    size_t n = 0;
    while (n + 1 < cap) {
        char c;
        ssize_t r = read(fd, &c, 1);
        if (r == 0) break;
        if (r < 0) {
            if (errno == EINTR) continue;
            return -1;
        }
        if (c == '\n') break;
        buf[n++] = c;
    }
    buf[n] = '\0';
    return (ssize_t)n;
}

static void rstrip_cr(char *s) {
    if (!s) return;
    size_t n = strlen(s);
    while (n > 0 && (s[n-1] == '\r' || s[n-1] == '\n')) {
        s[n-1] = '\0';
        n--;
    }
}

static void unescape_line_arg(const char *in, char *out, size_t cap) {
    if (cap == 0) return;
    if (!in) in = "";
    size_t w = 0;
    for (size_t i = 0; in[i] != '\0'; i++) {
        if (w + 1 >= cap) break;
        if (in[i] != '\\') { out[w++] = in[i]; continue; }
        const char next = in[i + 1];
        if (next == '\0') { out[w++] = '\\'; break; }
        if (next == 'n')  { out[w++] = '\n'; i++; continue; }
        if (next == '\\') { out[w++] = '\\'; i++; continue; }
        out[w++] = '\\';
        if (w + 1 >= cap) break;
        out[w++] = next;
        i++;
    }
    out[w] = '\0';
}

static void handle_cmd(int client, const char *line, bool *quitting) {
    if (strcmp(line, "PING") == 0) {
        (void)write(client, "PONG\n", 5);
        return;
    }
    if (strcmp(line, "SHOW") == 0) {
        ui_show_async();
        (void)write(client, "OK\n", 3);
        return;
    }
    if (strcmp(line, "HIDE") == 0) {
        ui_hide_async();
        (void)write(client, "OK\n", 3);
        return;
    }
    if (strcmp(line, "QUIT") == 0) {
        (void)write(client, "BYE\n", 4);
        if (quitting) *quitting = true;
        ui_quit_async();
        return;
    }

    const char *sp = strchr(line, ' ');
    if (!sp) {
        (void)write(client, "ERR unknown\n", 12);
        return;
    }

    size_t cmd_len = (size_t)(sp - line);
    const char *arg_raw = sp + 1;

    if (cmd_len == 5 && strncmp(line, "TITLE", 5) == 0) {
        char tmp[256];
        unescape_line_arg(arg_raw, tmp, sizeof tmp);
        strlcpy0(g_title, tmp, sizeof g_title);
        ui_set_title_async(g_title);
        (void)write(client, "OK\n", 3);
        return;
    }

    if (cmd_len == 5 && strncmp(line, "SERVE", 5) == 0) {
        char root[MAX_PATH_LEN];
        unescape_line_arg(arg_raw, root, sizeof root);

        struct stat sst;
        if (stat(root, &sst) != 0 || !S_ISDIR(sst.st_mode)) {
            char err_msg[256];
            snprintf(err_msg, sizeof err_msg, "ERR directory not found: %s\n", root);
            (void)write(client, err_msg, strlen(err_msg));
            return;
        }

        int port = start_http_server(root);
        if (port < 0) {
            (void)write(client, "ERR http server failed\n", 23);
            return;
        }
        char resp[64];
        int rn = snprintf(resp, sizeof resp, "PORT %d\n", port);
        (void)write(client, resp, (size_t)rn);
        return;
    }

    if (cmd_len == 8 && strncmp(line, "NAVIGATE", 8) == 0) {
        char url[MAX_PATH_LEN];
        unescape_line_arg(arg_raw, url, sizeof url);
        ui_navigate_async(url);
        (void)write(client, "OK\n", 3);
        return;
    }

    if (cmd_len == 6 && strncmp(line, "JSEVAL", 6) == 0) {
        char code[8192];
        unescape_line_arg(arg_raw, code, sizeof code);
        ui_jseval_async(code);
        (void)write(client, "OK\n", 3);
        return;
    }

    (void)write(client, "ERR unknown\n", 12);
}

static void server_thread_main(const char *sock_path) {
    int server = make_unix_server(sock_path);
    if (server < 0) {
        fprintf(stderr, "[window_manager] failed to bind %s\n", sock_path);
        ui_quit_async();
        return;
    }

    // fprintf(stderr, "[window_manager] listening on %s\n", sock_path);

    bool quitting = false;
    while (!quitting) {
        int client = accept(server, NULL, NULL);
        if (client < 0) {
            if (errno == EINTR) continue;
            perror("accept");
            break;
        }

        g_client_fd = client;

        char line[8192];
        while (!quitting) {
            ssize_t n = read_line(client, line, sizeof line);
            if (n == 0) break;
            if (n < 0) break;
            rstrip_cr(line);
            handle_cmd(client, line, &quitting);
        }

        g_client_fd = -1;
        close(client);
    }

    close(server);
    unlink(sock_path);

    // If client disconnected, terminate the app
    if (!quitting) {
        ui_quit_async();
    }
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

int main(int argc, char **argv) {
    signal(SIGPIPE, SIG_IGN);

    const char *sock_path = DEFAULT_SOCKET_PATH;
    const char *title = "Novus Window";
    bool auto_show = false;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--socket") == 0 && i + 1 < argc) {
            sock_path = argv[++i];
        } else if (strcmp(argv[i], "--title") == 0 && i + 1 < argc) {
            title = argv[++i];
        } else if (strcmp(argv[i], "--auto-show") == 0) {
            auto_show = true;
        } else if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
            usage(argv[0]);
            return 0;
        } else {
            fprintf(stderr, "Unknown arg: %s\n", argv[i]);
            usage(argv[0]);
            return 2;
        }
    }

    strlcpy0(g_title, title, sizeof g_title);

    @autoreleasepool {
        [NSApplication sharedApplication];
        [NSApp setActivationPolicy:NSApplicationActivationPolicyAccessory];

        if (auto_show) {
            ui_show_async();
        }

        const char *sock_path_dup = strdup(sock_path);
        dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
            @autoreleasepool {
                server_thread_main(sock_path_dup);
                free((void *)sock_path_dup);
            }
        });

        [NSApp run];
    }

    if (g_http_server_fd >= 0) close(g_http_server_fd);

    return 0;
}
