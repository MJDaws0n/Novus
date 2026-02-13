// mac_silicon_window_manager (C)
//
// Minimal "window" manager for macOS using osascript dialogs.
// Exposes a simple line-based protocol over a UNIX domain socket:
//
//   TITLE <text>\n
//   TEXT <text>\n
//   SHOW\n           (shows a dialog using current TITLE/TEXT)
//   HIDE\n           (no-op; dialogs are owned by osascript)
//   PING\n  -> PONG\n
//   QUIT\n
//
// Build (Apple Silicon):
//   clang -O2 -Wall -Wextra -std=c11 \
//     lib/mac_silicon_window_manager/unbuilt/app.c \
//     -o lib/mac_silicon_window_manager/window_manager
//
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <unistd.h>

#ifndef SUN_LEN
#define SUN_LEN(su) ((socklen_t)(sizeof(*(su)) - sizeof((su)->sun_path) + strlen((su)->sun_path)))
#endif

#define DEFAULT_SOCKET_PATH "/tmp/novus_wm.sock"

struct state {
    char title[256];
    char text[2048];
};

static void usage(const char *argv0) {
    fprintf(stderr,
            "Usage: %s [--socket PATH] [--title TITLE] [--text TEXT] [--auto-show]\n",
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

static void escape_osa(const char *in, char *out, size_t cap) {
    // Escapes for AppleScript string literal inside double-quotes.
    if (cap == 0) return;
    size_t w = 0;
    for (size_t i = 0; in[i] != '\0'; i++) {
        const char c = in[i];
        const char *rep = NULL;
        char tmp[2] = {0, 0};

        if (c == '\\') rep = "\\\\";
        else if (c == '"') rep = "\\\"";
        else if (c == '\n') rep = "\\n";
        else if (c == '\r') rep = "";
        else {
            tmp[0] = c;
            rep = tmp;
        }

        for (size_t j = 0; rep[j] != '\0'; j++) {
            if (w + 1 >= cap) {
                out[w] = '\0';
                return;
            }
            out[w++] = rep[j];
        }
    }
    out[w] = '\0';
}

static void show_dialog(const struct state *st) {
    char esc_title[512];
    char esc_text[4096];
    escape_osa(st->title, esc_title, sizeof esc_title);
    escape_osa(st->text, esc_text, sizeof esc_text);

    char script[8192];
    snprintf(script, sizeof script,
             "display dialog \"%s\" with title \"%s\" buttons {\"OK\"} default button 1",
             esc_text, esc_title);

    pid_t pid = fork();
    if (pid == 0) {
        const char *osascript = "/usr/bin/osascript";
        char *const argv[] = {(char *)osascript, (char *)"-e", script, NULL};
        execv(osascript, argv);
        _exit(127);
    }

    // Parent: don't block the socket server; reap child later.
}

static int make_server(const char *sock_path) {
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) {
        perror("socket");
        return -1;
    }

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof addr);
    addr.sun_family = AF_UNIX;
    strlcpy0(addr.sun_path, sock_path, sizeof addr.sun_path);

    unlink(sock_path);
    if (bind(fd, (struct sockaddr *)&addr, SUN_LEN(&addr)) < 0) {
        perror("bind");
        close(fd);
        return -1;
    }
    if (listen(fd, 4) < 0) {
        perror("listen");
        close(fd);
        return -1;
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

static void reap_children_nonblocking(void) {
    int status = 0;
    while (waitpid(-1, &status, WNOHANG) > 0) {
    }
}

static void handle_cmd(struct state *st, int client, const char *line) {
    if (strcmp(line, "PING") == 0) {
        (void)write(client, "PONG\n", 5);
        return;
    }
    if (strcmp(line, "SHOW") == 0) {
        show_dialog(st);
        (void)write(client, "OK\n", 3);
        return;
    }
    if (strcmp(line, "HIDE") == 0) {
        // No-op: dialogs are owned by osascript and can be closed by the user.
        (void)write(client, "OK\n", 3);
        return;
    }
    if (strcmp(line, "QUIT") == 0) {
        (void)write(client, "BYE\n", 4);
        return;
    }

    const char *sp = strchr(line, ' ');
    if (!sp) {
        (void)write(client, "ERR unknown\n", 12);
        return;
    }

    size_t cmd_len = (size_t)(sp - line);
    const char *arg = sp + 1;

    if (cmd_len == 5 && strncmp(line, "TITLE", 5) == 0) {
        strlcpy0(st->title, arg, sizeof st->title);
        (void)write(client, "OK\n", 3);
        return;
    }
    if (cmd_len == 4 && strncmp(line, "TEXT", 4) == 0) {
        strlcpy0(st->text, arg, sizeof st->text);
        (void)write(client, "OK\n", 3);
        return;
    }

    (void)write(client, "ERR unknown\n", 12);
}

int main(int argc, char **argv) {
    const char *sock_path = DEFAULT_SOCKET_PATH;
    const char *title = "Novus Window";
    const char *text = "Hello from Novus.";
    bool auto_show = false;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--socket") == 0 && i + 1 < argc) {
            sock_path = argv[++i];
        } else if (strcmp(argv[i], "--title") == 0 && i + 1 < argc) {
            title = argv[++i];
        } else if ((strcmp(argv[i], "--text") == 0 || strcmp(argv[i], "--message") == 0) && i + 1 < argc) {
            text = argv[++i];
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

    struct state st;
    memset(&st, 0, sizeof st);
    strlcpy0(st.title, title, sizeof st.title);
    strlcpy0(st.text, text, sizeof st.text);

    int server = make_server(sock_path);
    if (server < 0) return 1;

    if (auto_show) {
        show_dialog(&st);
    }

    fprintf(stderr, "[window_manager] listening on %s\n", sock_path);

    bool quitting = false;
    while (!quitting) {
        reap_children_nonblocking();

        int client = accept(server, NULL, NULL);
        if (client < 0) {
            if (errno == EINTR) continue;
            perror("accept");
            break;
        }

        char line[4096];
        while (1) {
            ssize_t n = read_line(client, line, sizeof line);
            if (n == 0) break;
            if (n < 0) break;

            if (strcmp(line, "QUIT") == 0) {
                handle_cmd(&st, client, line);
                quitting = true;
                break;
            }
            handle_cmd(&st, client, line);
        }

        close(client);
    }

    close(server);
    unlink(sock_path);
    return 0;
}
