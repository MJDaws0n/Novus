// Novus WebKit App — client-side JS

(function () {
  'use strict';

  var logEl = document.getElementById('log');
  var inputEl = document.getElementById('msg-input');
  var msgCount = 0;

  // -----------------------------------------------------------------------
  // Send a message to Novus (via the WebKit bridge)
  // -----------------------------------------------------------------------
  window.sendMessage = function () {
    var text = inputEl.value.trim();
    if (!text) return;

    // novusSend is injected by the window manager (WKUserScript)
    if (typeof novusSend === 'function') {
      novusSend(text);
      appendLog('sent', 'You → Novus', text);
    } else {
      appendLog('sent', 'Error', 'novusSend not available (not running in Novus WebView)');
    }

    inputEl.value = '';
    inputEl.focus();
  };

  // Allow Enter key to send
  inputEl.addEventListener('keydown', function (e) {
    if (e.key === 'Enter') {
      e.preventDefault();
      window.sendMessage();
    }
  });

  // -----------------------------------------------------------------------
  // Receive messages from Novus
  // -----------------------------------------------------------------------
  if (typeof novusOnMessage === 'function') {
    novusOnMessage(function (msg) {
      appendLog('received', 'Novus → You', msg);
    });
  }

  // -----------------------------------------------------------------------
  // Log helper
  // -----------------------------------------------------------------------
  function appendLog(type, label, text) {
    msgCount++;
    var entry = document.createElement('div');
    entry.className = 'log-entry ' + type;
    entry.innerHTML =
      '<span class="label">[' + msgCount + '] ' + escapeHtml(label) + ':</span>' +
      escapeHtml(text);
    logEl.appendChild(entry);
    logEl.scrollTop = logEl.scrollHeight;
  }

  function escapeHtml(s) {
    var div = document.createElement('div');
    div.textContent = s;
    return div.innerHTML;
  }

  // Send an initial greeting so Novus knows the page is alive
  setTimeout(function () {
    if (typeof novusSend === 'function') {
      novusSend('hello from the web page!');
      appendLog('sent', 'Auto', 'hello from the web page!');
    }
  }, 500);
})();
