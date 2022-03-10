;;; -----------------------------------------------------------------------
;;;; LANGUAGE SERVERS for ts and js
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/
;;; -----------------------------------------------------------------------
;; npm install -g typescript-language-server;
;; npm install -g typescript

;;; -----------------------------------------------------------------------
;;;; JAVASCRIPT-MODE
;;; -----------------------------------------------------------------------

;; See also
;; https://tychoish.com/post/emacs-and-lsp-mode/
;; https://www.chadstovern.com/javascript-in-emacs-revisited/

(use-package js2-mode
  :mode "\\.js$"
  :hook (js2-mode . lsp-deferred)
  :config
  (setq js-indent-level 2))


;;; -----------------------------------------------------------------------
;;;; TYPESCRIPT-MODE
;;; -----------------------------------------------------------------------
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)

  ;; DAP-NODE : nodejs debugging
  ;; https://emacs-lsp.github.io/dap-mode/page/configuration/
  ;; https://code.visualstudio.com/docs/nodejs/nodejs-debugging
  (require 'dap-node)
  (dap-node-setup) ; install node debug adapter if needed
  )
