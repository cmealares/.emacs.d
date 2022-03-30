;;; -----------------------------------------------------------------------
;;; LANGUAGE SERVERS for ts and js
;;; -----------------------------------------------------------------------

;; https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/
;; npm install -g typescript-language-server
;; npm install -g typescript

;; https://emacs-lsp.github.io/lsp-mode/page/lsp-eslint/
;; npm install -g eslint
;; M-x lsp-install-server RET eslint

;;; -----------------------------------------------------------------------
;;; WEB DEV
;;; https://web-mode.org/
;;; -----------------------------------------------------------------------
(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :hook (web-mode . (lambda ()
                      (when (or (string-equal "js" (file-name-extension buffer-file-name))
                                (string-equal "jsx" (file-name-extension buffer-file-name))
                                (string-equal "ts" (file-name-extension buffer-file-name))
                                (string-equal "tsx" (file-name-extension buffer-file-name)))
                        ;; does not seem to be implemented yet
                        ;; instead manually call lsp-eslint-fix-all
                        (setq lsp-eslint-auto-fix-on-save t)
                        (lsp-deferred)) )))

;;; -----------------------------------------------------------------------
;;; EMMET
;;; https://github.com/smihica/emmet-mode
;;; C-j
;;; -----------------------------------------------------------------------
(use-package emmet-mode
  :disabled
  ;; :config
  ;;(add-hook 'web-mode-hook 'emmet-mode)  ;; autostart on any markup modes
  ;;(add-hook 'css-mode-hook  'emmet-mode) ;; autostart on css mode
  )

;;; -----------------------------------------------------------------------
;;; NOT USED
;;; -----------------------------------------------------------------------
(use-package js2-mode
  :disabled
  :mode "\\.js$"
  :hook (js2-mode . lsp-deferred)
  :config
  (setq js-indent-level 2))

(use-package typescript-mode
  :disabled
  :mode "\\.ts$"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)

  ;; DAP-NODE : nodejs debugging
  ;; https://emacs-lsp.github.io/dap-mode/page/configuration/
  ;; https://code.visualstudio.com/docs/nodejs/nodejs-debugging
  (require 'dap-node)
  (dap-node-setup)) ; install node debug adapter if needed
