;; Todo
;; https://tychoish.com/post/emacs-and-lsp-mode/
;; lukewh https://www.youtube.com/watch?v=ELOmzi0RW_8

;; https://www.chadstovern.com/javascript-in-emacs-revisited/
;; rsjsx https://www.thedigitalcatonline.com/blog/2020/07/18/emacs-configuration-for-python-javascript-terraform-and-blogging/

;; draco https://www.youtube.com/playlist?list=PLS0vLC7UzIP9w2uM3ZHSejT8KqlQtZE4Q


;;; -----------------------------------------------------------------------
;;;; LANGUAGE SERVERS for ts and js
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/
;;; -----------------------------------------------------------------------
;; npm install -g typescript-language-server
;; npm install -g typescript

;;; -----------------------------------------------------------------------
;;;; WEB DEV
;;; -----------------------------------------------------------------------

;; https://web-mode.org/
(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :hook (web-mode . lsp-deferred))

;; add project's node_modules/.bin/ directory to the buffer's exec-path
;; (use-package add-node-modules-path
;;   :defer t
;;   :hook (((js2-mode typescript-mode web-mode) . add-node-modules-path)))

;; https://github.com/smihica/emmet-mode
;; C-j
(use-package emmet-mode
  :disabled
  ;; :config
  ;;(add-hook 'web-mode-hook 'emmet-mode)  ;; autostart on any markup modes
  ;;(add-hook 'css-mode-hook  'emmet-mode) ;; autostart on css mode
  )

(use-package prettier-js
  :disabled
  :ensure t
  :hook (((js2-mode typescript-mode web-mode) . prettier-js-mode)))


;;; -----------------------------------------------------------------------
;;;; JAVASCRIPT / TYPESCRIPT
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
