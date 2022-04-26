;;; -----------------------------------------------------------------------
;;; CLOJURE
;;; https://github.com/clojure-emacs/clojure-mode
;;; -----------------------------------------------------------------------

(use-package clojure-mode
  :mode (("\\.edn$" . clojure-mode)
         ("\\.boot$" . clojure-mode)
         ("\\.cljs.*$" . clojure-mode))

  :hook (clojure-mode paredit-mode))

;; extra syntax highlighting for clojure
(use-package clojure-mode-extra-font-locking
  :after clojure-mode)


;;; -----------------------------------------------------------------------
;;; CIDER :
;,;; Integration with a Clojure REPL
;;; https://docs.cider.mx/
;;; -----------------------------------------------------------------------
(use-package cider
  :pin melpa-stable

  :hook ((cider-repl-mode . paredit-mode)
         (cider-repl-mode . rainbow-delimiters-mode))

  ;;:init
  ;; font-lock for all namespaces
  ;(setq cider-font-lock-dynamically '(macro core function var))
  )
