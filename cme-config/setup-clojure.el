;;; -----------------------------------------------------------------------
;;; CLOJURE
;;; https://github.com/clojure-emacs/clojure-mode
;;; -----------------------------------------------------------------------

(use-package clojure-mode
  ;;;:disabled
  :pin melpa
  :mode (("\\.edn$" . clojure-mode)
         ("\\.boot$" . clojure-mode)
         ("\\.cljs.*$" . clojure-mode)))

;; extra syntax highlighting for clojure
(use-package clojure-mode-extra-font-locking
  :disabled
  :after clojure-mode)


;;; -----------------------------------------------------------------------
;;; CIDER :
;;; Integration with a Clojure REPL
;;; https://docs.cider.mx/
;;; -----------------------------------------------------------------------
(use-package cider
  ;;:disabled
  :pin melpa

  :hook (cider-repl-mode . rainbow-delimiters-mode)

  ;;:init
  ;; font-lock for all namespaces
                                        ;(setq cider-font-lock-dynamically '(macro core function var))
)


;;; -----------------------------------------------------------------------
;;; PAREDIT - Lisp structured editing
;;; https://www.emacswiki.org/emacs/ParEdit
;;; -----------------------------------------------------------------------

;; tagedit fait la meme chose pour html

(use-package paredit
  :diminish
  :hook ((clojure-mode . enable-paredit-mode)
         (cider-repl-mode . enable-paredit-mode)))
