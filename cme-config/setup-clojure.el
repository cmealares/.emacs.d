;;; -----------------------------------------------------------------------
;;;; CLOJURE
;; https://github.com/clojure-emacs/clojure-mode
;;; -----------------------------------------------------------------------


(use-package clojure-mode
  :ensure t

  :mode (("\\.edn$" . clojure-mode)
         ("\\.boot$" . clojure-mode)
         ("\\.cljs.*$" . clojure-mode))

  :hook (clojure-mode paredit-mode))

;; extra syntax highlighting for clojure
(use-package clojure-mode-extra-font-locking
  :requires clojure-mode)


;;; -----------------------------------------------------------------------
;;;; CIDER :
;; Integration with a Clojure REPL
;; https://docs.cider.mx/
;;; -----------------------------------------------------------------------

(use-package cider
  :ensure t
  :pin melpa-stable


  :init
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

  ;; font-lock for all namespaces
  ;(setq cider-font-lock-dynamically '(macro core function var))



  )
