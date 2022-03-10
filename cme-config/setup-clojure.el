;;; -----------------------------------------------------------------------
;;;; CLOJURE
;;; -----------------------------------------------------------------------

;; https://github.com/clojure-emacs/clojure-mode

(use-package clojure-mode
  :disabled

  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
)

;; extra syntax highlighting for clojure
(use-package clojure-mode-extra-font-locking
  :requires clojure-mode)


;; Integration with a Clojure REPL
;; https://github.com/clojure-emacs/cider
;;(use-package cider
;;  :ensure t
;;  :requires clojure-mode
;;  :pin melpa-stable)

    ;; colorful parenthesis matching
   ; rainbow-delimiters
