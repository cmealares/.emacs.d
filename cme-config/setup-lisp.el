;;; -----------------------------------------------------------------------
;;;; PAREDIT - Lisp structured editing
;;; https://www.emacswiki.org/emacs/ParEdit
;;; -----------------------------------------------------------------------

;; tagedit fait la meme chose pour html

(use-package paredit
  :diminish
  :init
  (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode))


;;; -----------------------------------------------------------------------
;;;; ELDOC
;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
;;; -----------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;;; -----------------------------------------------------------------------
;;;; LISP
;;; -----------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (cme-disable-tabs)))

(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))

;;; my lisp is sbcl
;;; (setq inferior-lisp-program "nohup sbcl")
;;;
;;; ;;; online documentation
;;; (setq common-lisp-hyperspec-root "http://www.lispworks.com/reference/HyperSpec/")
;;;
;;; ;;(setq common-lisp-hyperspec-symbol-table
;;; ;;       "/local/doc/HyperSpec/Data/Map_Sym.txt") ;i had to download this file
;;;
;;; (setq cltl2-root-url  "http://www.uuhaus.de/cltl2/")
;;;
;;; ;;; slime setup
;;; (when (file-exists-p "/usr/local/share/emacs/site-lisp/slime-2.0")
;;;   (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/slime-2.0")
;;;   (setq slime-startup-animation nil)
;;;   (setq slime-kill-without-query-p t) ; quit emacs faster
;;;   (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
;;;   (require 'slime)
;;;   (slime-setup :autodoc t)
;;;
;;;   (add-hook 'sldb-hook 'sldb-print-condition)) ; give errors with max detail
;;;
