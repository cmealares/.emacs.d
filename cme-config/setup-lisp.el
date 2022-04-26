;;; -----------------------------------------------------------------------
;;; PAREDIT - Lisp structured editing
;;; https://www.emacswiki.org/emacs/ParEdit
;;; -----------------------------------------------------------------------

;; tagedit fait la meme chose pour html

(use-package paredit
  :diminish
  :hook ((emacs-lisp-mode       . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (lisp-mode             . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)) )

;;; -----------------------------------------------------------------------
;;; ELDOC
;;; eldoc-mode shows documentation in the minibuffer when writing code
;;; http://www.emacswiki.org/emacs/ElDoc
;;; -----------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;;; -----------------------------------------------------------------------
;;; LISP
;;; -----------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (cme-disable-tabs)))

(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))
