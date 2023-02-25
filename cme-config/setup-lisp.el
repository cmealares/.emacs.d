
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
