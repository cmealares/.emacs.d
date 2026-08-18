;;; -*- lexical-binding: t -*-

;;; ----------------------------------------------------------------------
;;; DIRED
;;  C-x C-q: edit dired buffer (enter wdired)
;;  j: jump to a file
;;  ^: open parent folder
;;; ----------------------------------------------------------------------
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (when win32-p
    ;; options for the ls emulation on windows
    (setq ls-lisp-dirs-first t)
    (setq ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")))
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t) )

;; from Prot
(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))
