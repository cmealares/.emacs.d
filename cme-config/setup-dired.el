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

;;; -----------------------------------------------------------------------
;;;; Do things on each dired marked file
;;; -----------------------------------------------------------------------
(defun cme-for-each-dired-marked-file(fn)
  "Do stuff for each marked file, only works in dired window"
  (interactive "aFunction:")
  (unless (eq major-mode 'dired-mode)
    (error (format "Not a Dired buffer \(%s\)" major-mode)))

  (let ((filenames (dired-get-marked-files)))
    (mapcar fn filenames))
  (revert-buffer t t)) ;refreshed dired buf

(defun cme-edit-each-marked-file ()
  "Open each marked file and enter recursive edit (Please exit with C-M-c)"
  (interactive)
  (message "Exit recursive edit with C-M-c")
  (cme-for-each-dired-marked-file
   (lambda (pathfile)
     (find-file pathfile)
     (goto-char (point-min))
     (recursive-edit) ))
  (message "All files done !!!"))
