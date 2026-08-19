;;; -*- lexical-binding: t -*-

;;; -----------------------------------------------------------------------
;;;; TABS SETTINGS
;;; -----------------------------------------------------------------------
(defun cme-disable-tabs ()
  (interactive)
  (setq indent-tabs-mode nil))

(defun cme-enable-tabs  ()
  (interactive)
  (setq indent-tabs-mode t))

;;; -----------------------------------------------------------------------
;;;; OPEN OS's FILE EXPLORER
;;; -----------------------------------------------------------------------
(when win32-p
  (defun cme-open-file-explorer ()
    "Open the current file in OS's file manager."
    (interactive)
    (start-process "ofe" nil "explorer.exe" "."))

  (defalias 'ofe   'cme-open-file-explorer))

(defun cme-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun cme-pretty-xml ()
  "Reformat xml buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (replace-match ">\n<" nil t))
    (indent-region (point-min) (point-max)))
  (message "I have re-formatted"))

;;; -----------------------------------------------------------------------
;;;; Increment number
;;; -----------------------------------------------------------------------
(defun cme-add-number (arg)
  "Add arg to number under cursor. If no arg, add 1."
  (interactive "p")
  (insert (format " %d" (+ (read (current-buffer)) arg)))
  (backward-word 1))

;; inplace increment : define a macro
(fset 'cme-increment "\C-c\C-i\C-@\M-b\C-w")

(global-set-key "\C-cj"    'cme-add-number)
(global-set-key "\C-ci"    'cme-increment)
