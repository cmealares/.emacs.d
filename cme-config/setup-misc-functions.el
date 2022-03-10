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

;;; -----------------------------------------------------------------------
;;;; http://www.emacswiki.org/emacs/MoveRegion
;;; -----------------------------------------------------------------------
(defun cme-move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun cme-move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (cme-move-region start end (if (null n) -1 (- n))))

(defun cme-move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (cme-move-region start end (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'cme-move-region-up)
(global-set-key (kbd "M-<down>") 'cme-move-region-down)

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

;;; -----------------------------------------------------------------------
;;;; More Functions
;;; -----------------------------------------------------------------------
(defun cme-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun cme-move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun cme-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; useless since we can use
;; C-x C-q : read-only-mode : toggle read only on the buffer
;;
(defun cme-hijack ()
  "Hijack current file: make it writable"
  (interactive)
  (let* ((filename (buffer-file-name))
         (curmodes  (file-modes filename)))
    (set-file-modes filename
                    (logior (string-to-number "0222" 8) ; ugo+w
                            curmodes))
    (read-only-mode nil)
    (message "File mode was: %o New mode: %o"
             curmodes
             (file-modes filename))))

(defun cme-revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (dolist (buf (buffer-list))
    (unless (null (buffer-file-name buf))
      (set-buffer (buffer-name buf))
      (revert-buffer t t)))
  (message "All buffers reverted!"))

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
;;;; Find occurences of some keywords in a file
;;; You can quickly browse files by using this command in conjunction with
;;; mydired script and virtual-dired
;;; -----------------------------------------------------------------------
(defalias 'fo    'cme-occur-keywords)

(defun cme-occur-keywords ()
  (interactive)
  (occur "choices\\|problem" 5)
  (other-window 1))

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
