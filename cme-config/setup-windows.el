;;; Buffer and window settings. -*- lexical-binding: t -*-

(when window-system
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 90)))

;; ----------------------------------------------------------------------
;; Improve performance problems of big files
;; ----------------------------------------------------------------------
(defun cme-find-big-file-hook ()
  "If a file is over a given size, activate some optimizations."
  (when (> (buffer-size) (* 1024 1024))
    (buffer-disable-undo)
    (set (make-local-variable 'mouse-wheel-scroll-amount) '(1)) ;scroll only one line
    (set (make-local-variable 'mouse-wheel-progressive-speed) nil) ;do not accelerate scrolling
    (set (make-variable-buffer-local 'line-number-mode) nil)
    (set (make-variable-buffer-local 'column-number-mode) nil)
    (set (make-variable-buffer-local 'buffer-read-only) t)
    (set (make-variable-buffer-local 'bidi-display-reordering) nil) ))

(add-hook 'find-file-hook 'cme-find-big-file-hook)

;; ----------------------------------------------------------------------
;; RAINBOW Colorize color names in buffers
;; ----------------------------------------------------------------------
(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode)

;; ----------------------------------------------------------------------
;; BEACON
;; light to follow the cursor
;; ----------------------------------------------------------------------
(use-package beacon
  :ensure t
  :defer 5
  :diminish
  :config
  (setq beacon-size 80)
  (setq beacon-color "#FF6600")
  (beacon-mode 1))

;; highlight current line
(when window-system (global-hl-line-mode 1))

;; ----------------------------------------------------------------------
;; UNIQUIFY - how buffer names are made unique
;; ----------------------------------------------------------------------
(use-package emacs
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\*") ) ; do not uniquify these

;; ----------------------------------------------------------------------
;; MIDNIGHT - clean-buffer-list at midnight
;; ----------------------------------------------------------------------
(use-package midnight
  :ensure t
  :defer 5
  :init
  ;; nb of days before a buffer becomes eligible for autokilling
  (setq clean-buffer-list-delay-general 3)
  :config (midnight-mode 1))

;; ----------------------------------------------------------------------
;; WINNER-MODE - navigate in window config with C-c right/left
;; ----------------------------------------------------------------------
(use-package winner
  :ensure nil
  :defer 1
  :config (winner-mode 1))

;; ----------------------------------------------------------------------
;; WINDMOVE - navigate buffers with S-arrow
;; ----------------------------------------------------------------------
(use-package windmove
  :ensure nil
  :defer 5
  :config
  (windmove-default-keybindings))

;; -----------------------------------------------------------------------
;; http://www.emacswiki.org/emacs/MoveRegion
;; -----------------------------------------------------------------------
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

;; -----------------------------------------------------------------------
;; More functions on buffers
;; -----------------------------------------------------------------------
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

(defun cme-revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (dolist (buf (buffer-list))
    (unless (null (buffer-file-name buf))
      (set-buffer (buffer-name buf))
      (revert-buffer t t)))
  (message "All buffers reverted!"))
