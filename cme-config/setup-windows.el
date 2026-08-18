;;; Buffer and window settings. -*- lexical-binding: t -*-

(when window-system
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 90)))

;;; ----------------------------------------------------------------------
;;; Improve performance problems of big files
;;; ----------------------------------------------------------------------
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

;;; ----------------------------------------------------------------------
;; RAINBOW Colorize color names in buffers
;;; ----------------------------------------------------------------------
(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode)

;;; ----------------------------------------------------------------------
;;; BEACON
;; light to follow the cursor
;;; ----------------------------------------------------------------------
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

;;; ----------------------------------------------------------------------
;;; UNIQUIFY - how buffer names are made unique
;;; ----------------------------------------------------------------------
(use-package emacs
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\*") ) ; do not uniquify these

;;; ----------------------------------------------------------------------
;;; MIDNIGHT - clean-buffer-list at midnight
;;; ----------------------------------------------------------------------
(use-package midnight
  :ensure t
  :defer 5
  :init
  ;; nb of days before a buffer becomes eligible for autokilling
  (setq clean-buffer-list-delay-general 3)
  :config (midnight-mode 1))

;;; ----------------------------------------------------------------------
;;; WINNER-MODE - navigate in window config with C-c right/left
;;; ----------------------------------------------------------------------
(use-package winner
  :ensure nil
  :defer 1
  :config (winner-mode 1))

;;; ----------------------------------------------------------------------
;;; WINDMOVE - navigate buffers with S-arrow
;;; ----------------------------------------------------------------------
(use-package windmove
  :ensure nil
  :defer 5
  :config
  (windmove-default-keybindings))

(global-set-key
 (kbd "C-M-s")
 (defhydra hydra-splitter ()
   "Move window splitter"
   ("<left>" hydra-move-splitter-left "left")
   ("<down>" hydra-move-splitter-down "down")
   ("<up>" hydra-move-splitter-up "up")
   ("<right>" hydra-move-splitter-right "right")
   ("s" window-swap-states "swap windows" :color blue)))

(defun hydra-move-splitter-left (arg)
  "Move window splitter (ARG) left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter (ARG) right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter (ARG) up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter (ARG) down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))
