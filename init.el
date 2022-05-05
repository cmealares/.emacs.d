;;; init.el --- My emacs configuration    -*- lexical-binding: t -*-

;; Author: Christophe Mealares

;;; Commentary:

;;; ----------------------------------------------------------------------
;; COMMANDS I always forget
;;; ----------------------------------------------------------------------
;;
;; emacs -Q -batch -f batch-byte-compile toto.el
;;
;;; -- MARK
;; C-<SPC> C-<SPC>      set mark
;; C-u C-<SPC>          pop mark: in same buffer
;; C-x C-<SPC>          pop global mark: can be in other buffer
;;
;;; -- Repeat command
;; C-x z z z z          repeat
;; C-x Esc Esc          repeat-complex-command.  M-p M-n move in history
;;
;; -- Edition / navigation
;; C-M-a/e              Move to beginning/end of defun
;; C-M-n/p              Move forward across one balanced group of parentheses.
;; C-M-u/d              Move backward out/forward down of one level of parentheses
;;
;; M-;                  comment-dwim
;; Esc C-h              mark-defun mark a function
;; Esc C-\\             indent-region
;; M-u / M-l / M-c      upcase-word / downcase-word / capitalize-word
;;
;;; -- Deleting
;; C-0 C-k              delete from point to beginning of line
;; S-C-<backspace>      delete entire line
;; M-z                  zap-to-char
;;
;;; -- Paragraph
;; M-m                  back-to-indentation
;; M-q                  fill-paragraph
;; C-u 35 C-x f         set fill-column to 35
;; M-t                  transpose-words
;;
;;; -- SEARCH and MATCH
;; C-s M-y              search last killed text
;; keep-lines           delete non matching lines
;; flush-lines          delete matching lines
;; count-matches        count-matches for regexp
;; highlight-regexp     C-x w h <> C-x w r
;; multi-occur          occur on multiple buffers
;; multi-occur-in-matching buffers
;;
;; C-u C-x q            Enter recursive edit in macro.  Exit recursive edit with C-M-c
;;
;;; -- REGEXP
;; re-builder           Helps to build a regexp
;;
;;; -- REGEXP tips from Steve Yegge
;;  capitalize words"     replace-regexp   \\(\\w+\\)\\(\\w\\)  ->   \\1\\,(capitalize \\2)
;;  number lines"         replace-regexp   ^\\(.+\\)        ->   \\,(1+ \\#) \\1):
;;  renumber a list"      replace-regexp   ^\\([0-9]+\\)    ->   \\,(1+ (string-to-int \\1))
;;  alphabetic list"      replace-regexp   ^\\(.+:\\)       ->   \\,(string (+ ?a \\#))
;;  open list of files"   replace-regexp   .+             ->   \\,(find-file-noselect \\&)

;;; Code:

;;; ----------------------------------------------------------------------
;;; Make startup faster by reducing the frequency of garbage collection
;;; The default is 800 kilobytes
;;; ----------------------------------------------------------------------
(setq gc-cons-threshold (* 50 1000 1000))

;;; ----------------------------------------------------------------------
;;; BETTER DEFAULTS
;;; ----------------------------------------------------------------------
;; no splash screen
(setq inhibit-startup-message t)

;; no UI
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; get rid of yes or no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; move by logical line, not by screen line
(setq line-move-visual nil)

;; do not wrap lines by words
(global-visual-line-mode -1)

;; scrolling
(setq scroll-conservatively 101)
(setq scroll-margin 2)
(setq scroll-preserve-screen-position t)

;; Allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; do not use shift to select text
(setq shift-select-mode nil)

;; flash instead of alarm bell
(setq visible-bell t)

(when (fboundp 'mouse-wheel-mode) (mouse-wheel-mode 1))

;; default width for fill-paragraph
(custom-set-variables '(fill-column 90))

;; enable upcase & down case region
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; display line/col number in modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; parenthesis
(show-paren-mode 1)
(setq blink-matching-paren t)
(electric-pair-mode 1)

;; region highlighting
(setq transient-mark-mode t)

;; Turn on font-lock in all modes that support it
(global-font-lock-mode 1)

;; Maximum colors
(setq font-lock-maximum-decoration t)

;; use ligatures when possible
(global-prettify-symbols-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (set-fringe-mode 10)
  (blink-cursor-mode 0))

;;; ----------------------------------------------------------------------
;;; LOAD PATH
;;; ----------------------------------------------------------------------
;; my configuration files
(add-to-list 'load-path (locate-user-emacs-file "cme-config"))

;; additional modules are installed here
(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))

;;; ----------------------------------------------------------------------
;;; SYSTEM DEFAULTS
;;; ----------------------------------------------------------------------
(defconst win32-p (eq system-type 'windows-nt) "Are we running on Windoze?")
(defconst linux-p (or (eq system-type 'gnu/linux) (eq system-type 'linux)) "Are we running on GNU/Linux?")

;; Load settings that are needed early
;; eg: http proxies, system paths
(let ((initos (locate-user-emacs-file "init-system.el")))
  (when (file-exists-p initos)
    (load initos)))

;;; ----------------------------------------------------------------------
;;; CUSTOMIZE - save in a dedicated file
;;; ----------------------------------------------------------------------
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;;; ----------------------------------------------------------------------
;;; BACKUP FILES in .emacs.d
;;; ----------------------------------------------------------------------
(setq backup-directory-alist
      `(("." . ,(locate-user-emacs-file "backups"))))

;;; ----------------------------------------------------------------------
;;; MARK NAVIGATION. Pop:C-u C-SPC. Global pop:C-x C-SPC
;;; repeating C-SPC with no prefix pops the next mark
;;; ----------------------------------------------------------------------
(setq set-mark-command-repeat-pop t)

;;; ----------------------------------------------------------------------
;;; PACKAGE MANAGEMENT
;; https://github.com/jwiegley/use-package
;;     :init code to run before a package is loaded. Keep minimal!
;;     :config code to run after a package is loaded
;;     :custom customized variables
;;; ----------------------------------------------------------------------
(require 'package)

;; ??? temp avoid failures for missing signature
;;(setq package-check-signature nil)

;; ??? temp workaround bug; avoid  melpa "has a running process"
(when linux-p
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t) ;; helps profile package loading

(use-package diminish)

;;; ----------------------------------------------------------------------
;;; ESUP : emacs startup profiler
;; https://blog.d46.us/advanced-emacs-startup/
;; to run: esup
;;; ----------------------------------------------------------------------
(use-package esup
  :disabled
  :config
  (setq esup-depth 0)) ; workaround bug on compiled files

;;; ----------------------------------------------------------------------
;;; ENCODING and UNICODE - use UTF-8
;; Inserting characters
;;     insert-char C-x 8 <RET>
;;     counsel-unicode-char
;;
;; Reload file with a named encoding: revert-buffer-with-coding-system
;; See the encoding of the current buffer: variable buffer-file-coding-system
;; Set coding system to save the file set-buffer-file-coding-system  C-x C-m f
;;; ----------------------------------------------------------------------
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; "C-x =" and "C-u C-x =" provide complete unicode information of a character
;; Now built-in:
;;Get info file at: http://www.unicode.org/Public/UNIDATA/UnicodeData.txt
;;(let ((x (locate-user-emacs-file "UnicodeData.txt")))
;;  (when (file-exists-p x) (setq describe-char-unicodedata-file x)))

(defun cme-decode-utf8 (l)
  "Decode list L of hexa numbers into a string."
  (interactive "xList of hexa codes. Eg:(#xC3 #xB3) : ")
  (princ
   (decode-coding-string
    (mapconcat #'byte-to-string l "")
    'utf-8)))

(defun cme-encode-to-utf8 (str)
  "Encode STR to its utf8 hexadecimal sequence."
  (interactive (list
                (read-string (format "String (%s): " (thing-at-point 'word))
                             nil nil (thing-at-point 'word))))
  (let
      ((decimal (encode-coding-string str 'utf-8)))
    (princ
     (mapcar (lambda (x) (format "%X" x))  decimal))))

;;; ----------------------------------------------------------------------
;;; THEME
;; https://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; http://emacsthemes.com/
;; What facet is used? describe-face
;; What font is used? describe-char and look at line in "display"
;; List all fonts (print (font-family-list))
;; List all loaded faces: list-faces-display
;;; ----------------------------------------------------------------------
(let ((themes-dir (locate-user-emacs-file "themes")))
  (setq custom-theme-directory themes-dir))

;; Frame properties. To display all: (prin1-to-string (frame-parameters))
(when window-system
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 85))

  ;; some free fonts:
  ;; source code pro
  ;; code new roman (monospaced)
  ;; roboto mono  (monospaced)
  ;; hack
  ;; jetbrains mono
  ;; ubuntu mono
  ;; mononoki

  (when linux-p
    (set-frame-font "DejaVu Sans Mono-11" nil t))

  (when win32-p
    (cond
     ((find-font (font-spec :name "Cascadia Code"))
      (set-frame-font "Cascadia Code-11" nil t))
     ((find-font (font-spec :name "Source Code Pro"))
      (set-frame-font "Source Code Pro-12" nil t))
     ((find-font (font-spec :name "Consolas"))
      (set-frame-font "Consolas-11" nil t))
     )))

(unless (package-installed-p 'zenburn-theme)
  (package-install 'zenburn-theme))

(unless (package-installed-p 'spacemacs-theme)
  (package-install 'spacemacs-theme))

(load-theme 'montmirail t)
;; use disable-theme to turn off

;;; ----------------------------------------------------------------------
;;; MODELINE
;; https://github.com/TheBB/spaceline
;;; ----------------------------------------------------------------------
(use-package spaceline
  :defer 1
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator 'arrow) ;'wave)
  ;;(spaceline-toggle-buffer-encoding-abbrev-on)
  ;;(spaceline-toggle-buffer-encoding-on)
  (spaceline-emacs-theme))

;;; ----------------------------------------------------------------------
;;; RAINBOW DELIMITERS
;;; ----------------------------------------------------------------------
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; ----------------------------------------------------------------------
;; RAINBOW Colorize color names in buffers
;;; ----------------------------------------------------------------------
(use-package rainbow-mode
  :commands rainbow-mode)

;;; ----------------------------------------------------------------------
;;; BEACON
;; light to follow the cursor
;;; ----------------------------------------------------------------------
(use-package beacon
  :defer 3
  :diminish
  :config
  (setq beacon-size 80)
  (setq beacon-color "#FF6600")
  (beacon-mode 1))

;; highlight current line
(when window-system (global-hl-line-mode 1))

;;; ----------------------------------------------------------------------
;;; ISPELL
;;; ----------------------------------------------------------------------
(setq ispell-program-name "aspell")
(setq ispell-dictionary "francais")

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
;;; ISEARCH - fold characters of the same kind. Des/activate with M-s '
;;; ----------------------------------------------------------------------
;; ...in other words: ignore diacritics
(when (>= emacs-major-version 25)
  (setq search-default-mode #'char-fold-to-regexp))

;;; ----------------------------------------------------------------------
;;; ENABLE EDITION OF COMPRESSED FILES
;;; ----------------------------------------------------------------------
(auto-compression-mode 1)

;;; ----------------------------------------------------------------------
;;; RECENT FILES
;;; ----------------------------------------------------------------------
(use-package recentf
  :config
  ; speedup load time for remote files that are not accessible
  (setq recentf-keep '(file-remote-p file-readable-p))
  (setq recentf-max-saved-items 200)
  (setq recentf-max-menu-items 60))

;;; ----------------------------------------------------------------------
;;; SAVE PLACE - remember last point in visited file
;;; ----------------------------------------------------------------------
(use-package saveplace
  :config
  (setq save-place-file (locate-user-emacs-file "saveplace"))
  (save-place-mode 1))

;;; ----------------------------------------------------------------------
;;; Save minibuffer history
;;; ----------------------------------------------------------------------
(setq history-length 20)
(savehist-mode 1)

;;; ----------------------------------------------------------------------
;;; UNIQUIFY - how buffer names are made unique
;;; ----------------------------------------------------------------------
(use-package emacs
  :config
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\*") ) ; do not uniquify these

;;; ----------------------------------------------------------------------
;;; MIDNIGHT - clean-buffer-list at midnight
;;; ----------------------------------------------------------------------
(use-package midnight
  :defer 3
  :config (midnight-mode 1)
  :custom
  ;; nb of days before a buffer becomes eligible for autokilling
  (clean-buffer-list-delay-general 3))

;;; ----------------------------------------------------------------------
;;; HYDRA
;; https://github.com/abo-abo/hydra
;;; ----------------------------------------------------------------------
(use-package hydra)

;; Hydra for modes that toggle on and off
(global-set-key
 (kbd "C-x t")
 (defhydra hydra-toggle (:color blue)
   "toggle"
   ("a" abbrev-mode "abbrev")
   ("s" flyspell-mode "flyspell")
   ("d" toggle-debug-on-error "debug")
   ("f" auto-fill-mode "fill")
   ("n" global-linum-mode "line number")
   ("t" toggle-truncate-lines "truncate")
   ("w" whitespace-mode "whitespace")
   ("q" nil "cancel")))

;;; ----------------------------------------------------------------------
;;; WINNER-MODE - navigate in window config with C-c right/left
;;; ----------------------------------------------------------------------
(use-package winner
  :defer 1
  :config (winner-mode 1))

;;; ----------------------------------------------------------------------
;;; WINDMOVE - navigate buffers with S-arrow
;;; ----------------------------------------------------------------------
(use-package windmove
  :defer 1
  :config
  (windmove-default-keybindings))

(defhydra hydra-splitter (global-map "C-M-s")
  "splitter"
  ("h" hydra-move-splitter-left "left")
  ("j" hydra-move-splitter-down "down")
  ("k" hydra-move-splitter-up "up")
  ("l" hydra-move-splitter-right "right")
  ("s" window-swap-states "swap windows" :color blue))

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

;;; ----------------------------------------------------------------------
;;; DIRED
;;  C-x C-q: edit dired buffer (enter wdired)
;;  j: jump to a file
;;  ^: open parent folder
;;; ----------------------------------------------------------------------
(use-package dired
  :ensure nil ; do not install because it does not exist
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :init
  (when win32-p
    ;; options for the ls emulation on windows
    (setq ls-lisp-dirs-first t)
    (setq ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))))

;;(use-package all-the-icons-dired
;;  :hook (dired-mode . all-the-icons-dired-mode))

;; visits the selected directory in the current buffer
(use-package dired-single
  :commands (dired dired-jump)
  :bind (:map dired-mode-map
              ([remap dired-find-file] . 'dired-single-buffer)
              ([remap dired-up-directory] . 'dired-single-up-directory)
              ([remap dired-mouse-find-file-other-window] . 'dired-single-buffer-mouse) ))

;;; ----------------------------------------------------------------------
;;; HUNGRY DELETE
;;; ----------------------------------------------------------------------
(use-package hungry-delete
  :defer 1
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode))

;;; ----------------------------------------------------------------------
;;; SWIPER: IVY:completion & COUNSEL:command completion & SWIPER:isearch
;; https://github.com/abo-abo/swiper
;; M-j extend the minibuffer input with the next word
;; M-r toggle regexp
;; C-M-j select current input
;; C-j ou / complete directory
;; C-c C-o ivy-occur  save the completion session to a buffer
;;; ----------------------------------------------------------------------
(use-package counsel
  :diminish
  :bind (:map minibuffer-local-map
              ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))
  ;;("<f1> f" . counsel-describe-function)
  ;;("<f1> v" . counsel-describe-variable)
  ;;("<f1> l" . counsel-find-library)
  ;;("<f2> i" . counsel-info-lookup-symbol)
  ;;("<f2> u" . counsel-unicode-char)
  ;;("C-c g" . counsel-git)
  ;;("C-c j" . counsel-git-grep)
  ;;("C-c k" . counsel-ag)
  ;;("C-x l" . counsel-locate)

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-c C-s" . isearch-forward)
         ("C-c C-r" . ivy-resume)
         ("<f3>" . ivy-resume) ))

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  ;;(setq enable-recursive-minibuffers t)
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;;; ----------------------------------------------------------------------
;;; SMEX
;; sort commands by frequency
;;; ----------------------------------------------------------------------
(use-package smex
  :config (smex-initialize))

;;; ----------------------------------------------------------------------
;;; AVY - Jump to visible char
;; https://github.com/abo-abo/avy
;;; ----------------------------------------------------------------------
;;(use-package avy
;;  :bind ("M-s" . avy-goto-char))

;;; ----------------------------------------------------------------------
;;; GREP
;;; ----------------------------------------------------------------------
(when win32-p
  ;; ms find might be in the path before cygwin's
  (setq find-program "c:/cygwin64/bin/find.exe")
  (setq grep-program "c:/cygwin64/bin/grep.exe")

  ;; since emacs23, the default did not work
  (setq grep-find-command (concat find-program
                                  " . -type f -print0 | xargs -0 -e grep -nH -e "))

  ;; use grep-find instead of rgrep which I can't get to work
  (defalias 'rgrep  'grep-find)

  ;; See https://www.emacswiki.org/emacs/NTEmacsWithCygwin
  ;; rgrep may generate find commands that use the null device.
  ;; But Emacs uses "NUL" the windows null device instead of /dev/null
  ;; this causes errors: "grep: NUL: No such file or directory"
  ;; Solution: force it to use cygwin's null device
  (setq null-device "/dev/null"))

;;; ----------------------------------------------------------------------
;;; WGREP
;; Edit a grep buffer and apply those changes to the file buffer
;; https://github.com/mhayashi1120/Emacs-wgrep
;;; ----------------------------------------------------------------------
(use-package wgrep
  :commands (wgrep-change-to-wgrep-mode))

;; Refactorings
;; - Run search with a grep command (projectile-grep)
;; - In the result buffer, to wgrep-change-to-wgrep-mode
;; - Edit and then C-x Cs-

;; It is also possible to search with a counsel-xxx (grep, git-grep, ag) command
;; then, save the current completion session to a buffer with ivy-occur C-c C-o
;; Then change to wgrep

;;; ----------------------------------------------------------------------
;;; AG - The silversearcher
;; On windows, install with cygwin
;; https://github.com/ggreer/the_silver_searcher
;;; ----------------------------------------------------------------------
(use-package ag
  :commands (ag))

;; (use-package wgrep-ag
;;   :after (wgrep ag))

;;; ----------------------------------------------------------------------
;;; WHICH-KEY shows the keybindings of entered commads
;;; ----------------------------------------------------------------------
(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config (which-key-mode))

;;; ----------------------------------------------------------------------
;;; UNDO TREE      tree: C-x u     undo: C-_   redo: M-_
;; https://gitlab.com/tsc25/undo-tree
;;; ----------------------------------------------------------------------
(use-package undo-tree
  :diminish undo-tree-mode
  :commands (undo-tree-visualize)
  :config
  (let ((undotree-dir (locate-user-emacs-file "undotree")))
    (setq undo-tree-history-directory-alist `(("." . ,undotree-dir))))
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode 1))

;;; ----------------------------------------------------------------------
;;; EDIFF DIFF MODE
;;; ----------------------------------------------------------------------
(use-package diff-mode
  :config
  (setq ediff-diff-options "-w")
  ;; do not spawn a new frame for the ediff control window
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; use a vertical layout
  (setq ediff-split-window-function 'split-window-horizontally))

;;; ----------------------------------------------------------------------
;;; SHELL MODE
;; http://www.cygwin.com/faq/faq-nochunks.html#faq.using.ntemacs
;;; ----------------------------------------------------------------------
(when win32-p
  (setq exec-path (cons "C:/cygwin/bin" exec-path))
  (setenv "PATH" (concat "C:\\cygwin\\bin;" (getenv "PATH")))

  ;;   LOGNAME and USER are expected in many Emacs packages
  ;;   Check these environment variables.

  (if (and (null (getenv "USER"))
           ;; Windows includes variable USERNAME, which is copied to
           ;; LOGNAME and USER respectively.
           (getenv "USERNAME"))
      (setenv "USER" (getenv "USERNAME")))

  (if (and (getenv "LOGNAME")
           ;;  Bash shell defines only LOGNAME
           (null (getenv "USER")))
      (setenv "USER" (getenv "LOGNAME")))

  (if (and (getenv "USER")
           (null (getenv "LOGNAME")))
      (setenv "LOGNAME" (getenv "USER")))

  (setq shell-file-name "bash")
  (setenv "SHELL" shell-file-name)
  (setq explicit-shell-file-name shell-file-name)

  ;; Remove C-m (^M) characters that appear in output
  (add-hook 'comint-output-filter-functions
            'comint-strip-ctrl-m) )

;;; ----------------------------------------------------------------------
;;; TRAMP
;;; ----------------------------------------------------------------------
(use-package tramp
  :defer 3
  :config
  (setq tramp-verbose 6)
  ;;(setq tramp-verbose 10)

  (when win32-p
    ;; PuTTY's ssh tunneling - of course they must be on the PATH
    ;; configure and save a session in putty
    ;; and then C-x C-f //plinkx:wasabi:toto.cpp
    (setq tramp-default-method "plinkx")) )

;;; ----------------------------------------------------------------------
;;; ibuffer MODE
;;; ----------------------------------------------------------------------
;; do not show empty groups
(setq ibuffer-show-empty-filter-groups nil)

;; do not ask confirmation when deleting
(setq ibuffer-expert t)

;;; ----------------------------------------------------------------------
;;; GIT
;;; ----------------------------------------------------------------------
;; https://magit.vc
(use-package magit
  :pin melpa
  :bind (("C-x g" . magit-status)
         ;;("C-x M-g" . magit-dispatch); C-c M-g: magit-file-dispatch
         )
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  ;;(setq magit-refresh-status-buffer nil) ; windows perf issues???
 )

;; https://gitlab.com/pidu/git-timemachine
(use-package git-timemachine
  :bind (("s-g" . git-timemachine)))

;; git-gutter? blamer?

;;; ----------------------------------------------------------------------
;;; PROJECTILE - project management
;; https://github.com/bbatsov/projectile
;; http://projectile.readthedocs.io/en/latest/
;; project: s-p p    file: s-p f     dir: s-p d    help: s-p C-h
;; grep: s-p s g
;; switch to file with other extension: s-p a
;; regenerate tags: s-p R    search: s-p j  see projectile-tags-command
;;; ----------------------------------------------------------------------
(use-package projectile
  :pin melpa-stable
  :bind-keymap
  (("s-p" . projectile-command-map)
   ("C-c p" . projectile-command-map))
  :bind (("<f1>" . projectile-commander))
  :config
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action  'projectile-dired)
  (projectile-mode 1))

;;; ----------------------------------------------------------------------
;;; COMPLETION - HIPPIE & ABBREVIATIONS
;;; ----------------------------------------------------------------------
;; hippie-expand is built-in
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-RET") 'hippie-expand)

(setq dabbrev-case-fold-search nil)

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand              ; yasnippet
        ;;try-expand-all-abbrevs             ; from abbrevs but does not work with skeletons
        try-expand-dabbrev                 ; from current buf
        try-expand-dabbrev-from-kill       ; from kill ring
        try-expand-dabbrev-all-buffers     ; from all bufs
        try-expand-line                    ; entire line from buf
        try-expand-whole-kill              ; from kill ring
        try-complete-file-name-partially   ; as a file name
        try-complete-file-name
        try-complete-lisp-symbol-partially ; as emacs lisp expression
        try-complete-lisp-symbol
        try-expand-list                    ; as a list
        ))

;;; ----------------------------------------------------------------------
;;; COMPANY - complete anything
;; http://company-mode.github.io/
;;; ----------------------------------------------------------------------
(use-package company
  :diminish company-mode
  :config
  ;; rewrite the backends to not use clang and cmake (which we do not use)
  (setq company-backends '((company-capf company-files company-keywords company-dabbrev-code company-etags company-dabbrev)))

  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq tab-always-indent 'complete)
  (global-company-mode 1)

  :bind (:map company-mode-map
              ;; since emacs24 the standard is completion-at-point C-M-i
              ;; but I prefer company
              ([remap completion-at-point] . 'company-complete)
              ([remap indent-for-tab-command] . 'company-indent-or-complete-common)
              ))

;;; ----------------------------------------------------------------------
;;; FLYCHECK
;; http://www.flycheck.org/en/latest/
;; C-c !
;;; ----------------------------------------------------------------------
(use-package flycheck
  :commands flycheck-mode
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :hook
  (flycheck-error-list-mode . visual-line-mode))

;;; ----------------------------------------------------------------------
;;; LSP-MODE : language server protocol
;; https://emacs-lsp.github.io/lsp-mode/
;; lsp-workspace-restart : in case of problem, restart server
;;; ----------------------------------------------------------------------
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l") ; or s-L. On windoz, s-l locks the screen
  :config
  ;; default (lsp-headerline-breadcrumb-mode 1)
  (lsp-enable-which-key-integration t))

  ;;:bind (:map lsp-mode-map
  ;;            ("<tab>" . company-indent-or-complete-common))

;;
(use-package lsp-ui
  :after lsp
  :bind (:map lsp-ui-mode-map
              ;; rebind M-. and M-? as suggested in the doc
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :config
  (setq lsp-ui-doc-position 'top)
  ;;(setq lsp-ui-doc-delay .2)
  ;; tell peek commands to show and not jump to
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t) )


(use-package treemacs
  :after lsp)

(use-package lsp-treemacs
  :commands
  (lsp-treemacs-errors-list ;; this is broken on windows
   lsp-treemacs-symbols))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;;; DAP-MODE : debug adapter protocol
;; https://www.youtube.com/watch?v=0bilcQVSlbM&list=PLEoMzSkcN8oNvsrtk_iZSb94krGRofFjN&index=2
;; https://emacs-lsp.github.io/dap-mode/
;; Cmmands:
;;    https://emacs-lsp.github.io/dap-mode/page/features/
;;    dap-debug (-last)
;;    dap-breakpoint-toggle
;;    dap-breakpoint-log-message
;;    dap-disconnect : stop debugging
;;    dap-hydra
;;    dap-ui-repl
(use-package dap-mode
  :disabled
  :commands (dap-debug dap-hydra)
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra)) )

;;; ----------------------------------------------------------------------
;;; COMPILATION
;;; ----------------------------------------------------------------------
(setq compilation-window-height 8)

;; (defadvice compile (around around-compile)
;;   "Save and restore window config around compilation"
;;   (cme-save-window-config)
;;   ad-do-it)

;; (ad-activate 'compile)

(setq compilation-finish-function
   (lambda (buf str)
     (if (equal (buffer-name buf) "*compilation*")
         (if (string-match "exited abnormally" str)
             ;;there were errors
             (progn
               (setq my-compile-error t)
               (message "compilation errors, press C-x ` to visit"))

           ;;no errors, make the compilation window go away in 0.5 seconds
           ;;(run-at-time 0.5 nil 'delete-windows-on buf)
           (run-at-time 0.5 nil 'winner-undo)
           (message "COMPILATION SUCCESSFUL !")))))

;(add-hook 'gdb-mode-hook
;         (lambda ()
;           (enlarge-window
;            (- (/ (frame-height (selected-frame)) 3)
;               (window-height (selected-window))))))


;(setq special-display-buffer-names
;      (append special-display-buffer-names '(("*compilation*" (width . 80) (height . 20) (unsplittable . t)))))


(setq gdb-many-windows t)

;;; ----------------------------------------------------------------------
;;; YASNIPPET - Code templates
;; TAB: expansion; yas-describe-table; yas-insert-snippet
;; http://joaotavora.github.io/yasnippet/
;;; ----------------------------------------------------------------------
(use-package yasnippet
  :diminish yas-minor-mode
  :hook
  ;; has no effect because the scratch buffer is in prog mode
  (prog-mode . yas-minor-mode) )

(use-package yasnippet-snippets
  :after (yasnippet) )

;; we can also activate it for some modes only. eg:
;; (add-hook 'c++-mode-hook 'yas-minor-mode)

;;; ----------------------------------------------------------------------
;;; LOAD OTHER CONFIG FILES
;;; ----------------------------------------------------------------------
(load "setup-misc-functions.el")
(load "setup-browse.el")
(load "setup-org.el")
(load "setup-tagging.el")
(load "setup-cc.el")
(load "setup-json.el")
(load "setup-javascript.el")
(load "setup-lisp.el")
;;(load "setup-clojure.el")
;;(load "setup-haskell.el")
;;(load "setup-python.el")

;; (load "sap-misc.el")
;; (load "sap-browse.el")

;;; ----------------------------------------------------------------------
;;; START SERVER
;;; ----------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;; ----------------------------------------------------------------------
;;; TIE SOME FILE EXTENSIONS TO MODES
;;; ----------------------------------------------------------------------
(setq auto-mode-alist
      (append
       '(("\\.gmk$"         . makefile-mode)
         ("\\.mak$"         . makefile-mode))
       auto-mode-alist))

;;; ----------------------------------------------------------------------
;;; KEY BINDINGS
;; M-x describe-bindings to view all bindings
;;; ----------------------------------------------------------------------

;; C-x @ h adds the hyper flag to the next character, C-x @ s adds the super
(when win32-p
  ;; bind menu key to hyper H-
  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper)

  ;; bind windows keys to super s-
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)

  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super)

  (w32-register-hot-key [s-])
  (w32-register-hot-key [h-]))

(global-set-key (kbd "<escape>")      'keyboard-escape-quit)
(global-set-key (kbd "C-M-SPC")       'cycle-spacing)

(when (fboundp 'imenu)
  (global-set-key [mouse-3]           'imenu))

(global-set-key (kbd "C-S-<f1>")      'find-name-dired)

;; f3 is kmacro-start-macro-or-insert-counter'

(global-set-key (kbd "<f7>")          'recompile)
(global-set-key (kbd "C-<f7>")        'compile)
(global-set-key (kbd "C-S-<f7>")      'kill-compilation)

(global-set-key (kbd "<f8>")          'other-frame)
(global-set-key (kbd "M-<f8>")        'cme-pretty-xml)

(global-set-key (kbd "C-x p")         'proced)

;;; ----------------------------------------------------------------------
;;; ALIAS DEFINITIONS
;;; ----------------------------------------------------------------------
(defalias 'bb    'bury-buffer)
(defalias 'ra    'cme-revert-all-buffers)
(defalias 'rb    '(lambda () (interactive) (revert-buffer t t)))

(defalias 'ff    'find-name-dired)
(defalias 'gf    'grep-find)
(defalias 'fgd   'find-grep-dired)
(defalias 'gfd   'find-grep-dired)
(defalias 'fd    'find-dired)

(defalias 'eb    'ediff-buffers)

(defalias 'ffap  'find-file-at-point)

;;; ----------------------------------------------------------------------
;;; Make gc pauses faster by decreasing the threshold.
;;; ----------------------------------------------------------------------
(setq gc-cons-threshold (* 2 1000 1000))
