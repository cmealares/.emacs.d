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
;; C-x r <SPC>          store mark into register; j to jump
;; C-x r m              set bookmark; b to jump
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
(setq fill-column 90)

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
;;; ----------------------------------------------------------------------
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(use-package use-package
  :demand t
  :ensure nil
  :init
  ;; helps profile package loading
  (setq use-package-verbose t))

(use-package diminish
  :ensure t)

;;; ----------------------------------------------------------------------
;;; ESUP : emacs startup profiler
;; https://blog.d46.us/advanced-emacs-startup/
;; to run: esup
;;; ----------------------------------------------------------------------
(use-package esup
  :disabled
  :ensure t
  :config
  (setq esup-depth 0)) ; workaround bug on compiled files

;;; ----------------------------------------------------------------------
;;; THEME
;; https://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; http://emacsthemes.com/
;;; ----------------------------------------------------------------------
(let ((themes-dir (locate-user-emacs-file "themes")))
  (setq custom-theme-directory themes-dir))

(unless (package-installed-p 'zenburn-theme)
  (package-install 'zenburn-theme))

(unless (package-installed-p 'spacemacs-theme)
  (package-install 'spacemacs-theme))

(unless (package-installed-p 'ef-themes)
  (package-install 'ef-themes))

(load-theme 'montmirail t)
;; use disable-theme to turn off

;;(use-package ef-themes
;;  :ensure nil
;;  :config
;;  (ef-themes-load-theme 'ef-orange))

;;; ----------------------------------------------------------------------
;;; THE FONT
;; What facet is used? describe-face
;; What font is used? describe-char and look at line in "display"
;; Frame properties. To display all: (prin1-to-string (frame-parameters))
;; List all fonts (print (font-family-list))
;; List all loaded faces: list-faces-display
;; Show all attributes of a face (print (face-all-attributes 'default))
;;; ----------------------------------------------------------------------
(when window-system
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 90))

;; Some free fonts:
;;    Monospaced
;;       firacode
;;       cascadia code
;;       source code pro
;;       code new roman
;;       roboto mono
;;       jetbrains mono
;;       ubuntu mono
;;       mononoki
;;       iosevka
;;    Variable width:
;;       cantarell

(defconst cme-monospaced-font
  (cond
   ((find-font (font-spec :name "Fira Code")) "Fira Code")
   ((find-font (font-spec :name "Cascadia Code")) "Cascadia Code")
   ((find-font (font-spec :name "Source Code Pro")) "Source Code Pro")
   ((find-font (font-spec :name "Consolas")) "Consolas")
   ((find-font (font-spec :name "DejaVu Sans Mono")) "DejaVu Sans Mono")
   (t (progn (message "Cannot find a monospaced font") nil) )))

(when cme-monospaced-font
  ;; default must have a fixed height, the others must be relative (float)
  (set-face-attribute 'default nil :height 120 :font cme-monospaced-font)
  ;; fixed pitch face
  (set-face-attribute 'fixed-pitch nil :height 1.0 :font cme-monospaced-font) )

(defconst cme-proportional-font
  (cond
   ((find-font (font-spec :name "Cantarell")) "Cantarell")
   ((find-font (font-spec :name "Lucida Sans Unicode")) "Lucida Sans Unicode")
   (t (progn (message "Cannot find a proportional font") cme-monospaced-font) )))

(when cme-proportional-font ;;  used in org mode setup
  ;; variable pitch face
  (set-face-attribute 'variable-pitch nil :height 1.0 :font cme-proportional-font)))


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
;;; DELSEL
;; Delete the selected text as sool as the user types something
;;; ----------------------------------------------------------------------
(use-package delsel
  :ensure nil ; it is built-in
  :hook (after-init . delete-selection-mode))

;;; ----------------------------------------------------------------------
;;; MODELINE
;; https://github.com/TheBB/spaceline
;;; ----------------------------------------------------------------------
(use-package spaceline
  :ensure t
  ;;:disabled
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
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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
;;; ISEARCH
;;; ----------------------------------------------------------------------

;; fold characters of the same kind... in other words: ignore diacritics
;; Des/activate with M-s '
(setq search-default-mode #'char-fold-to-regexp)

;; Interpret the empty space as a regular expression that matches any
;; character between the words
(setq search-whitespace-regexp ".*?")

;; Display a counter before the prompt
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)

;;; ----------------------------------------------------------------------
;;; ENABLE EDITION OF COMPRESSED FILES
;;; ----------------------------------------------------------------------
(auto-compression-mode 1)

;;; ----------------------------------------------------------------------
;;; RECENT FILES
;;; ----------------------------------------------------------------------
(use-package recentf
  :ensure t
  :config
  ; speedup load time for remote files that are not accessible
  (setq recentf-keep '(file-remote-p file-readable-p))
  (setq recentf-max-saved-items 200)
  (setq recentf-max-menu-items 60)
  (recentf-mode 1))

;;; ----------------------------------------------------------------------
;;; SAVE PLACE - remember last point in visited file
;;; ----------------------------------------------------------------------
(use-package saveplace
  :ensure t
  :config
  (setq save-place-file (locate-user-emacs-file "saveplace"))
  (save-place-mode 1))

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
;;; HYDRA
;; https://github.com/abo-abo/hydra
;;; ----------------------------------------------------------------------
(use-package hydra
  :ensure t)

;; Hydra for modes that toggle on and off
(global-set-key
 (kbd "C-x t")
 (defhydra hydra-toggle (:color blue)
   "toggle"
   ("a" abbrev-mode "abbrev")
   ;;("s" flyspell-mode "flyspell")
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

;;; ----------------------------------------------------------------------
;;; HUNGRY DELETE
;;; ----------------------------------------------------------------------
(use-package hungry-delete
  :ensure t
  :defer 1
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode))

;;; ----------------------------------------------------------------------
;;; THE MINIBUFFER
;;; ----------------------------------------------------------------------
;; Save history
(setq history-length 20)
(savehist-mode 1)

;; Vertical layout for the minibuffer
;; https://github.com/minad/vertico/wiki
(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1))

;; Show more info in the minibuffer
;; https://github.com/minad/marginalia
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

;; Better completion algorithm
;; https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles partial-completion)))))

;; Search and navigation
;; https://github.com/minad/consult#use-package-example
(use-package consult
  :ensure t
  :config
  ;; delay preview because it is slow when modes are not yet initialized
  (setq consult-preview-key (list :debounce 0.4 'any))
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;;("C-x M-#" . consult-register)
         ("C-x r j" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  )

;; Minibuffer actions. Think right-click
;; https://github.com/oantolin/embark
;; https://karthinks.com/software/fifteen-ways-to-use-embark/
;; embark-act
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; glue embark and consult
(use-package embark-consult
  :ensure t)

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
  :ensure t
  :commands (wgrep-change-to-wgrep-mode))

;; Refactorings
;; - Run search with a grep command (projectile-grep)
;; - In the result buffer, to wgrep-change-to-wgrep-mode
;; - Edit and then C-x C-s / C-c C-k

;; It is possible to search with a counsel-xxx (grep, git-grep, ag) command
;; then, save the current completion session to a buffer with ivy-occur C-c C-o
;; Then change to wgrep

;;; ----------------------------------------------------------------------
;;; AG - The silversearcher
;; On windows, install with cygwin
;; https://github.com/ggreer/the_silver_searcher
;;; ----------------------------------------------------------------------
(use-package ag
  :ensure t
  :commands (ag))

;; (use-package wgrep-ag
;;   :after (wgrep ag))

;;; ----------------------------------------------------------------------
;;; WHICH-KEY shows the keybindings of entered commads
;;; ----------------------------------------------------------------------
(use-package which-key
  :ensure t
  :defer 1
  :diminish which-key-mode
  :config (which-key-mode))

;;; ----------------------------------------------------------------------
;;; UNDO TREE      tree: C-x u     undo: C-_   redo: M-_
;; https://gitlab.com/tsc25/undo-tree
;;; ----------------------------------------------------------------------
(use-package undo-tree
  :ensure t
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
  :ensure nil
  :config
  (setq ediff-diff-options "-w")
  ;; do not spawn a new frame for the ediff control window
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; use a vertical layout
  (setq ediff-split-window-function 'split-window-horizontally)
  ;; show ancestor
  (setq ediff-merge-revisions-with-ancestor t))

;;; ----------------------------------------------------------------------
;;; SMERGE
;;; smerge-ediff command prefix is C-c ^
;;; ----------------------------------------------------------------------
(use-package smerge-mode
  :ensure t
  :commands smerge-mode
  :init
  ;;(setq smerge-command-prefix (kbd "C-c s"))
  )

(global-set-key
 (kbd "C-c s")
 (defhydra hydra-smerge (:color red :hint nil :pre (smerge-mode 1))
   "
Navigate       Keep               other
----------------------------------------
_p_: previous  _c_: current       _e_: ediff
_n_: next      _m_: mine  <<      _u_: undo
_j_: up        _o_: other >>      _r_: refine
_k_: down      _a_: all           _q_: quit
               _b_: base
"
   ("n" smerge-next)
   ("p" smerge-prev)
   ("c" smerge-keep-current)
   ("m" smerge-keep-mine)
   ("o" smerge-keep-other)
   ("b" smerge-keep-base)
   ("a" smerge-keep-all)
   ("e" smerge-ediff)
   ("j" previous-line)
   ("k" forward-line)
   ("r" smerge-refine)
   ("u" undo)
   ("q" nil :exit t)))

;;; ----------------------------------------------------------------------
;;; SHELL MODE
;; http://www.cygwin.com/faq/faq-nochunks.html#faq.using.ntemacs
;;; ----------------------------------------------------------------------
;;(when win32-p
;;  (setq exec-path (cons "C:/cygwin/bin" exec-path))
;;  (setenv "PATH" (concat "C:\\cygwin\\bin;" (getenv "PATH")))
;;
;;  ;;   LOGNAME and USER are expected in many Emacs packages
;;  ;;   Check these environment variables.
;;
;;  (if (and (null (getenv "USER"))
;;           ;; Windows includes variable USERNAME, which is copied to
;;           ;; LOGNAME and USER respectively.
;;           (getenv "USERNAME"))
;;      (setenv "USER" (getenv "USERNAME")))
;;
;;  (if (and (getenv "LOGNAME")
;;           ;;  Bash shell defines only LOGNAME
;;           (null (getenv "USER")))
;;      (setenv "USER" (getenv "LOGNAME")))
;;
;;  (if (and (getenv "USER")
;;           (null (getenv "LOGNAME")))
;;      (setenv "LOGNAME" (getenv "USER")))
;;
;;  (setq shell-file-name "bash")
;;  (setenv "SHELL" shell-file-name)
;;  (setq explicit-shell-file-name shell-file-name)
;;
;;  ;; Remove C-m (^M) characters that appear in output
;;  (add-hook 'comint-output-filter-functions
;;            'comint-strip-ctrl-m) )

;;; ----------------------------------------------------------------------
;;; TRAMP
;;; ----------------------------------------------------------------------
(use-package tramp
  :ensure nil
  :defer t
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
  :ensure t
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
  :ensure t
  :bind (("s-g" . git-timemachine)))

;; git-gutter? blamer?

;;; ----------------------------------------------------------------------
;;; PROJECTILE - project management
;; https://github.com/bbatsov/projectile
;; http://projectile.readthedocs.io/en/latest/
;; project: s-p p    file: s-p f     dir: s-p d    help: s-p C-h
;; projectile-invalidate-cache
;; grep: s-p s g
;; switch to file with other extension: s-p a
;; regenerate tags: s-p R    search: s-p j  see projectile-tags-command
;;; ----------------------------------------------------------------------
(use-package projectile
  :ensure t
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
;;; COMPLETIONS
;;; ----------------------------------------------------------------------
(setq tab-always-indent 'complete) ; tab to do completion

;; CORFU: in buffer completion popup
;; https://github.com/minad/corfu
(use-package corfu
  :ensure t
  :hook
  (prog-mode . (lambda () (setq-local corfu-auto t)))
  :config
  (setq corfu-min-width 30)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; show documentation

  (global-corfu-mode 1))

;; CAPE: more capfs
;; https://github.com/minad/cape
(use-package cape
  :ensure t
  :after corfu
  :config
  (setq completion-at-point-functions '(cape-dabbrev cape-file)))

;;; ----------------------------------------------------------------------
;;; FLYCHECK
;; http://www.flycheck.org/en/latest/
;; C-c !
;;; ----------------------------------------------------------------------
(use-package flycheck
  :ensure t
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
  :ensure t
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
  :ensure t
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
  :ensure t
  :after lsp)

(use-package lsp-treemacs
  :ensure t
  :commands
  (lsp-treemacs-errors-list ;; this is broken on windows
   lsp-treemacs-symbols))

(use-package lsp-ivy
  :ensure t
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
  :ensure t
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
  :ensure t
  :diminish yas-minor-mode
  :hook
  ;; has no effect because the scratch buffer is in prog mode
  (prog-mode . yas-minor-mode) )

(use-package yasnippet-snippets
  :ensure t
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
(load "sap-browse.el")

;;; ----------------------------------------------------------------------
;;; START SERVER
;;; ----------------------------------------------------------------------
(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

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
(defalias 'rb    #'(lambda () (interactive) (revert-buffer t t)))

(defalias 'ff    'find-name-dired)
(defalias 'gf    'grep-find)
(defalias 'fgd   'find-grep-dired)
(defalias 'gfd   'find-grep-dired)
(defalias 'fd    'find-dired)
(defalias 'is    'isearch-forward)

(defalias 'eb    'ediff-buffers)

(defalias 'ffap  'find-file-at-point)

;;; ----------------------------------------------------------------------
;;; Make gc pauses faster by decreasing the threshold.
;;; ----------------------------------------------------------------------
(setq gc-cons-threshold (* 2 1000 1000))
