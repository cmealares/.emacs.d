;;; -----------------------------------------------------------------------
;;;; HASKELL
;;; https://github.com/haskell/haskell-mode
;;; https://github.com/haskell/haskell-mode/wiki
;;; -----------------------------------------------------------------------

;;  I am not yet into haskell...
;;
;;   (require-package 'haskell-mode)
;;
;;   ;; setup interactive mode
;;   (require 'haskell-interactive-mode)
;;   (require 'haskell-process)
;;   (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;;
;;   ;; indentation
;;   (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;
;;   (after-load 'haskell-mode
;;
;;     (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
;;       (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
;;       (add-to-list 'exec-path my-cabal-path))
;;
;;     ;; process type
;;     (setq haskell-process-type 'ghci)
;;     ; use cabal-repl to work in sandboxed projects
;;     ;(setq haskell-process-type 'cabal-repl)
;;
;;
;;     ;; autoformatting : cabal install stylish-haskell
;;     (setq haskell-stylish-on-save t)
;;
;;     ;; tagging : cabal install hasktags
;;     (setq haskell-tags-on-save t)
;;     (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
;;     ;;(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
;;
;;     (setq haskell-process-suggest-remove-import-lines t)
;;     (setq haskell-process-auto-import-loaded-modules t)
;;     (setq haskell-process-log t)
;;
;;     ;; searches
;;     (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
;;
;;
;;     ;; haskell-mode bindings
;;     (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
;;
;;     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;;     (define-key haskell-mode-map (kbd "C-c C-'") 'haskell-interactive-bring)
;;     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;;     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;;     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;     (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;     (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
;;     (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
;;
;;     ;; cabal-mode bindings
;;     (define-key haskell-cabal-mode-map (kbd "C-c C-'") 'haskell-interactive-bring)
;;     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal) )
