;;; -----------------------------------------------------------------------
;;;; CC-MODE SETTINGS
;;;
;;; c-style-alist : list of available styles
;;; -----------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.java$" . java-mode))


;; To find the indentation variable that is associated to
;; a given block, move to this block and type C-c C-o
;; Auto mode can be customized thanks to the context keywords: C-c C-s

;;; hook for all c-like languages
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; automatically break long lines
            (auto-fill-mode)
            (diminish 'auto-fill-function)

            ;;
            (c-toggle-auto-newline 1)

            ;; now activated with other package
            ;;(c-toggle-auto-hungry-state 1)

            (font-lock-add-keywords
             nil
             '(("\\(!!!\\+\\|\\?\\?\\?\\+\\|CMEA?\\|TODO\\)"
                1 font-lock-warning-face prepend)))
            ))

;; This is how to optimize a regexp for multi-string search:
;(regexp-opt '("TODO" "CME" "CMEA" "???+" "!!!+" ) t)

(defun cme-rom-java-mode-hook ()
  ;; basic style
  (c-set-style "java")

  (cme-enable-tabs)

  ;; one tab equals 4 blanks
  (setq tab-width 4)
  (setq c-basic-offset tab-width)

  ;; indentation
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open 0)

  ;; tune auto mode
  (add-to-list 'c-hanging-braces-alist '(class-open after))
  (add-to-list 'c-hanging-braces-alist '(inline-open after)) )

;; hilo
(defun cme-hilo-java-mode-hook ()
  ;; basic style
  (c-set-style "java")

  (cme-disable-tabs)

  ;; one tab equals 4 blanks
  (setq tab-width 4)
  (setq c-basic-offset tab-width)

  ;; indentation
  (c-set-offset 'substatement-open 0)
;  (c-set-offset 'brace-list-open 0)
;  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open 0)

  ;; tune auto mode
  (add-to-list 'c-hanging-braces-alist '(substatement-open before)) )

(defun cme-c-mode-hook ()
  ;; basic style
  (c-set-style "gnu")

  (cme-enable-tabs)

  ;; how many blanks per tab
  (setq tab-width 4)
  (setq c-basic-offset tab-width)

  ;; indentation: tweak c-offsets-alist
  ;; gnu -> microsoft
  (c-set-offset 'innamespace 0)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open 0)

  ;; clean up: semicolon after class
  ;; (add-to-list 'c-cleanup-list 'defun-close-semi)

  ;; tune auto mode: no newline at
  (add-to-list 'c-hanging-braces-alist '(class-close before)) )


(add-hook 'c++-mode-hook 'cme-c-mode-hook)
(add-hook 'c-mode-hook   'cme-c-mode-hook)
(add-hook 'java-mode-hook 'cme-hilo-java-mode-hook)

;; company backend for c/c++
;; (use-package company-irony
;;   :config
;;   (add-to-list 'company-backends 'company-irony))

;; (use-package irony
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))


;;; -----------------------------------------------------------------------
;;;; JAVA
;;; -----------------------------------------------------------------------
(defun cme-java-describe (symbol-name)
  (interactive "MJava Class: ")
  (let ((my-string (replace-regexp-in-string
                    "^.*class-use/.*n"
                    ""
                    (shell-command-to-string
                     (concat "find C:/jdk-6u10-docs/docs/api/ -name "
                             symbol-name
                             ".html"))
                    )))

    (string-match "^.*$" my-string)

;    (with-output-to-temp-buffer "*tlog-output*"
;      (princ my-string)
;      (princ (format "match: %s" (match-string 0 my-string))))
  (browse-url (concat "file:" (match-string 0 my-string)))))

(global-set-key (kbd "C-h j") 'cme-java-describe)
