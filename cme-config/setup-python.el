;;; -----------------------------------------------------------------------
;;; PYTHON
;;; -----------------------------------------------------------------------
;;;(use-package python-mode
;;;(use-package 'pytest)


;;; PYLOOKUP : browse documentation; http://taesoo.org/Opensource/Pylookup
;; - Install doc: sudo aptitude install python2.6-doc
;; - Dload pylookup: mv pylookup ~/.emacs.d/site-lisp
;; - Index database: ./pylookup.py -u file:/usr/share/doc/python2.6-doc/html
(defun cme-pylookup-setup ()
  "Configure access to documentation with pylookup"

 (let ((pylookup-dir (expand-file-name "pylookup"  cme-site-lisp-dir)))

   (when (file-exists-p pylookup-dir)
     (add-to-list 'load-path pylookup-dir)

     (require 'pylookup)

     ;; set executable file and db file
     (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
     (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

     ;; to speedup, just load it on demand
     (autoload 'pylookup-lookup "pylookup"
       "Lookup SEARCH-TERM in the Python HTML indexes." t)
     (autoload 'pylookup-update "pylookup"
       "Run pylookup-update and create the database at `pylookup-db-file'." t)

     (define-key python-mode-map "\C-ch" 'pylookup-lookup))))


;;; PYMACS & ROPEMACS
;;; Extends emacs with python
;;(defun cme-pymacs-ropemacs-setup ()
;;  "Activate pymacs and ropemacs"
;;
;;  (when (require 'pymacs nil 'noerror)
;;
;;    ;; do not overwrite std key bindings
;;    (setq ropemacs-enable-shortcuts nil)
;;    ;; set prefix for refactoring key bindings
;;    (setq ropemacs-local-prefix "C-c C-p")
;;
;;    (pymacs-load "ropemacs" "rope-" 'noerror)
;;    (setq ropemacs-enable-autoimport t)))


(add-hook 'python-mode-hook
          (lambda ()
            ;; bind RET to newline and indent
            (define-key python-mode-map "\C-m" 'newline-and-indent)

            (cme-disable-tabs)

            ;; one tab equals 4 blanks
            (setq tab-width 4)

            (define-key python-mode-map (kbd "C-c C-,") 'pytest-run-file)

            (cme-pylookup-setup)
            ;;(cme-pymacs-ropemacs-setup)
            ))
