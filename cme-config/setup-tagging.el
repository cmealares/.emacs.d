;;; -----------------------------------------------------------------------
;;;; GGTAGS : a frontend to GNU GLOBAL
;; http://www.gnu.org/software/global : needs to be on path
;; Can also use exuberant ctags as backend
;; https://github.com/leoliu/ggtags
;; M-.    jump to definition
;; M-,    pop mark
;; C-M-.  find
;; !! loading ggtags modifies M-. Use M-, to abort navigation
;;; -----------------------------------------------------------------------
(use-package ggtags
  :diminish ggtags-mode
  :bind (:map ggtags-mode-map
              ;;("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g f" . ggtags-find-file)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags)
              ("M-," . pop-tag-mark)
  )
  :init
  (diminish 'eldoc-mode)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

;;a essayer (setq-local imenu-create-index-function #'ggtags-build-imenu-index)

;;; -----------------------------------------------------------------------
;;;; TAGGING WITH Exuberant ctags from http://ctags.sourceforge.net/
;;; -----------------------------------------------------------------------
(defmacro cme-make-tagger (command)
  "Make a function that will use the given command to generate a TAGS  file in a directory"
  `(lambda (dir)
     (interactive "DCreate TAGS in directory: ")
     (cme-tag-and-visit dir ,command)))

(defsubst cme-tag-and-visit (dir command)
  "Use the given command to generate a TAGS file in the given directory"
  (message "Generating tags in %s..." dir)

  (let  ((olddir default-directory))
    (cd dir)
    (shell-command  command)
    (visit-tags-table dir)
    (cd olddir)
    (message "Tags loaded")) )

(defalias 'tag-python (cme-make-tagger "ctags -e -R --languages=Python")
  "Generate a TAGS file for Python.")

(defalias 'tag-java (cme-make-tagger "ctags -e -R --languages=Java")
  "Generate a TAGS file for Java.")

(defalias 'tag-cpp (cme-make-tagger "ctags -e -R --languages=C++ -Iinterface=class")
  "Generate a TAGS file for C++.")

(defun tag-language (dir)
  "Generates a TAGS file in a directory dir."
  (interactive "DCreate TAGS in directory: ")

     (cme-tag-and-visit
      dir
      (concat "ctags -e -R --languages="
              (ido-completing-read "Language: " (cme-ctags-list-languages) nil t))))

(defun cme-ctags-list-languages ()
  (split-string
   (shell-command-to-string "ctags --list-languages")))
