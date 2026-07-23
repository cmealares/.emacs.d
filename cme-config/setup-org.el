;;; -----------------------------------------------------------------------
;;; ORG MODE
;;; -----------------------------------------------------------------------

(defun cme-org-font-setup ()
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.20)
                  (org-level-4 . 1.15)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1)))
    (set-face-attribute (car face) nil :height (cdr face)))

  ;; Set other faces to use fixed pitch
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun cme-org-todo-setup ()
  (setq org-use-fast-todo-selection t)

  ;; gtd
  ;;(setq org-todo-keywords
  ;;      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)" "SOMEDAY(s)" "|" "DONE(d!)" "REJECTED(r@)")))

  ;; scrum
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN PROGRESS(i)" "BLOCKED(b@/!)" "|" "DONE(d!)" "REJECTED(r@)")))

  (setq org-todo-keyword-faces
        '(("WAITING" . (:foreground "orange" :weight bold))
          ("BLOCKED" . (:foreground "orange" :weight bold)))) )


(use-package org
  :ensure t
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))

  :hook
  (org-mode
   . (lambda ()
       (variable-pitch-mode 1)
       (visual-line-mode 1)
       (auto-fill-mode 1)))

  :config
  ;; Don't ruin S-arrow to switch windows. Use M-+ and M-- instead
  (setq org-replace-disputed-keys t)

  ;;(setq org-hide-leading-stars t)
  (setq org-startup-folded t)
  (setq org-ellipsis " ▾")
  ;; hide *, / etc
  (setq org-hide-emphasis-markers t)

  ;; insert new headings after content
  (setq org-insert-heading-respect-content t)

  (setq org-log-done 'time)
  (setq org-clock-into-drawer t)

  (when win32-p
    (setq org-directory "~/OneDrive - SAP SE/cme_backups/projets/")
    )
  (setq org-agenda-files (list org-directory))

  (cme-org-font-setup)
  (cme-org-todo-setup))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  ;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  )


;; REMEMBER
;; see http://members.optusnet.com.au/~charles57/GTD/remember.html
;;       (when (fboundp 'org-remember-insinuate)
;;         (org-remember-insinuate)
;;         (global-set-key "\C-cr" 'org-remember)
;;
;;         (setq org-remember-templates
;;           '(("Todo" ?t "* TODO %^{Description} %^g\n%?Added: %u"
;;            "~/projets.org" "TASKS")) ))
