;;; -----------------------------------------------------------------------
;;;; ORG MODE
;;; -----------------------------------------------------------------------

(use-package org
    :mode "\\.org\\$"

    :bind (("C-c l" . org-store-link)
           ("C-c a" . org-agenda))

    :hook (org-mode . visual-line-mode)

    :custom
    ;; org-mode: Don't ruin S-arrow to switch windows
    ;; use M-+ and M-- instead
    (org-replace-disputed-keys t)

    :config
    (when (fboundp 'org-bullets-mode)
      (org-bullets-mode t))

    (setq org-hide-leading-stars t)
    (setq org-odd-levels-only t)
    (setq org-startup-folded t)
    (set-face-foreground 'org-hide cme-background-color)

    ;; todo
    (setq org-log-done 'time)
    (setq org-use-fast-todo-selection t)
    ;;(setq org-clock-into-drawer "LOGBOOK")

    ;; gtd
    ;;(setq org-todo-keywords
    ;;      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)" "SOMEDAY(s)" "|" "DONE(d!)" "REJECTED(r@)")))
    ;; scrum
    (setq org-todo-keywords
          '((sequence "TODO(t)" "IN PROGRESS(i)" "BLOCKED(b@/!)" "|" "DONE(d!)" "REJECTED(r@)")))

    (setq org-todo-keyword-faces
          '(("WAITING" . (:foreground "orange" :weight bold))
            ("BLOCKED" . (:foreground "orange" :weight bold))))

;; agenda seems slow... and I do not use it
;;    ;; what org files to put in agenda view
;;    (setq org-agenda-files '("~/projets.org"))
;;
;;    ;; custom agenda views
;;     (setq org-agenda-custom-commands
;;           '(
;;             (" " "Sprint tasks and agenda"
;;              ((agenda ""
;;                       ((org-agenda-overriding-header "blabla")
;;                        (org-agenda-ndays 30)
;;                        (org-agenda-skip-timestamp-if-done nil)
;;                        (org-agenda-show-all-dates nil)))
;;               (tags-todo "CATEGORY=\"task\""
;;                          ((org-agenda-overriding-header "Active sprint tasks")))
;;               (tags "CATEGORY=\"task\"/REJECTED"
;;                     ((org-agenda-overriding-header "Cancelled sprint tasks")))))
;;
;;             ("h" "Tasks that were closed this sprint"
;;              tags "CATEGORY=\"task\"+CLOSED>=\"[2011-09-01 Thu]\"+CLOSED<=\"[2011-09-30 Fri]\"") ))
;;
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
