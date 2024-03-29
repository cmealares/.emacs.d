;;; -----------------------------------------------------------------------
;;; ORG MODE
;;; -----------------------------------------------------------------------
(defun cme-org-mode-setup ()
  ;;(variable-pitch-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 1) )

(defun cme-org-font-setup ()
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.20)
                  (org-level-4 . 1.15)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1)))
    (set-face-attribute (car face) nil :font cme-proportional-font :weight 'regular :height (cdr face)))

  ;; Set other faces to use fixed pitch
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun cme-org-todo-setup ()
  (setq org-log-done 'time)
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
    :bind (("C-c l" . org-store-link)
           ("C-c a" . org-agenda))

    :hook (org-mode . cme-org-mode-setup)

    :custom
    ;; org-mode: Don't ruin S-arrow to switch windows
    ;; use M-+ and M-- instead
    (org-replace-disputed-keys t)

    :config
    (cme-org-font-setup)
    (cme-org-todo-setup)

    ;;(setq org-hide-leading-stars t)
    ;;(setq org-odd-levels-only t)
    (setq org-startup-folded t)
    (setq org-ellipsis " ▾")
    (setq org-hide-emphasis-markers t)
    ;;(setq org-clock-into-drawer "LOGBOOK")
    )


(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  ;;:custom   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
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
