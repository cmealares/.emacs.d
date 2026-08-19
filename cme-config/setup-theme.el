;;; -*- lexical-binding: t -*-

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
  ;; default font must have a fixed height
  ;; others fonts must be relative (float)
  (set-face-attribute 'default nil :height 120 :font cme-monospaced-font)
  ;; fixed pitch face
  (set-face-attribute 'fixed-pitch nil :height 1.0 :font cme-monospaced-font) )

(defconst cme-proportional-font
  (cond
   ;; bad stars in org mode ((find-font (font-spec :name "Cantarell")) "Cantarell")
   ;; idem ((find-font (font-spec :name "Lucida Sans Unicode")) "Lucida Sans Unicode")
   (t (progn (message "Cannot find a proportional font") cme-monospaced-font) )))

(when cme-proportional-font ;;  used in org mode setup
  ;; variable pitch face
  (set-face-attribute 'variable-pitch nil :height 1.0 :font cme-proportional-font))
