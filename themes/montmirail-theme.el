;;; montmirail-theme.el --- A color theme for Emacs.

;; Copyright (C) 2022 Christophe Mealares

;; Author: Christophe Mealares <cmea84@gmail.com>
;; URL:
;; Version:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU Gener&al Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Based on the default theme of Emacs.

;;; Code:

(deftheme montmirail "The Montmirail color theme.")

(let ((class '((class color) (min-colors 89)))
      (montmirail-fg          "#0b132b")
      (montmirail-fg+1        "#152451")
      (montmirail-fg+2        "#1d3272")
      (montmirail-fg+3        "#264192")
      (montmirail-fg+4        "#2e4fb2")
      (montmirail-fg+5        "#3c60cd")
      (montmirail-bg-5        "#ff9d0a")
      (montmirail-bg-4        "#ffad33")
      (montmirail-bg-3        "#ffbe5c")
      (montmirail-bg-2        "#ffce85")
      (montmirail-bg-1        "#ffdead")
      (montmirail-bg          "#ffebcd")
      (montmirail-bg+1        "#fff7eb")

      (montmirail-red-6       "#520000")
      (montmirail-red-5       "#660000")
      (montmirail-red-4       "#7a0000")
      (montmirail-red-3       "#8f0000")
      (montmirail-red-2       "#a30000")
      (montmirail-red-1       "#b80000")
      (montmirail-red         "#d00000")firebrick
      (montmirail-red+1       "#ff0a0a")
      (montmirail-red+2       "#ff3333")
      (montmirail-orange      "#ff7f50")
      (montmirail-yellow      "#ffe066")
      (montmirail-green-5     "#143e1a")
      (montmirail-green-4     "#1e5c26")
      (montmirail-green-3     "#287b33")
      (montmirail-green-2     "#2d8b39")
      (montmirail-green-1     "#37a946")
      (montmirail-green       "#3cbb4d")
      (montmirail-green+1     "#56c865")
      (montmirail-green+2     "#74d281")
      (montmirail-green+3     "#93dc9d")
      (montmirail-green+4     "#b2e6b9")
      (montmirail-cyan        "#1c7293")
      (montmirail-blue        "#61bdf2")
      (montmirail-violet      "#9448bc")
      (montmirail-brown       "#7a4900")
      (montmirail-pink        "#ff4f79")
      )

  ;; fg #0b132b
  ;; #63b4d1 #7699d4 #9448bc  #480355 (russian violet)
  ;; #e63462 #fe5f55 #c7efcf
  ;; #0b6e4f "#fa9f42"
  ;; #7f95d1
  ;; #7692ff #de3c4b
  ;; #a11692 #ff4f79
  ;; #f25f5c #ffe066 #247ba0 #70c1b3 #89ce94 #7d5ba6
  ;; #a30000 #ff7700 #efd28d #00afb5
  ;; #1c7293 #9eb3c2 #ff212e #f4d06f


  (custom-theme-set-faces
   'montmirail
   ;; basic
   `(default ((,class :background ,montmirail-bg :foreground ,montmirail-fg)))
   `(fringe ((,class (:foreground ,montmirail-fg :background ,montmirail-bg+1))))
   `(cursor ((,class :background ,montmirail-orange)))

   `(highlight ((,class (:background ,montmirail-green+4))))
   `(success ((,class (:foreground ,montmirail-green :weight bold))))
   `(warning ((,class (:foreground ,montmirail-orange :weight bold))))
   `(tooltip ((,class (:foreground ,montmirail-fg :background ,montmirail-bg+1))))

   `(menu ((,class (:foreground ,montmirail-fg :background ,montmirail-bg))))
   `(minibuffer-prompt ((,class (:foreground ,montmirail-fg+4))))

   `(region ((,class (:background ,montmirail-bg-2 :extend t))))
   `(secondary-selection ((,class (:background ,montmirail-bg+1))))
   `(trailing-whitespace ((,class (:background ,montmirail-violet))))
   `(vertical-border ((,class (:foreground ,montmirail-fg))))

   ;; isearch
   `(isearch ((,class (:foreground ,montmirail-fg :weight bold :background ,montmirail-bg-3))))
   `(isearch-fail ((,class (:foreground ,montmirail-fg :background ,montmirail-violet))))
   `(lazy-highlight ((,class (:foreground ,montmirail-fg :weight bold :background ,montmirail-bg-1))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,montmirail-fg+3)))) ; "dark slate blue"
   `(font-lock-comment-face ((,class (:foreground ,montmirail-red))))
   `(font-lock-constant-face ((,class (:foreground ,montmirail-cyan)))); "dark cyan"
   `(font-lock-function-name-face ((,class (:foreground ,montmirail-fg+5))))
   `(font-lock-keyword-face ((,class (:foreground ,montmirail-violet :weight bold)))) ; "purple"
   `(font-lock-string-face ((,class (:foreground ,montmirail-red-3)))); "VioletRed4"
   `(font-lock-type-face ((,class (:foreground ,montmirail-green :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,montmirail-brown)))) ; "sienna"
   `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   ))

(provide-theme 'montmirail)
;;; montmirail-theme.el ends here
