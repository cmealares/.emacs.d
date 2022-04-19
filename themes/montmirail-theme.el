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
      (montmirail-fg-1        "#000000")
      (montmirail-fg-05       "#040710")
      (montmirail-fg          "#0b132b")
      (montmirail-fg+1        "#152451")
      (montmirail-fg+2        "#1d3272")
      (montmirail-bg-2        "#ffad33")
      (montmirail-bg-1        "#ffbe5c")
      (montmirail-bg-08       "#ffce85")
      (montmirail-bg-05       "#ffdead")
      (montmirail-bg          "#ffebcd")
      (montmirail-bg+05       "#ff00ff")
      (montmirail-bg+1        "#fff7eb")
      (montmirail-bg+2        "#ffffff")
      (montmirail-bg+3        "#ff00ff")

      (montmirail-orange      "#ff7f50") ; coral

      )

  (custom-theme-set-faces
   'montmirail
   ;; basic
   `(default ((,class :background ,montmirail-bg :foreground ,montmirail-fg)))
   `(cursor ((,class :background ,montmirail-orange)))
   `(fringe ((t (:foreground ,montmirail-fg :background ,montmirail-bg+1))))
))

(provide-theme 'montmirail)
;;; montmirail-theme.el ends here
