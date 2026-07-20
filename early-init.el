;;; -*- lexical-binding: t -*-

;;; -----------------------------------------------------------------------
(setq package-enable-at-startup nil)

;; no menus
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
