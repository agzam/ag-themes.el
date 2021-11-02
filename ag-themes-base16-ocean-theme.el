;;; ag-themes-base16-ocean-theme.el --- Customized theme based on base16-ocean -*- lexical-binding: t; -*-
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/ag-themes.el
;; Created: Nov-2021
;; Keywords: color theme
;; License: GPL v3
;; Package-Requires: ((emacs "27"))
;; Version: 1.0.0

(require 'ag-themes)
(require 'base16-ocean-theme)

(deftheme ag-themes-base16-ocean "Customized theme based on base16-ocean")

(color-theme-set-faces
 'ag-themes-base16-ocean
 (color-theme-get-faces 'base16-ocean))

(provide-theme 'ag-themes-base16-ocean)

(provide 'ag-themes-base16-ocean-theme)
