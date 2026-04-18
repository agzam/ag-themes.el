;;; ag-themes-doom-feather-light-theme.el --- Customized theme based on doom-feather-light -*- lexical-binding: t; -*-
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/ag-themes.el
;; Created: Nov-2021
;; Keywords: faces
;; License: GPL v3
;; Package-Requires: ((emacs "30") (doom-themes "20250924"))
;; Version: 1.0.0
;;
;;; Commentary:
;;
;; Customized theme based on doom-feather-light.
;;
;;; Code:

(require 'ag-themes)
(require 'doom-themes)

(ag-themes-deftheme ag-themes-doom-feather-light
  "Customized theme based on doom-feather-light."
  :base doom-feather-light
  :modeline-height 0.85
  :faces
  (default :background (darker 3)
   tab-bar-tab :background (darker 10 default :background)
   tab-bar-tab-inactive :foreground (lighter 15)))

;;; ag-themes-doom-feather-light-theme.el ends here
