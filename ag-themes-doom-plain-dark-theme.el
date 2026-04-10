;;; ag-themes-doom-plain-dark-theme.el --- Customized theme based on doom-plain-dark -*- lexical-binding: t; -*-
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
;; Customized theme based on doom-plain-dark.
;;
;;; Code:

(require 'ag-themes)
(require 'doom-themes)

(ag-themes-deftheme ag-themes-doom-plain-dark
  "Customized theme based on doom-plain-dark."
  :base doom-plain-dark
  :modeline-height 0.85
  :faces
  (default :background (lighter 0)))

;;; ag-themes-doom-plain-dark-theme.el ends here
