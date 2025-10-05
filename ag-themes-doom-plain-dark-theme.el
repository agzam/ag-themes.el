;;;  ag-themes-doom-plain-dark-theme.el --- Customized theme based on doom-plain-dark -*- lexical-binding: t; -*-
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
;;  Description
;; Customized theme based on doom-plain-dark
;;; Code:

(require 'ag-themes)
(require 'doom-themes)
(require 'doom-plain-dark-theme)

(deftheme ag-themes-doom-plain-dark "Customized theme based on doom-plain-dark.")

(let* ((faces `((default (:background (lighter 0)))))
       (new-faces (append faces (ag-themes--modify-modeline-faces '(:height 0.85)))))
  (color-theme-set-faces 'ag-themes-doom-plain-dark
                         'doom-plain-dark
                         new-faces))

(provide-theme 'ag-themes-doom-plain-dark)

(provide 'ag-themes-doom-plain-dark-theme)
;;; ag-themes-doom-plain-dark-theme.el ends here
