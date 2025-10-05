;;;  ag-themes-doom-feather-light-theme.el --- Customized theme based on doom-feather-light -*- lexical-binding: t; -*-
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
;; Customized theme based on doom-feather-light
;;; Code:

(require 'ag-themes)
(require 'doom-themes)
(require 'doom-feather-light-theme)

(deftheme ag-themes-doom-feather-light "Customized theme based on doom-feather-light.")

(let* ((faces `((default (:background (lighter 0)))))
       (new-faces (append faces (ag-themes--modify-modeline-faces '(:height 0.85)))))
  (color-theme-set-faces 'ag-themes-doom-feather-light
                         'doom-feather-light
                         new-faces))

(provide-theme 'ag-themes-doom-feather-light)

(provide 'ag-themes-doom-feather-light-theme)
;;; ag-themes-doom-feather-light-theme.el ends here
