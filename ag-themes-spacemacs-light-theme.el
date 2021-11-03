;;; ag-themes-spacemacs-light-theme.el --- Customized theme based on spacemacs-light -*- lexical-binding: t; -*-
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/ag-themes.el
;; Created: Nov-2021
;; Keywords: color theme
;; License: GPL v3
;; Package-Requires: ((emacs "27"))
;; Version: 1.0.0

(require 'ag-themes)
(require 'spacemacs-light-theme)

(deftheme ag-themes-spacemacs-light "Customized theme based on spacemacs-light")

(let* ((base-faces (color-theme-get-faces 'spacemacs-light))
       (alter `((default (:background (lighter 1)))
                )))
  (dolist (face (append alter (ag-themes--modify-modeline-faces '(:height 0.8))))
    (add-to-list 'base-faces face :append))
  (color-theme-set-faces
   'ag-themes-spacemacs-light base-faces))

(provide-theme 'ag-themes-spacemacs-light)

(provide 'ag-themes-spacemacs-light-theme)
