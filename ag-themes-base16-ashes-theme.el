;;;  --- Customized theme based on base16-ashes -*- lexical-binding: t; -*-
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/ag-themes.el
;; Created: Nov-2021
;; Keywords: faces
;; License: GPL v3
;; Package-Requires: ((emacs "27"))
;; Version: 1.0.0
;;
;;; Commentary:
;;
;;  Description
;; Customized theme based on base16-ashes
;;; Code:

(require 'ag-themes)
(require 'base16-ashes-theme)

(deftheme ag-themes-base16-ashes "Customized theme based on base16-ashes.")

(let* ((bg-accent "#3a4a4f")
       (faces `((show-paren-match (:background unspecified :foreground "orange"))
                (magit-diff-file-heading-highlight (:background ,bg-accent))
                (tab-bar (:background (lighter default :background 0)))
                (tab-bar-tab (:background (darker default :background 10)
                              :foreground (darker default :foreground 20)
                              :weight bold :box unspecified))
                (tab-bar-tab-inactive (:background (lighter default :background 0)
                                       :foreground (darker default :foreground 35)
                                       :box unspecified))

                (vertico-current (:background ,bg-accent))))
       (new-faces (append faces (ag-themes--modify-modeline-faces '(:height 0.85)))))
  (color-theme-set-faces 'ag-themes-base16-ashes
                         'base16-ashes
                         new-faces))

(provide-theme 'ag-themes-base16-ashes)

(provide 'ag-themes-base16-ashes-theme)
;;; ag-themes-base16-ashes-theme.el ends here
