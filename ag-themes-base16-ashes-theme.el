;;; ag-themes-base16-ashes-theme.el --- Customized theme based on base16-ashes -*- lexical-binding: t; -*-
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
;; Customized theme based on base16-ashes.
;;
;;; Code:

(require 'ag-themes)

(ag-themes-deftheme ag-themes-base16-ashes
  "Customized theme based on base16-ashes."
  :base base16-ashes
  :modeline-height 0.85
  :palette ((bg-accent . "#3a4a4f"))
  :faces
  (show-paren-match :background unspecified :foreground "orange"
   magit-diff-file-heading-highlight :background bg-accent
   tab-bar :background (lighter 0 default :background)
   tab-bar-tab :background (darker 10 default :background)
               :foreground (darker 20 default :foreground)
               :weight bold :box unspecified
   tab-bar-tab-inactive :background (lighter 0 default :background)
                        :foreground (darker 35 default :foreground)
                        :box unspecified
   vertico-current :background bg-accent))

;;; ag-themes-base16-ashes-theme.el ends here
