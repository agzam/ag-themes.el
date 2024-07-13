;;;  --- Customized theme based on base16-tokyo-night-light -*- lexical-binding: t; -*-
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
;; Customized theme based on base16-tokyo-night-light
;;; Code:

(require 'ag-themes)
(require 'base16-tokyo-night-light-theme)

(deftheme ag-themes-base16-tokyo-night-light "Customized theme based on base16-tokyo-night-light.")

(let* ((base00 "#d5d6db")
       (base01 "#cbccd1")
       (base02 "#dfe0e5")
       (base03 "#9699a3")
       (base04 "#4c505e")
       (base05 "#343b59")
       (base06 "#1a1b26")
       (base07 "#1a1b26")
       (base08 "#343b58")
       (base09 "#965027")
       (base0A "#166775")
       (base0B "#485e30")
       (base0C "#3e6968")
       (base0D "#34548a")
       (base0E "#5a4a78")
       (base0F "#8c4351")
       (faces `((default (:background (lighter 0)))
                (show-paren-match (:background unspecified :foreground (lighter cursor :background 0) :underline t))
                (dired-directory (:background (lighter default :background 1.5)))
                (dired-subtree-depth-1-face (:background (lighter dired-directory :background 5)))
                (dired-subtree-depth-2-face (:background (lighter dired-directory :background 8)))
                (dired-subtree-depth-3-face (:background (lighter dired-directory :background 12)))
                (dired-subtree-depth-4-face (:background (lighter dired-directory :background 15)))
                (dired-subtree-depth-5-face (:background (lighter dired-directory :background 20)))
                (dired-subtree-depth-6-face (:background (lighter dired-directory :background 30)))
                (aw-leading-char-face (:height 5.0))))
       (new-faces (append faces (ag-themes--modify-modeline-faces '(:height 0.85)))))
  (color-theme-set-faces 'ag-themes-base16-tokyo-night-light
                         'base16-tokyo-night-light
                         new-faces))

(provide-theme 'ag-themes-base16-tokyo-night-light)

(provide 'ag-themes-base16-tokyo-night-light-theme)
;;; ag-themes-base16-tokyo-night-light-theme.el ends here
