;;;  --- Customized theme based on ef-elea-light -*- lexical-binding: t; -*-
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
;; Customized theme based on ef-elea-light-theme
;;; Code:

(require 'ag-themes)
(require 'ef-elea-light-theme)

(deftheme ag-themes-ef-elea-light "Customized theme based on ef-elea-light.")

(let* ((faces `(
                ;; (default (:background (lighter 0)))
                ;; (show-paren-match (:background unspecified :foreground (lighter cursor :background 0) :underline t))
                ;; (dired-directory (:background (lighter default :background 1.5)))
                ;; (dired-subtree-depth-1-face (:background (lighter dired-directory :background 5)))
                ;; (dired-subtree-depth-2-face (:background (lighter dired-directory :background 8)))
                ;; (dired-subtree-depth-3-face (:background (lighter dired-directory :background 12)))
                ;; (dired-subtree-depth-4-face (:background (lighter dired-directory :background 15)))
                ;; (dired-subtree-depth-5-face (:background (lighter dired-directory :background 20)))
                ;; (dired-subtree-depth-6-face (:background (lighter dired-directory :background 30)))
                ;; (aw-leading-char-face (:height 5.0))
                ;; (org-meta-line (:foreground "#b6b9c4"))
                ;; (org-block-begin-line (:extend t))
                ;; (org-block-end-line (:extend t :inherit org-block-begin-line))
                ;; (org-modern-indent-bracket-line (:background "#cbccd1" :height 1.3 :inherit org-meta-line))
                ;; (lsp-face-highlight-textual (:background ,base04))
                ))
       (new-faces (append faces (ag-themes--modify-modeline-faces '(:height 0.85)))))
  (color-theme-set-faces 'ag-themes-ef-elea-light
                         'ef-elea-light-theme
                         new-faces))

(provide-theme 'ag-themes-ef-elea-light)

(provide 'ag-themes-ef-elea-light-theme)
;;; ag-themes-ag-themes-ef-elea-light-theme.el ends here
