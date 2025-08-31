;;;  ag-themes-ef-elea-light-theme.el --- Customized theme based on ef-elea-light -*- lexical-binding: t; -*-
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/ag-themes.el
;; Created: Nov-2021
;; Keywords: faces
;; License: GPL v3
;; Package-Requires: ((emacs "30") (ef-themes "1"))
;; Version: 1.0.0
;;
;;; Commentary:
;;
;;  Description
;; Customized theme based on ef-elea-light
;;; Code:

(require 'ag-themes)
(require 'ef-themes)
(require 'ef-elea-light-theme)

(deftheme ag-themes-ef-elea-light "Customized theme based on ef-elea-light.")

(let* ((faces `((default (:background (lighter 0)))

                (region (:background (darker default :background 7)
                         :inverse-video unspecified))

                (tab-bar (:background (lighter default :background 3)))
                (tab-bar-tab
                 (:background (lighter default :background 20) :foreground (lighter default :foreground 0)
                  :weight bold
                  :box unspecified))
                (tab-bar-tab-inactive
                 (:background (lighter default :background 3)
                  :foreground (darker default :foreground 20)
                  :box unspecified))

                (org-block-begin-line
                 (:underline unspecified
                  :background (darker default :background 3)
                  :foreground (darker default :background 60)
                  :height 0.9
                  :weight ultra-light
                  :inherit fixed-pitch :extend t))

                (org-block (:background unspecified
                            :inherit fixed-pitch))

                (org-block-end-line
                 (:overline unspecified
                  :background (darker default :background 3)
                  :foreground (darker default :background 60)
                  :height 0.9
                  :weight ultra-light
                  :inherit fixed-pitch
                  :extend t))

                (org-meta-line (:inherit fixed-pitch
                                :height 0.9
                                :weight ultra-light))

                (org-modern-indent-bracket-line
                 (:background (darker default :background 3)
                  :foreground (darker default :background 20)
                  :height 1.3
                  :inherit org-meta-line))

                (org-drawer (:foreground (darker default :background 15)))

                (org-special-keyword
                 (:inherit (org-drawer fixed-pitch)
                  :foreground (darker default :background 28)))

                (org-property-value
                 (:inherit (org-drawer fixed-pitch)
                  :foreground (darker default :background 32)))

                (magit-diff-hunk-heading-highlight
                 (:background unspecified))
                (magit-section-highlight
                 (:background unspecified))
                (magit-diff-context-highlight
                 (:background unspecified))))
       (new-faces (append faces (ag-themes--modify-modeline-faces '(:height 0.85)))))
  (color-theme-set-faces 'ag-themes-ef-elea-light
                         'ef-elea-light
                         new-faces))

(provide-theme 'ag-themes-ef-elea-light)

(provide 'ag-themes-ef-elea-light-theme)
;;; ag-themes-ef-elea-light-theme.el ends here
