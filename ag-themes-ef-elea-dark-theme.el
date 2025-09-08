;;;  ag-themes-ef-elea-dark-theme.el --- Customized theme based on ef-elea-dark -*- lexical-binding: t; -*-
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
;; Customized theme based on ef-elea-dark
;;; Code:

(require 'ag-themes)
(require 'ef-themes)
(require 'ef-elea-dark-theme)

(deftheme ag-themes-ef-elea-dark "Customized theme based on ef-elea-dark.")

(let* ((faces `((default (:background (lighter 0)))

                ;; (region (:background (darker default :background 7)
                ;;          :inverse-video unspecified))

                (tab-bar (:background unspecified))
                (tab-bar-tab
                 (:background unspecified
                  :foreground (lighter default :foreground 0)
                  :weight bold
                  :box unspecified))
                (tab-bar-tab-inactive
                 (:background (lighter default :background 3)
                  :foreground (darker default :foreground 20)
                  :box unspecified))

                (org-block-begin-line
                 (:underline unspecified
                  :background (lighter default :background 3)
                  :foreground (lighter default :background 60)
                  :height 0.9
                  :weight ultra-light
                  :inherit fixed-pitch :extend t))

                (org-block (:background unspecified
                            :inherit fixed-pitch))

                (org-block-end-line
                 (:overline unspecified
                  :background (darker org-block-begin-line :background 0)
                  :foreground (darker org-block-begin-line :foreground 0)
                  :height 0.9
                  :weight ultra-light
                  :inherit fixed-pitch
                  :extend t))

                ;; (org-meta-line (:inherit fixed-pitch
                ;;                 :height 0.9
                ;;                 :weight ultra-light))

                (org-modern-indent-bracket-line
                 (:background (darker org-block-begin-line :background 3)
                  :foreground (lighter default :background 20)
                  :height 1.3
                  :inherit org-meta-line))

                ;; (org-drawer (:foreground (darker default :background 15)))

                ;; (org-special-keyword
                ;;  (:inherit (org-drawer fixed-pitch)
                ;;   :foreground (darker default :background 28)))

                ;; (org-property-value
                ;;  (:inherit (org-drawer fixed-pitch)
                ;;   :foreground (darker default :background 32)))

                (dired-directory
                 (:background unspecified))

                (dired-subtree-depth-1-face
                 (:background unspecified))
                (dired-subtree-depth-2-face
                 (:background unspecified))
                (dired-subtree-depth-3-face
                 (:background unspecified))
                (dired-subtree-depth-4-face
                 (:background unspecified))
                (dired-subtree-depth-5-face
                 (:background unspecified))
                (dired-subtree-depth-6-face
                 (:background unspecified))

                (dired-symlink
                 (:background unspecified))

                (font-lock-comment-face
                 (:background (darker default :background 5)
                  :foreground (lighter default :background 25)))
                (font-lock-comment-delimiter-face
                 (:background (darker default :background 5)
                  :foreground (lighter default :background 30)))

                (font-lock-warning-face
                 (:background unspecified))))
       (new-faces (append faces (ag-themes--modify-modeline-faces '(:height 0.85)))))
  (color-theme-set-faces 'ag-themes-ef-elea-dark
                         'ef-elea-dark
                         new-faces))

(provide-theme 'ag-themes-ef-elea-dark)

(provide 'ag-themes-ef-elea-dark-theme)
;;; ag-themes-ef-elea-dark-theme.el ends here
