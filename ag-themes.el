;;; ag-themes.el --- Customized color themes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ag Ibragimomv
;;
;; Author: Ag Ibragimomv <https://github.com/agzam/ag-themes.el>
;; Maintainer: Ag Ibragimomv <agzam.ibragimov@gmail.com>
;; Created: November 02, 2021
;; Version: 0.0.1
;; Keywords: faces
;; Homepage: https://github.com/agzam/ag-themes.el
;; Package-Requires: ((emacs "27"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun color-get-face-colors (face)
  "Returns main FACE colors as a list of hex values"
  (delete-dups
   (seq-remove
    'null
    (seq-reduce
     (lambda (acc color)
       (if (eq 'unspecified color) acc
         (let ((hex (apply 'color-rgb-to-hex
                           (append (color-name-to-rgb color) '(2)))))
           (append acc (list hex)))))
     (list (face-attribute face :foreground)
           (face-attribute face :background))
     '()))))

(defun color-sort-colors (colors)
  "Sort given list of colors from the lightest shade to the darkest"
  (seq-map
   (lambda (x) (car x))
   (sort
    (seq-map
     (lambda (c)
       (cons c (last (apply 'color-rgb-to-hsl (color-name-to-rgb c)))))
     colors)
    (lambda (x y)
      (< (cadr y) (cadr x))))))

(defun color-get-theme-palette ()
  "Return all the main colors used in the loaded theme, sorted by
color intensity (from lighter to darker)"
  (color-sort-colors
   (delete-dups
    (seq-mapcat 'color-get-face-colors (face-list)))))

(defun color-find-faces-of-color (color)
  "Return all the faces of the loaded theme that are using COLOR."
  (let* ((faces+colors (seq-mapcat
                        (lambda (face)
                          (when-let ((colors (color-get-face-colors face)))
                            (list (list face colors))))
                        (face-list)))
         (filtered (seq-filter
                    (lambda (face+colors)
                      (pcase-let ((`(,face ,colors) face+colors))
                        (when (seq-some
                               (lambda (clr)
                                 (pcase-let* ((conv (lambda (c)
                                                      (seq-map
                                                       (lambda (n)
                                                         (string-to-number (format "%f" n)))
                                                       (color-name-to-rgb c))))
                                              (`(,r1 ,g1 ,b1) (funcall conv color))
                                              (`(,r2 ,g2 ,b2) (funcall conv clr)))
                                   (and (= r1 r2) (= g1 g2) (= b2 b2))))
                               colors)
                          face)))
                    faces+colors)))
    (seq-map 'car filtered)))

(require 'color)

(defun plist-merge (&rest plists)
  (if plists
      (let ((result (copy-sequence (car plists))))
        (while (setq plists (cdr plists))
          (let ((plist (car plists)))
            (while plist
              (setq result (plist-put result (car plist) (car (cdr plist)))
                    plist (cdr (cdr plist))))))
        result)
    nil))


(defun ag-themes--merge-face-specs (face &rest face-lists)
  (apply
   'plist-merge
   (seq-map
    'cadr
    (seq-filter
     (lambda (x) (eq (car x) face))
     (apply 'append face-lists)))))

(defun color-theme-set-faces (theme base-theme faces-alist)
  "Modified version of `custom-theme-set-faces' that additionally
allows to re-use existing faces by directly applying
modificications to them. Theme changes applied immediately.

Examples:
(color-theme-set-faces 'zenburn
  `(
    ;; set foreground to green and use background color of the
    ;; same face (default of zenburn theme) but make it darker by 10%
    (default (:foreground \"green\" :background (darker 10)))

    ;; take foreground color of mode-line face
    ;; and use it for :backround, but make it 20% lighter
    (mode-line (:background (lighter :foreground 20)))))

    ;; use background color of org-level-1 face,
    ;; make it a bit darker and use that for org-level-2's foreground
    (org-level-2 (:foreground (darker org-level-1 :background 15)))

    ;; desaturate org-level-3 face background by 10%
    (org-level-3 (:background (color-desaturate-name 10)))))"

  (fset 'darker 'color-darken-name)
  (fset 'lighter 'color-lighten-name)
  (fset 'desaturate 'color-desaturate-name)
  (fset 'saturate 'color-saturate-name)
  (let* ((base-faces (color-theme-get-faces base-theme))

         (resolve-face-prop
          (lambda (face face-prop faces-so-far)
            (pcase-let* ((`(,prop-name ,prop-val) face-prop)
                         (`(,fn ,arg1 ,arg2 ,arg3) prop-val)
                         (prev-props
                          (ag-themes--merge-face-specs
                           face
                           base-faces
                           faces-so-far)))
              (cond
               ((eq fn 'quote) face-prop)
               ((not (fboundp fn)) face-prop) ; fn set, but uknown
               ;; fn called for another face and propert
               (arg3 (when-let ((other-props (ag-themes--merge-face-specs
                                              arg1 base-faces faces-so-far))
                                (prop-val (plist-get other-props arg2)))
                       (list prop-name (format "%s" (funcall fn prop-val arg3)))))
               ;; fn called for the same face and property
               (arg2 (when-let ((prop-val (plist-get prev-props arg2)))
                       (list prop-name (format "%s" (funcall fn prop-val arg2)))))
               ;; fn called for the same property
               (arg1 (when-let ((prop-val (plist-get prev-props prop-name)))
                       (list prop-name (format "%s" (funcall fn prop-val arg1)))))))))

         (resolve-face-props
          (lambda (face face-props faces-so-far)
            (seq-mapcat
             (lambda (prop)
               (funcall resolve-face-prop
                        face prop faces-so-far))
             (seq-partition face-props 2))))

         (reducer
          (lambda (acc face-spec)
            (pcase-let*
                ((`(,face ,face-props) face-spec)
                 (prev-props
                  (seq-map 'cadr
                           (seq-filter
                            (lambda (x) (eq (car x) face))
                            (append base-faces acc))))

                 (new-props
                  (apply 'plist-merge
                         (append
                          prev-props
                          (list (funcall resolve-face-props
                                         face
                                         face-props
                                         acc)))))
                 ;; overwrite any previous face definition
                 (acc (seq-remove (lambda (x) (eq (car x) face)) acc))
                 (new-el `(,face ,new-props)))
              (push new-el acc))))
         (faces (seq-reduce reducer (append
                                     base-faces
                                     faces-alist) '()))
         (new-faces (seq-map
                     (lambda (x)
                       `(,(car x) ((t ,@(cdr x)))))
                     faces)))
    (apply 'custom-theme-set-faces theme new-faces)))

(defun color-theme-get-faces (theme)
  "Get list of faces with their attributes of a given THEME.
If theme is not loaded, it loads it first"
  (let* ((theme-settings (or (get theme 'theme-settings)
                             (progn
                               (load-theme theme :no-ask :no-enable)
                               (get theme 'theme-settings))))
         (extract-props (lambda (props)
                          "extracts face props based on display type"
                          (seq-reduce
                           (lambda (acc x)
                             (pcase-let* ((`(((,disp-type ,disp-val)) . ,face-props) x)
                                          (face-props (cond
                                                       ((listp (car face-props)) (car face-props))
                                                       ((listp face-props) face-props))))
                               (if acc acc
                                 ;; prioritize graphic & color displays
                                 (cond ((eq disp-val 'graphic) face-props)
                                       ((eq disp-val 'color) face-props)
                                       ((eq disp-type 'min-colors) face-props)
                                       (t face-props)))))
                           props nil))))
    (seq-remove
     'null
     (seq-map
      (lambda (x)
        (pcase-let* ((`(,prop-type ,face _ . (,props)) x))
          (when (eq prop-type 'theme-face)
            (list face (funcall extract-props props)))))
      theme-settings))))

(defun ag-themes--modify-modeline-faces (face-attrs)
  (let ((faces '(mode-line
                 mode-line-buffer-id
                 mode-line-emphasis
                 mode-line-highlight
                 mode-line-inactive

                 persp-face-lighter-default
                 persp-face-lighter-nil-persp
                 persp-face-lighter-buffer-not-in-persp

                 eyebrowse-mode-line-active
                 eyebrowse-mode-line-inactive
                 eyebrowse-mode-line-separator
                 eyebrowse-mode-line-delimiters

                 doom-modeline-buffer-timemachine
                 doom-modeline-battery-error
                 doom-modeline-battery-critical
                 doom-modeline-battery-warning
                 doom-modeline-battery-normal
                 doom-modeline-battery-full
                 doom-modeline-battery-charging
                 doom-modeline-lsp-running
                 doom-modeline-lsp-error
                 doom-modeline-lsp-warning
                 doom-modeline-lsp-success
                 doom-modeline-repl-warning
                 doom-modeline-repl-success
                 doom-modeline-persp-buffer-not-in-persp
                 doom-modeline-persp-name
                 doom-modeline-evil-replace-state
                 doom-modeline-evil-visual-state
                 doom-modeline-evil-operator-state
                 doom-modeline-evil-normal-state
                 doom-modeline-evil-motion-state
                 doom-modeline-evil-insert-state
                 doom-modeline-evil-emacs-state
                 doom-modeline-debug-visual
                 doom-modeline-bar-inactive
                 doom-modeline-bar
                 doom-modeline-unread-number
                 doom-modeline-notification
                 doom-modeline-urgent
                 doom-modeline-warning
                 doom-modeline-info
                 doom-modeline-debug
                 doom-modeline-input-method-alt
                 doom-modeline-input-method
                 doom-modeline-host
                 doom-modeline-panel
                 doom-modeline-highlight
                 doom-modeline-project-root-dir
                 doom-modeline-project-dir
                 doom-modeline-project-parent-dir
                 doom-modeline-buffer-minor-mode
                 doom-modeline-buffer-major-mode
                 doom-modeline-buffer-modified
                 doom-modeline-buffer-file
                 doom-modeline-buffer-path
                 doom-modeline-vspc-face
                 doom-modeline-spc-face)))
    (seq-map (lambda (x) `(,x ,face-attrs)) faces)))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;### (autoloads nil "ag-themes-base16-ocean-theme" "ag-themes-base16-ocean-theme.el"
;;;;;;  (0 0 0 0))

(provide 'ag-themes)
;;; ag-themes.el ends here
