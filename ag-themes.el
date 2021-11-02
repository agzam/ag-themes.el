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

(defun color-theme-set-faces (theme faces-alist)
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
  (let* ((_ (progn (defalias 'darker 'color-darken-name)
                   (defalias 'lighter 'color-lighten-name)
                   (defalias 'desaturate 'color-desaturate-name)
                   (defalias 'saturate 'color-saturate-name)))
         (resolve-face-prop (lambda (face face-prop)
                              (pcase-let* ((`(,prop-name ,prop-val) face-prop)
                                           (`(,fn ,arg1 ,arg2 ,arg3) prop-val))
                                (cond
                                 ((not (fboundp fn)) face-prop)
                                 (arg3 (list prop-name (funcall fn (face-attribute arg1 arg2) arg3)))
                                 (arg2 (list prop-name (funcall fn (face-attribute face arg1) arg2)))
                                 (arg1 (list prop-name (funcall fn (face-attribute face prop-name) arg1)))))))
         (resolve-face-props (lambda (face face-props)
                               (seq-mapcat
                                (lambda (prop)
                                  (funcall resolve-face-prop face prop))
                                (seq-partition face-props 2))))
         (reducer (lambda (acc face-spec)
                    (pcase-let ((`(,face . ,face-props) face-spec))
                      (add-to-list
                       'acc
                       (list face (list (list t (funcall resolve-face-props face face-props))))))))
         (faces (seq-reduce reducer faces-alist '())))
    (apply 'custom-theme-set-faces theme faces)))

(defun color-theme-get-faces (theme)
  "Get list of faces with their attributes of a given THEME.
If theme is not loaded, it loads it first"
  (let* ((theme (get theme 'theme-settings))
         (theme-settings (if theme theme
                           (progn
                             (load-theme theme :no-ask :no-enable)
                             (get theme 'theme-settings))))
         (extract-props (lambda (props)
                          "extracts face props based on display type"
                          (seq-reduce
                           (lambda (acc x)
                             (pcase-let ((`(((,disp-type ,disp-val)) ,face-props) x))
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


(provide 'ag-themes)
;;; ag-themes.el ends here
