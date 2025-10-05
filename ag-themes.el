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
;; Package-Requires: ((emacs "29"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun color-get-face-colors (face)
  "Return main FACE colors as a list of hex values."
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
  "Sort given list of COLORS from the lightest shade to the darkest."
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
  "Return all the main colors used in the loaded theme.
Sorted by color intensity (from lighter to darker)"
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
                                   (and (= r1 r2) (= g1 g2) (= b1 b2))))
                               colors)
                          face)))
                    faces+colors)))
    (seq-map 'car filtered)))

(require 'color)

(defun plist-merge (&rest plists)
  "Merge multiple PLISTS, handling both plists and lists containing plists."
  (when plists
    (let ((result (car plists)))
      ;; Unwrap the first plist if needed
      (when (and (listp result) (listp (car result)) (keywordp (caar result)))
        (setq result (car result)))
      (setq result (copy-sequence result))
      
      (dolist (plist (cdr plists))
        ;; Unwrap if needed
        (when (and (listp plist) (listp (car plist)) (keywordp (caar plist)))
          (setq plist (car plist)))
        (while plist
          (setq result (plist-put result (car plist) (cadr plist))
                plist (cddr plist))))
      result)))

(defun ag-themes--merge-face-specs (face &rest face-lists)
  "Merges properties from multiple FACE-LISTS into single FACE."
  (apply
   'plist-merge
   (seq-map
    'cadr
    (seq-filter
     (lambda (x) (eq (car x) face))
     (apply 'append face-lists)))))

(defun color-theme-set-faces (theme base-theme faces-alist)
  "Modified version of `custom-theme-set-faces'.

Allows relative (darker, lighter, etc.) changes to existing faces
in the BASE-THEME. FACES-ALIST is a list of modified faces and
their properties. Applied modifications get reflected in a new
THEME.

Examples:
\\(color-theme-set-faces \\'new-zenburn \\'zenburn
  \\`(
    ;; set foreground to green and use background color of the
    ;; same face (default of zenburn theme) but make it darker by 10%
    (default (:foreground \"green\" :background (darker 10)))

    ;; take foreground color of mode-line face
    ;; and use it for :backround, but make it 20% lighter
    (mode-line (:background (lighter :foreground 20)))))

    ;; use background color of org-level-1 face,
    ;; make it a bit darker and use that for org-level-2 foreground
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
                         (`(,fn ,arg1 ,arg2 ,arg3) (when (listp prop-val) prop-val))
                         (prev-props
                          (ag-themes--merge-face-specs
                           face
                           base-faces
                           faces-so-far)))
              (cond
               ((eq fn 'quote) face-prop)
               ((not (fboundp fn)) face-prop) ; fn set, but unknown

               ;; ignore inheriting multiple faces
               ((and (eq prop-name :inherit)
                     (listp prop-val))
                face-prop)

               ;; fn called for another face and property
               (arg3 (when-let* ((other-props (ag-themes--merge-face-specs
                                               arg1 base-faces faces-so-far))
                                 (prop-val (plist-get other-props arg2)))
                       (list prop-name (format "%s" (funcall fn prop-val arg3)))))
               ;; fn called for the same face and property
               (arg2 (when-let* ((prop-val (plist-get prev-props arg2)))
                       (list prop-name (format "%s" (funcall fn prop-val arg2)))))
               ;; fn called for the same property
               (arg1 (when-let* ((prop-val (plist-get prev-props prop-name)))
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
  "Get list of faces with their attributes of a given THEME."
  (let* ((theme-settings (or (get theme 'theme-settings)
                             (progn
                               (load-theme theme :no-ask :no-enable)
                               (get theme 'theme-settings))))
         (extract-props (lambda (props)
                          "extracts face props based on display type"
                          ;; Check if we have extra wrapping
                          (let ((props (if (and (= (length props) 1)
                                                (listp (car props))
                                                (listp (caar props))
                                                (not (eq (caar props) t)))  ; don't unwrap ((t ...))
                                           (car props)  ; unwrap ef-themes style
                                         props)))
                            (seq-reduce
                             (lambda (acc x)
                               (if acc acc
                                 (cond
                                  ;; ((t :inherit shadow))
                                  ((and (listp x) (eq (car x) t))
                                   (cdr x))
                                  ;; (((conditions) :prop val ...)) - ef-themes style
                                  ((and (listp x) (listp (car x)) (keywordp (cadr x)))
                                   (cdr x))
                                  ;; (((conditions) (:prop val ...))) - base16 style
                                  ((and (listp x) (listp (car x)) (listp (cadr x)))
                                   (cadr x))
                                  (t nil))))
                             props nil)))))
    (seq-remove
     'null
     (seq-map
      (lambda (x)
        (when (and (listp x)
                   (eq (car x) 'theme-face))
          (let ((face (cadr x))
                (props-raw (cdddr x)))
            (when props-raw
              (list face (funcall extract-props props-raw))))))
      theme-settings))))

(defun ag-themes--modify-modeline-faces (face-attrs)
  "Set modeline faces with `FACE-ATTRS'."
  (let ((faces '(
                 ;; mode-line
                 mode-line-buffer-id
                 mode-line-emphasis
                 mode-line-highlight
                 ;; mode-line-inactive

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

(provide 'ag-themes)
;;; ag-themes.el ends here
