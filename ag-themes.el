;;; ag-themes.el --- Customized color themes with face override DSL -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ag Ibragimov
;;
;; Author: Ag Ibragimov <https://github.com/agzam/ag-themes.el>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: November 02, 2021
;; Version: 0.0.2
;; Keywords: faces
;; Homepage: https://github.com/agzam/ag-themes.el
;; Package-Requires: ((emacs "29"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Define derivative Emacs themes by overlaying face overrides on a base theme.
;; Color transforms let you express relative changes rather than hardcoding hex
;; values.
;;
;; Usage:
;;
;;   (ag-themes-deftheme ag-themes-my-theme
;;     "My custom overrides."
;;     :base doom-one
;;     :modeline-height 0.85
;;     :palette ((bg-alt . "#1e1e2e"))
;;     :faces
;;     (default :background bg-alt
;;      mode-line :background (darker 10 default :foreground)
;;      org-block :background (lighter 5)))
;;
;; Transform syntax - (FN AMOUNT [SOURCE-FACE] [SOURCE-PROP]):
;;
;;   (darker 10)                     - same face, same property
;;   (lighter 10 :background)        - same face, use :background as source
;;   (darker 30 default :foreground) - use default face's :foreground as source
;;
;;; Code:

(require 'color)
(require 'seq)

;;; --- Transform dispatch ---

(defconst ag-themes--transforms
  '((darker     . color-darken-name)
    (lighter    . color-lighten-name)
    (saturate   . color-saturate-name)
    (desaturate . color-desaturate-name))
  "Map of DSL transform symbols to color manipulation functions.")

;;; --- Utilities ---

(defun ag-themes--plist-merge (&rest plists)
  "Merge PLISTS left-to-right; later values win."
  (let ((result (copy-sequence (or (car plists) '()))))
    (dolist (plist (cdr plists))
      (let ((p plist))
        (while p
          (setq result (plist-put result (car p) (cadr p))
                p (cddr p)))))
    result))

(defun ag-themes--substitute-palette (form palette)
  "Walk FORM replacing symbols found in PALETTE alist with their values.
Palette symbols must not clash with transform names (darker, lighter,
saturate, desaturate)."
  (cond
   ((and (symbolp form) (assq form palette))
    (cdr (assq form palette)))
   ((consp form)
    (cons (ag-themes--substitute-palette (car form) palette)
          (ag-themes--substitute-palette (cdr form) palette)))
   (t form)))

(defun ag-themes--parse-flat-faces (flat-list)
  "Parse FLAT-LIST into alist of (FACE PLIST).
Non-keyword symbols start new faces; keywords begin property pairs."
  (let ((items flat-list)
        result current-face current-props)
    (while items
      (let ((item (pop items)))
        (cond
         ((keywordp item)
          (push item current-props)
          (push (pop items) current-props))
         ((symbolp item)
          (when current-face
            (push (list current-face (nreverse current-props)) result))
          (setq current-face item
                current-props nil)))))
    (when current-face
      (push (list current-face (nreverse current-props)) result))
    (nreverse result)))

;;; --- Base theme face extraction ---

(defun ag-themes--extract-face-attrs (spec)
  "Extract attribute plist from a theme face SPEC.
Handles standard, ef-themes, and base16 wrapping conventions."
  (when (consp spec)
    (let ((chosen (ignore-errors (face-spec-choose spec))))
      (cond
       ;; Standard: (:prop val ...)
       ((keywordp (car-safe chosen))
        chosen)
       ;; Wrapped: ((:prop val ...))
       ((keywordp (car-safe (car-safe chosen)))
        (car chosen))
       ;; Fallback for formats face-spec-choose can't handle
       (t
        (let ((levels (if (and (consp (car-safe spec))
                               (consp (car-safe (car-safe spec)))
                               (not (eq (car-safe (car-safe spec)) t)))
                          (list spec (car spec))
                        (list spec))))
          (seq-some
           (lambda (level)
             (when (consp level)
               (seq-some
                (lambda (item)
                  (cond
                   ((keywordp (car-safe item)) item)
                   ((keywordp (car-safe (car-safe item))) (car item))))
                level)))
           levels)))))))

(defun ag-themes--base-theme-faces (theme)
  "Extract face attribute alist from THEME's registered settings."
  (unless (memq theme custom-known-themes)
    (load-theme theme :no-ask :no-enable))
  (let (result)
    (dolist (setting (get theme 'theme-settings))
      (when (eq (car setting) 'theme-face)
        (let* ((face (nth 1 setting))
               (spec (nth 3 setting))
               (attrs (ag-themes--extract-face-attrs spec)))
          (when attrs
            (push (list face attrs) result)))))
    (nreverse result)))

;;; --- Transform resolution ---

(defun ag-themes--get-face-prop (face prop resolved)
  "Look up PROP of FACE in the RESOLVED alist."
  (when-let* ((entry (assq face resolved)))
    (plist-get (cadr entry) prop)))

(defun ag-themes--resolve-value (value prop face resolved)
  "Resolve VALUE which may be a transform expression.
PROP is the target property, FACE the target face, RESOLVED the
accumulated face alist.  Returns the resolved value, or nil when
a transform's source is missing."
  (if (or (not (consp value)) (eq (car value) 'quote))
      value
    (if-let* ((fn (alist-get (car value) ag-themes--transforms)))
        (pcase (cdr value)
          ;; (FN AMOUNT) - same face, same property
          (`(,(and amount (pred numberp)))
           (if-let* ((src (ag-themes--get-face-prop face prop resolved)))
               (funcall fn src amount)
             (message "ag-themes: %s %s not found for %s" (car value) prop face)
             nil))

          ;; (FN AMOUNT :src-prop) - same face, different property
          (`(,(and amount (pred numberp)) ,(and src-prop (pred keywordp)))
           (if-let* ((src (ag-themes--get-face-prop face src-prop resolved)))
               (funcall fn src amount)
             (message "ag-themes: %s %s not found for %s" (car value) src-prop face)
             nil))

          ;; (FN AMOUNT src-face :src-prop) - different face and property
          (`(,(and amount (pred numberp))
             ,(and src-face (pred symbolp))
             ,(and src-prop (pred keywordp)))
           (if-let* ((src (ag-themes--get-face-prop src-face src-prop resolved)))
               (funcall fn src amount)
             (message "ag-themes: %s %s of %s not found"
                      (car value) src-prop src-face)
             nil))

          ;; Args don't match any transform shape - return as literal
          (_ value))
      ;; Not a known transform symbol - return as literal
      value)))

(defun ag-themes--resolve-props (face props resolved)
  "Resolve all property values in PROPS plist for FACE.
Keeps nil values only when explicitly specified (not from failed transforms)."
  (let ((items props) result)
    (while items
      (let* ((key (pop items))
             (raw (pop items))
             (val (ag-themes--resolve-value raw key face resolved)))
        (when (or val (null raw))
          (push key result)
          (push val result))))
    (nreverse result)))

;;; --- Application ---

(defun ag-themes--apply (theme base-theme flat-faces modeline-height)
  "Build THEME by overlaying face overrides on BASE-THEME.
FLAT-FACES is the flat property list of overrides.
MODELINE-HEIGHT sets :height on modeline-related faces when non-nil."
  (let* ((base-faces (ag-themes--base-theme-faces base-theme))
         (overrides (ag-themes--parse-flat-faces flat-faces))
         (overrides (if modeline-height
                        (append overrides
                                (ag-themes--modeline-faces modeline-height))
                      overrides))
         (all-specs (append base-faces overrides))
         (resolved '()))
    ;; Sequential reduce so earlier overrides are visible to later lookups
    (dolist (entry all-specs)
      (pcase-let* ((`(,face ,props) entry)
                   (new-props (ag-themes--resolve-props face props resolved))
                   (prev (cadr (assq face resolved)))
                   (merged (if prev
                               (ag-themes--plist-merge prev new-props)
                             new-props)))
        (setq resolved
              (cons (list face merged)
                    (seq-remove (lambda (x) (eq (car x) face))
                                resolved)))))
    (apply #'custom-theme-set-faces theme
           (mapcar (lambda (entry)
                     `(,(car entry) ((t ,@(cadr entry)))))
                   resolved))))

;;; --- Modeline faces ---

(defun ag-themes--modeline-faces (height)
  "Build override alist setting HEIGHT for modeline-related faces."
  (let ((attrs (list :height height)))
    (mapcar (lambda (f) (list f attrs))
            '(mode-line-buffer-id mode-line-emphasis mode-line-highlight
              persp-face-lighter-default persp-face-lighter-nil-persp
              persp-face-lighter-buffer-not-in-persp
              eyebrowse-mode-line-active eyebrowse-mode-line-inactive
              eyebrowse-mode-line-separator eyebrowse-mode-line-delimiters
              doom-modeline-buffer-timemachine
              doom-modeline-battery-error doom-modeline-battery-critical
              doom-modeline-battery-warning doom-modeline-battery-normal
              doom-modeline-battery-full doom-modeline-battery-charging
              doom-modeline-lsp-running doom-modeline-lsp-error
              doom-modeline-lsp-warning doom-modeline-lsp-success
              doom-modeline-repl-warning doom-modeline-repl-success
              doom-modeline-persp-buffer-not-in-persp doom-modeline-persp-name
              doom-modeline-evil-replace-state doom-modeline-evil-visual-state
              doom-modeline-evil-operator-state doom-modeline-evil-normal-state
              doom-modeline-evil-motion-state doom-modeline-evil-insert-state
              doom-modeline-evil-emacs-state doom-modeline-debug-visual
              doom-modeline-bar-inactive doom-modeline-bar
              doom-modeline-unread-number doom-modeline-notification
              doom-modeline-urgent doom-modeline-warning
              doom-modeline-info doom-modeline-debug
              doom-modeline-input-method-alt doom-modeline-input-method
              doom-modeline-host doom-modeline-panel doom-modeline-highlight
              doom-modeline-project-root-dir doom-modeline-project-dir
              doom-modeline-project-parent-dir
              doom-modeline-buffer-minor-mode doom-modeline-buffer-major-mode
              doom-modeline-buffer-modified doom-modeline-buffer-file
              doom-modeline-buffer-path
              doom-modeline-vspc-face doom-modeline-spc-face))))

;;; --- Diagnostic utilities ---

(defun ag-themes-get-face-colors (face)
  "Return FACE's foreground and background as a list of hex strings."
  (delete-dups
   (seq-remove
    #'null
    (seq-reduce
     (lambda (acc color)
       (if (eq 'unspecified color) acc
         (let ((hex (apply #'color-rgb-to-hex
                           (append (color-name-to-rgb color) '(2)))))
           (append acc (list hex)))))
     (list (face-attribute face :foreground)
           (face-attribute face :background))
     '()))))

(defun ag-themes-sort-colors (colors)
  "Sort COLORS from lightest to darkest by HSL lightness."
  (mapcar #'car
          (sort (mapcar (lambda (c)
                          (cons c (last (apply #'color-rgb-to-hsl
                                              (color-name-to-rgb c)))))
                        colors)
                (lambda (x y) (< (cadr y) (cadr x))))))

(defun ag-themes-get-theme-palette ()
  "Return all colors in the loaded theme, sorted light to dark."
  (ag-themes-sort-colors
   (delete-dups (seq-mapcat #'ag-themes-get-face-colors (face-list)))))

(defun ag-themes-find-faces-of-color (color)
  "Find all faces in the loaded theme that use COLOR."
  (let ((target-rgb (mapcar (lambda (n) (string-to-number (format "%f" n)))
                            (color-name-to-rgb color))))
    (seq-filter
     (lambda (face)
       (when-let* ((colors (ag-themes-get-face-colors face)))
         (seq-some
          (lambda (clr)
            (equal target-rgb
                   (mapcar (lambda (n) (string-to-number (format "%f" n)))
                           (color-name-to-rgb clr))))
          colors)))
     (face-list))))

;;; --- Theme definition macro ---

;;;###autoload
(defmacro ag-themes-deftheme (name docstring &rest plist)
  "Define theme NAME with DOCSTRING as face overrides on a base theme.

PLIST keywords:
  :base            Base theme symbol to override (required).
  :palette         Alist of (SYMBOL . VALUE) for substitution in :faces.
  :modeline-height Number for :height on modeline-related faces.
  :faces           Flat face specs: FACE :prop val :prop val FACE ..."
  (declare (indent 1))
  (let* ((base (plist-get plist :base))
         (palette (plist-get plist :palette))
         (modeline-height (plist-get plist :modeline-height))
         (raw-faces (plist-get plist :faces))
         (faces (if palette
                    (ag-themes--substitute-palette raw-faces palette)
                  raw-faces)))
    `(progn
       (deftheme ,name ,docstring)
       (ag-themes--apply ',name ',base ',faces ,modeline-height)
       (provide-theme ',name)
       (provide ',(intern (concat (symbol-name name) "-theme"))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'ag-themes)
;;; ag-themes.el ends here
