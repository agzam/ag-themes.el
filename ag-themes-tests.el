;;; ag-themes-tests.el --- Tests for ag-themes -*- lexical-binding: t; -*-

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -l ert -l ag-themes.el -l ag-themes-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ag-themes)

;;; --- Property resolution ---

(ert-deftest ag-themes-resolve-props-unsets-color-with-unspecified ()
  "A hand-written nil colour reaches Emacs as `unspecified'."
  (should (equal (ag-themes--resolve-props 'org-table '(:background nil) nil)
                 '(:background unspecified))))

(ert-deftest ag-themes-resolve-props-unsets-every-nil-invalid-attribute ()
  "Every attribute nil cannot unset is rewritten, colours included."
  (dolist (key ag-themes--nil-invalid-attributes)
    (should (equal (ag-themes--resolve-props 'some-face (list key nil) nil)
                   (list key 'unspecified)))))

(ert-deftest ag-themes-resolve-props-keeps-nil-where-emacs-takes-it ()
  "Attributes Emacs switches off with nil keep it."
  (should (equal (ag-themes--resolve-props 'mode-line '(:box nil :underline nil) nil)
                 '(:box nil :underline nil))))

(ert-deftest ag-themes-resolve-props-passes-values-through ()
  "Ordinary values survive untouched."
  (should (equal (ag-themes--resolve-props 'default '(:foreground "#abcdef" :weight bold) nil)
                 '(:foreground "#abcdef" :weight bold))))

(ert-deftest ag-themes-resolve-props-drops-failed-transform ()
  "A transform with no source drops its pair instead of unsetting the attribute."
  (should (equal (ag-themes--resolve-props 'orphan '(:background (darker 10)) nil)
                 nil)))

(ert-deftest ag-themes-resolve-props-resolves-transform-against-resolved ()
  "A transform reads its source from the accumulated alist."
  (let ((resolved '((default (:background "#ffffff")))))
    (should (equal (ag-themes--resolve-props
                    'default '(:background (darker 100)) resolved)
                   '(:background "#000000000000")))))

;;; --- Blend transform ---

;; Batch Emacs has no display, so `color-name-to-rgb' quantises every input to
;; one bit per channel.  Colours here stay on the black/white/primary corners
;; that survive it; a subtler literal would read back as a different colour.

(ert-deftest ag-themes-blend-reads-the-source-each-shape-names ()
  "Every blend shape mixes its colour into the source it selects."
  (let ((resolved '((default (:background "#000000" :foreground "#000000"))
                    (other (:background "#000000")))))
    ;; (blend AMOUNT COLOR) - same face, same property
    (should (equal (ag-themes--resolve-value
                    '(blend 50 "#ffffff") :background 'default resolved)
                   "#7f7f7f"))
    ;; (blend AMOUNT COLOR :src-prop) - same face, another property
    (should (equal (ag-themes--resolve-value
                    '(blend 50 "#ffffff" :foreground) :background 'default resolved)
                   "#7f7f7f"))
    ;; (blend AMOUNT COLOR src-face :src-prop) - another face's property
    (should (equal (ag-themes--resolve-value
                    '(blend 25 "#ff0000" other :background) :background 'target resolved)
                   "#3f0000"))))

(ert-deftest ag-themes-blend-spans-source-to-color ()
  "A zero amount keeps the source and a full amount reaches the colour."
  (let ((resolved '((default (:background "#ff0000")))))
    (should (equal (ag-themes--resolve-value
                    '(blend 0 "#00ff00" default :background) :background 'x resolved)
                   "#ff0000"))
    (should (equal (ag-themes--resolve-value
                    '(blend 100 "#00ff00" default :background) :background 'x resolved)
                   "#00ff00"))))

(ert-deftest ag-themes-resolve-props-drops-failed-blend ()
  "A blend with no source drops its pair, as the shifting transforms do."
  (should (equal (ag-themes--resolve-props
                  'orphan '(:background (blend 50 "#ff0000")) nil)
                 nil)))

(ert-deftest ag-themes-unary-transform-rejects-a-color-argument ()
  "A shifting transform handed a color survives as a literal.
Its color function takes no second color, so the expression must not
reach one."
  (let ((resolved '((default (:background "#ffffff")))))
    (should (equal (ag-themes--resolve-value
                    '(darker 10 "#fff") :background 'default resolved)
                   '(darker 10 "#fff")))))

(ert-deftest ag-themes-resolve-value-passes-non-transform-lists-through ()
  "Lists that only look like transforms stay literal."
  (should (equal (ag-themes--resolve-value
                  '(variable-pitch org-block) :inherit 'org-verse nil)
                 '(variable-pitch org-block)))
  (should (equal (ag-themes--resolve-value
                  '(:color "#dedae0") :box 'mode-line nil)
                 '(:color "#dedae0"))))

;;; --- Theme application ---

(ert-deftest ag-themes-apply-emits-no-nil-colors ()
  "Nothing a theme hands `custom-theme-set-faces' carries a nil colour."
  (let (specs)
    (cl-letf (((symbol-function 'ag-themes--base-theme-faces)
               (lambda (_theme) '((default (:foreground "#000000" :background "#ffffff")))))
              ((symbol-function 'custom-theme-set-faces)
               (lambda (_theme &rest args) (setq specs args))))
      (ag-themes--apply 'probe-theme 'probe-base
                        '(org-table :background nil
                          magit-header-line :background nil
                          mode-line :box nil)
                        nil))
    (pcase-dolist (`(,face ((t . ,attrs))) specs)
      (dolist (key '(:foreground :background))
        (should-not (and (plist-member attrs key)
                         (null (plist-get attrs key))
                         (format "%s has a nil %s" face key)))))
    (should (equal (cadr (assq 'org-table specs)) '((t :background unspecified))))
    (should (equal (cadr (assq 'mode-line specs)) '((t :box nil))))))

(ert-deftest ag-themes-apply-lets-an-override-switch-off-inverse-video ()
  "An override's nil beats a base theme's `:inverse-video' t.
Left on, it swaps the foreground and background the override computed."
  (let (specs)
    (cl-letf (((symbol-function 'ag-themes--base-theme-faces)
               (lambda (_theme)
                 '((diff-refine-added (:inherit diff-added :inverse-video t)))))
              ((symbol-function 'custom-theme-set-faces)
               (lambda (_theme &rest args) (setq specs args))))
      (ag-themes--apply 'probe-theme 'probe-base
                        '(diff-refine-added :inverse-video nil
                          :foreground "#116329" :background "#beedcb")
                        nil))
    (should (equal (cadr (assq 'diff-refine-added specs))
                   '((t :inherit diff-added :inverse-video nil
                        :foreground "#116329" :background "#beedcb"))))))

(provide 'ag-themes-tests)
;;; ag-themes-tests.el ends here
