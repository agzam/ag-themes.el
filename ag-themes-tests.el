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

(provide 'ag-themes-tests)
;;; ag-themes-tests.el ends here
