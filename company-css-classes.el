;;; company-css-classes.el --- Company backend for css files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 russ
;;
;; Author: russ <http://github/russ>
;; Maintainer: russ <russ@algo>
;; Created: May 21, 2020
;; Modified: May 21, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/russ/company-css-classes
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A Company backend for CSS files.
;;
;;  Currently requires a path to a css file to be set.
;;
;;  That file is parsed and the resulting classes are fed as a company backend.
;;
;;; Code:

(require 'cl-macs)
(require 'dash)
(require 's)

;;;
;;; Configuration
;;;

(defvar company-css-classes-filepath nil
  "The css file to parse for classes.")

;;;
;;; Utilities
;;;

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;;;
;;; Caching
;;; This caching style was derived from @glittershark in `org-clubhouse`
;;; https://github.com/glittershark/org-clubhouse/blob/f00c22a49af3798c8a4be6d1831b5142ff56a19e/org-clubhouse.el#L408
;;;

(defvar company-css-classes-cache-clear-functions ())

(defmacro defcache (name &optional docstring &rest body)
  "A mechanism for creating cached functions.

Creates two functions and one var:
`NAME-cache' is a var with the current cached value.
`NAME' is a function with DOCSTRING and BODY that passes back the cached value
if it exists.
`clear-NAME-cache' is a function to clear the cache itself.

TODO at present DOCSTRING is actually required, otherwise the first form in body
will be used."
  (let* ((doc (when docstring (list docstring)))
         (cache-var-name (intern (concat (symbol-name name)
                                         "-cache")))
         (clear-cache-function-name
          (intern (concat "clear-" (symbol-name cache-var-name)))))
    `(progn
       (defvar ,cache-var-name :no-cache)
       (setq ,cache-var-name :no-cache)

       (defun ,name ()
         ,@doc
         (when (equal :no-cache ,cache-var-name)
           (setq ,cache-var-name (progn ,@body)))
         ,cache-var-name)
       (defun ,clear-cache-function-name ()
         (interactive)
         (setq ,cache-var-name :no-cache))

       (push (quote ,clear-cache-function-name)
             company-css-classes-cache-clear-functions))))

(defun company-css-classes-clear-cache ()
  "Clears all caches created with `defcache'."
  (interactive)
  (setq company-css-classes-cache-clear-functions '())
  ;; (-map #'funcall company-css-classes-cache-clear-functions)
  )

(comment
 ;; defcache usage
 (company-css-classes-clear-cache)
 (defcache example-fn
   "Required doc string."
   (message "howdy")
   "cowboy")

 (example-fn)
 example-fn-cache
 (clear-example-fn-cache)
 )

;;;
;;; Parse the CSS file and `get-classes'
;;;

(defun css-file-path ()
  "Return the css-file-path for to be parsed.

  TODO use the defvar
  TODO read from .dir-locals.el
  TODO work from project-dir?
  (file-truename company-css-classes-filepath)"
  (file-truename "~/russmatney/yodo/public/css/main-built.css"))

(defconst css-imenu-generic-expression
  "^[ \t]*\\([[:word:].:#, \t-]+\\)\\s-*{"
  "Regular expression matching any selector. Used by imenu.")

;; TY: https://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list
(defun matches-in-file (regexp filename)
  "Return a list of strings matching REGEXP in FILENAME."
  (let ((matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (current-buffer)
          (with-temp-buffer
            (insert-file-contents-literally filename)
            (save-restriction
              (widen)
              (goto-char 1)
              (while (search-forward-regexp regexp nil t 1)
                ;; TODO parse styles from css here as metadata
                (push (match-string 0) matches))))))
      matches)))

(defun trim-class-name (s)
  "Trims a passed class-name S.

The regex currently includes leading space and dots,
and trailing open-brackets."
  (s-chop-suffix " {" (s-chop-prefix "." s)))

(defcache get-classes
  "Pull the filename from config, parse the file into classes, clean them up.
  Returns css-classes as a list."
  (let* ((filepath (css-file-path))
         (parsed (matches-in-file css-imenu-generic-expression filepath)))
    (->> parsed
         (cl-remove-if-not (lambda (s) (s-prefix? "." s)))
         (cl-map 'list #'trim-class-name)
         (cl-remove-if
          (lambda (s)
            (or
             ;; remove all but 500 level tailwind colors
             (s-suffix? "-75" s)
             (s-suffix? "-100" s)
             (s-suffix? "-150" s)
             (s-suffix? "-200" s)
             (s-suffix? "-300" s)
             (s-suffix? "-400" s)
             ;; (s-suffix? "-500" s)
             (s-suffix? "-600" s)
             (s-suffix? "-700" s)
             (s-suffix? "-800" s)
             (s-suffix? "-900" s)
             (s-suffix? "-1000" s)))))))

(comment
 (get-classes)
 (length (get-classes))
 get-classes-cache
 (clear-get-classes-cache)

(let ((s "hello-600"))
  (or
   (s-suffix? "-900" s)
   (s-suffix? "-700" s)
   )))

;;;
;;; Company backend
;;;

(defun company-css-classes-backend (command &optional arg &rest ignored)
  "A backend for company mode that serves css classes from a specified file.

COMMAND, ARG, and IGNORED are dictated by company-mode's backend spec."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-css-classes-backend))
    (prefix (company-grab-symbol))
    (candidates (cl-remove-if-not
                 ;; (lambda (c) (sample-fuzzy-match arg c))
                 (lambda (c) (string-prefix-p arg c))
                 (get-classes)))))

;;;
;;; Minor mode necessary?
;;;

(define-minor-mode company-css-classes-mode
  "If enabled, updates to the todo keywords on org headlines will update the
linked ticket in Clubhouse."
  :group 'company
  :lighter "Company-CSS-Classes"
  :keymap '())

(provide 'company-css-classes)
;;; company-css-classes.el ends here
