;;; esxml-form.el --- HTML Forms with EmacsLisp  -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: data, lisp
;; Created: 23rd September 2012
;; Package-Requires: ((kv "0.0.7")(esxml "0.0.7"))
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an HTML Form processing library for ESXML. It ties together
;; the things in Lisp you need to make Forms work; like validation,
;; database validation and rendering.


;;; Code:

(require 'kv)
(require 'esxml)

(defconst esxml-form-field-defn
  '(name
    &key
    (type :regex) ; or :password, :email
    (regex ".+")
    ;; :html is one of :text :textarea :password
    ;; :checkbox :radio :select
    ;;
    ;; Further options should deal with the extra
    ;; data required by some of those types, for
    ;; example, :checkbox_selected could be used
    ;; for the checkbox
    (html :text)
    (type-check-failure "the content of the field was wrong")
    db-key
    db-check)
  "The Lisp definition used for a field.")

(defmacro* esxml-form-field-set ((&key db db-key) &rest field-args)
  "Make a field set.

A field set binds some field parameters together with some other
data, for example, a database."
  (let ((fields (make-symbol "fieldsv")))
    `(let ((,fields
            (map-bind ;; FIXME optional fields?
             ,esxml-form-field-defn
             (list name
                   :type type
                   :regex regex
                   :type-check-failure type-check-failure
                   :html html
                   :db-check db-check
                   :db-key db-key)
             (quote ,field-args))))
       (list :db (quote ,db)
             :db-key (quote ,db-key)
             :fields ,fields))))

(defun esxml-form-field-set-fields (fs)
  (plist-get fs :fields))


;; Verification stuff

(defvar esxml-form-field-set-email-verify-re
  "[a-zA-Z0-9-]+@[a-zA-Z0-9.-]+\\(.com\\|.net\\|.org\\)$")

(defun esxml-form-field-set--validity-check (field value)
  "Do a validity check on the FIELD."
  (case (plist-get field :type)
    (:regex
     (equal
      0
      (string-match
       (plist-get field :regex)
       (or value ""))))
    (:email
     (string-match esxml-form-field-set-email-verify-re value))
    (:password
     ;; really? is this a verification?
     t)))

(defun esxml-form-field-set-check (fs params)
  "Check field set FS against the PARAMS values.

Checks that ALL the required fields are there and that any field
that is there is correclty specified.

Returns the empty list when it passes and an alist of field,
errors if it fails."
  (loop
     with field-value
     for (field-name . field-plist) in (esxml-form-field-set-fields fs)
     unless
       (esxml-form-field-set--validity-check
        field-plist
        (setq field-value (cdr (kvassoqc field-name params))))
     collect (list ; return the error structure
              field-name
              field-value
              (plist-get field-plist :type-check-failure))))

(defun esxml-form-field-set->esxml (fs &optional params)
  "Fieldset FS to ESXML description of fields.

PARAMS, if supplied, is an ALIST of field-names -> value bindings
which are used to validate the fields and assigned to the
respective fields in the output.

The output is an ESXML representation of a form in label
style (an HTML LABEL element contains the controls).

If validation errors occur they are output as a DIV with class
\"error\", again, inside the LABEL."
  (let ((errors (when params
                  (esxml-form-field-set-check fs params))))
    (destructuring-bind (field-name &rest field-plist) fs
      `(fieldset
        ()
        ,@(eval
           `(map-bind
             ,esxml-form-field-defn
             (let* ((symname (symbol-name name))
                    (value (aget params symname))
                    (err (aget errors name)))
               (apply
                'esxml-label
                (cons
                 symname
                 (cons
                  (case html
                    (:text (esxml-input symname "text" value))
                    (:password (esxml-input symname "password" value))
                    (:checkbox (esxml-input symname "checkbox" value))
                    (:radio (esxml-input symname "radio" value))
                    ;;(:select (esxml-select (symbol-name name)))
                    (:textarea (esxml-textarea symname value)))
                  (when err
                    (list
                     `(div
                       ((class . "error"))
                       ,(elt err 1))))))))
             (esxml-form-field-set-fields fs)))))))

(provide 'esxml-form)

;; ends
