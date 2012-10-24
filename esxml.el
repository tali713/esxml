;;; esxml.el --- Library for working with xml via esxml and sxml
;; Copyright (C) 2012  

;; Author: Evan Izaksonas-Smith <izak0002 at umn dot edu>
;; Maintainer: Evan Izaksonas-Smith
;; Created: 15th August 2012
;; Version: 0.1.0
;; Package-Requires: ((kv "0.0.5"))
;; Keywords: tools, lisp, comm
;; Description: A library for easily generating XML/XHTML in elisp
;;
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

;; This is XML/XHTML done with S-Expressions in EmacsLisp.  Simply,
;; this is the easiest way to write HTML or XML in Lisp.

;;; Code:
(eval-when-compile
  (require 'cl))
(require 'xml)
(require 'kv)

(defun string-trim-whitespace (string)
  "A simple function, strips the whitespace from beginning and
end of the string.  Leaves all other whitespace untouched."
  (replace-regexp-in-string
   (rx string-start (* whitespace)
       (group (+? anything))
       (* whitespace) string-end)
   "\\1"
   string))

(defun esxml-trim-ws (esxml)
  "This may cause problems, is intended for parsing xml into sxml
but may eroneously delete desirable white space."
  (if (stringp esxml) (string-trim-whitespace esxml)
    (destructuring-bind (tag attrs . body) esxml
      `(,tag ,attrs
             ,@(mapcar 'esxml-trim-ws body)))))

(defun esxml--convert-pair (pair)
  "Converts from cons cell to attribute pair.  Not intended for
general use."
  (format "%S=%S" (car pair) (cdr pair)))


;; While the following could certainly have been written using format,
;; concat makes them easier to read.  Update later if neccesary for
;; efficiency.

;; Though at first glance the recursive nature of this function might
;; give one pause, since xml is a recursive data type, a recursive
;; parser is an optimal strategy.  each node will be visited exactly
;; once during the transformation.
;;
;; Further, since a string is a terminal node and since xml can be
;; represented as a string, non dynamic portions of the page may be
;; precached quite easily.
(defun esxml-to-xml (esxml)
  "This translates an esxml expression, i.e. that which is
returned by xml-parse-region.  The structure is defined as a
string or a list where the first element is the tag the second is
an alist of attribute value pairs and the remainder of the list
is 0 or more esxml elements.

 (TAG ATTRS &rest BODY) || STRING

TAG: is the tag and must be a symbol.

ATTRS: is an alist of attribute pairs each pair must be of the
       form (KEY . VALUE).

KEY: is the name of the attribute and must be a symbol.

VALUE: is the value of the attribute and must be a string.

BODY: is zero or more esxml expressions.  Having no body forms
      implies that the tag should be self closed.  If there is
      one or more body forms the tag will always be explicitly
      closed, even if they are the empty string.

STRING: if the esxml expression is a string it is returned
        unchanged, this allows for caching of any constant parts,
        such as headers and footers.
"
  (if (stringp esxml) esxml
    (destructuring-bind (tag attrs &rest body) esxml
      (concat "<" (symbol-name tag)
              (when attrs
                (concat " " (mapconcat 'esxml--convert-pair attrs " ")))
              (if body
                  (concat ">" (mapconcat 'esxml-to-xml body "")
                          "</" (symbol-name tag) ">")
                "/>")))))

(defun pp-esxml-to-xml (esxml)
  "This translates an esxml expresion as `esxml-to-xml' but
indents it for ease of human readability, it is neccesarrily
slower and will produce longer output."
  (if (stringp esxml) esxml
    (destructuring-bind (tag attrs . body) esxml
      (concat "<" (symbol-name tag) " "
              (when attrs
                  (mapconcat 'esxml--convert-pair attrs " "))
              (if body
                  (concat ">\n"
                          (replace-regexp-in-string
                           "^" "  "
                           (mapconcat 'pp-esxml-to-xml
                                      body "\n"))
                          "\n</" (symbol-name tag) ">")
                "/>")))))

(defun sxml-to-esxml (sxml)
  "Translates sxml to esxml so the common standard can be used.
See: http://okmij.org/ftp/Scheme/SXML.html."
  (if (stringp sxml) sxml
    (pcase sxml
      (`(,tag (@ . ,attrs) . ,body)
       `(,tag ,(mapcar (lambda (attr)
                         (cons (first attr)
                               (or (second attr)
                                   (prin1-to-string (first attr)))))
                       attrs)
              ,@(mapcar 'sxml-to-esxml body)))
      (`(,tag . ,body)
       `(,tag nil
              ,@(mapcar 'sxml-to-esxml body))))))

(defun sxml-to-xml (sxml)
  "Translates sxml to xml, via esxml, hey it's only a constant
factor. :)"
  (esxml-to-xml (sxml-to-esxml sxml)))


;;; Some generators for common problems
;; Tabular and listed data are common patterns, so rather than do
;; something like:
;; (esxml-to-xml
;;  `(html ()
;;         (body ()
;;               ,@(mapcar (lambda (url-entry)
;;                           (destructuring-bind (url name)
;;                               `(li ()
;;                                    (a ((href . ,url))
;;                                       ,name))))))))
;; we should instead define this cleanly.

(defun esxml-link (url &rest body)
  `(a ((href . ,url)) ,@body))

(defun esxml-label (label-text attribs &rest body)
  "Make a label with LABEL-TEXT and ATTRIBS.

Optionally include the BODY."
  (let ((label-element
         `(label
           ,attribs
           (span () ,(concat label-text ": ")))))
    (if body
        (append label-element body)
        label-element)))

(defun esxml-input (name type &optional value)
  "Make an HTML INPUT control.

VALUE is optional, if it's supplied whatever is supplied is used.
`nil' is the blank string."
  `(input ((name . ,name)
           (type . ,type)
           (placeholder . ,name)
           ,@(when value `((value . ,value))))))

(defun esxml-textarea (name &optional content)
  "Make an HTML TextArea control."
  `(textarea ((name . ,name)
              (placeholder . ,name))
             ;; textareas require a body all the time
             ,@(if content (list content) "")))

(defun esxml-listify (body &optional ordered-p)
  `(,(if ordered-p 'ol 'ul) ()
    ,@(map-bind body
                `(li () ,@body)
                body)))

(defun esxml-create-bookmark-list (bookmark-list seperator &optional ordered-p)
  (esxml-listify (map-bind (url name &optional description)
                           `(,(esxml-link url name)
                             ,@(when description
                                 `(,seperator ,description)))
                           bookmark-list)
                 ordered-p))
;;; Example
;; (setq bookmark-list
;;       '(("http://www.emacswiki.org" "Emacs Wiki" "Accept no substitutes")
;;         ("http://www.github.com/" "Github")
;;         ("http://www.google.com" "Google" "Everyones favorite search engine")))

;; (esxml-to-xml (esxml-create-bookmark-list bookmark-list ": "))



(defun xml-to-esxml (string &optional trim)
  (with-temp-buffer
    (insert string)
    (let ((parse-tree (libxml-parse-xml-region (point-min)
                                               (point-max))))
      (if trim
          (esxml-trim-ws parse-tree)
        parse-tree))))

(defun esxml-get-by-key (esxml key value)
  "Returns a list of all elements whose wttribute KEY match
VALUE.  KEY should be a symbol, and VALUE should be a string.
Will not recurse below a match."
  (unless (stringp esxml)
    (destructuring-bind (tag attrs . body) esxml 
      (if (equal value
                 (assoc-default key attrs))
          (list esxml)
        (apply 'append (mapcar (lambda (sexp)
                                 (esxml-get-by-key sexp key value))
                               body))))))

(defun esxml-get-tags (esxml tags)
  "Returns a list of all elements whose tag is a member of TAGS.
TAGS should be a list of tags to be matched against. Will not
recurse below a match."
  (unless (stringp esxml) 
    (let ((tag (car esxml))
          (attrs (cadr esxml))
          (body (cddr esxml)))
      (if (member tag tags)
          (list esxml)
        (apply 'append (mapcar (lambda (sexp)
                                 (esxml-get-tags sexp tags))
                               body))))))

(defun esxml-get-forms (esxml)
  "Returns a list of all forms."
  (esxml-get-tags esxml '(form)))

(provide 'esxml)
;;; esxml.el ends here
