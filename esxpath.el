;;; esxpath.el --- testing phase -*- lexical-binding: t -*-

;; Copyright (C) 2013  Evan Izaksonas-Smith

;; Author: Evan Izaksonas-Smith <tali713@rastafy>
;; Keywords: extensions, lisp

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

;; 

;;; Code:
(require 'esxml)


;;probable call

(defvar test-sheet
  (sxml-to-esxml '(bookstore
                   (book
                    (title (@ (lang "eng")) "Harry Potter")
                    (price  "29.99"))
                   (book
                    (title (@ (lang "eng")) "Learning XML")
                    (price  "39.95")))))

(defmacro esxpath-transform (pathspec)
  (pcase pathspec
    (`(,(and keyword) (pred keywordp) . ,_)
     (error "Keyword %s unhandled" keyword))
    (`(,(and root (pred symbolp)) ,(and body (pred symbolp)))
     `(if (eq ',root (car ,body)) (nthcdr 2 ,body)))
    (`(,(and node (pred symbolp)) ,(and body (pred consp)))
     `(remove-if-not (apply-partially 'esxpath-tag-p ',node) 
                     (esxpath-transform ,body)))
    (_ pathspec)))

(pp (macroexpand '(esxpath-transform (title (book (bookstore test-sheet))))))
(remove-if-not
 (apply-partially 'esxpath-tag-p 'title)
 (remove-if-not
  (apply-partially 'esxpath-tag-p 'book)
  (if
    (eq 'bookstore
        (car test-sheet))
      (list test-sheet))))

(defmacro plambda (&rest pcases)
  (declare (indent defun))
  (let ((args (gensym "args")))
    `(lambda (,args)
       (pcase ,args ,@pcases))))


(defun esxpath-tag-p (tag esxml)
  (and (consp esxml)
       (eq tag (car esxml))))

(defun esxpath-attrs-match (test-attrs esxml)
  (pcase-let ((`(,_ ,attrs . ,_) esxml))
    (every (plambda
             (`(,key . ,(pred stringp)) (equal (assoc-default key test-attrs)
                                               (assoc-default key attrs)))
             (x (error "Unhandled attr: %s" x)))
           test-attrs)))

(defun esxpath-select-root (root esxml)
  "Returns the root node as a node-set if it matches ROOT.
ROOT is a symbol, the tag of the root node."
  (if (esxpath-tag-p root esxml) (list esxml)))

(defun esxpath-get-children (esxml)
  (pcase esxml
    (`(,_ ,_ . ,body) body)))

(defun on-nodeset (function)
  (lambda (node-set)
    (apply 'append (mapcar function node-set))))

(defalias 'esxpath-select-children (on-nodeset 'esxpath-get-children))

(defun esxpath-select (tag &optional attrs node-set)
  (declare (advertised-calling-convention '(tag [attrs] node-set) ""))
  (let* ((node-set (or node-set (prog1 attrs (setq attrs nil))))
         (filter (cond ((and tag attrs)
                        (lambda (node)
                          (and (esxpath-tag-p tag node)
                               (esxpath-attrs-match attrs node))))
                       (tag (lambda (node) (esxpath-tag-p tag node)))
                       (attrs (lambda (node) (esxpath-attrs-match attrs node))))))
    (remove-if-not filter node-set)))

(defun esxpath-child-p (child-node parent-node)
  (find child-node (esxpath-get-children parent-node)))

(defun esxpath-descendent-p (descendent-node ancestor-node)
  (or (esxpath-child-p descendent-node ancestor-node)
      (let ((children (esxpath-get-children ancestor-node)))
        (if children (every (apply-partially 'esxpath-descendent-p descendent-node)
                            children)))))

(defun esxpath-has-child (pred)
  (lambda (node)
    (any pred (esxpath-get-children node))))

(defun esxpath-select-with-child (pred)
  (lambda (node-set)
    (remove-if-not (esxpath-has-child pred)
                   node-set)))


(defun esxpath-select-from-children (tag attrs node-set)
  (esxpath-select tag attrs (esxpath-select-children node-set)))



(defun esxpath-fill-missing-attributes (pathspec)
  (pcase pathspec
    (`(,key
       ,arg
       ,(and pathspec (pred consp)))
     `(,key ,arg ,(esxpath-fill-missing-attributes pathspec)))

    (`(,(and key (pred keywordp))
       ,(and pathspec (pred consp)))
     `(,key ,(esxpath-fill-missing-attributes pathspec)))

    (`(,key
       ,(and pathspec (pred consp)))                 
     `(,key () ,(esxpath-fill-missing-attributes pathspec)))

    (`(,key)                 
     `(,key ()))

    (_ pathspec)))

(defun esxpath-transform (pathspec)
  `(lambda (esxml)
     ,(pcase-recur `(,(car (last pathspec))
                     (esxpath-select-root ',(first pathspec) esxml))
        ;; (`((:has-child ,pathspec) ,form) (esxpath-select-with-child ))
        (`((,(and tag (pred symbolp))
            ,(and attrs (pred attrsp))
            ,pathspec)
           ,form)
         (:recur pathspec `(esxpath-select-from-children ',tag ',attrs ,form)))
        (`((,(and tag (pred symbolp))
            ,(and attrs (pred attrsp)))
           ,form)
         `(esxpath-select-from-children ',tag ',attrs ,form))
        (`(,pathspec ,form) `((pathspec ,pathspec) (form ,form))))))

(defmacro esxpath (pathspec)
  (esxpath-transform (esxpath-fill-missing-attributes pathspec)))



(pp (macroexpand '(esxpath (bookstore (book)))))
(pp (esxpath-transform
     (esxpath-fill-missing-attributes
      '(bookstore (book (title ((price . "1.00"))))))
     ))



(funcall (esxpath (bookstore (book)))
         test-sheet)






(provide 'esxpath)
;;; esxpath.el ends here
