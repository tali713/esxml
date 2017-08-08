(require 'cl-lib)


;;; CSS selector parsing

;; https://www.w3.org/TR/selectors/#w3cselgrammar
;; https://www.w3.org/TR/selectors4/#grammar
;; https://www.w3.org/TR/2003/WD-css3-syntax-20030813/#detailed-grammar

;; you might be wondering why I'm using both level 3 and 4 standards,
;; well, the level 3 one has a buggy lexer section whereas level 4
;; omits crucial parser definitions, so both have to be used...

(defvar esxml-css-selector-token-matchers
  (let* ((h "[0-9a-f]")
         (nl "\n\\|\r\n\\|\r\\|\f")
         (nonascii "[\200-\U0010ffff]")
         (unicode (format "\\%s\\{1,6\\}[ \t\r\n\f]?" h))
         (escape (format "\\(?:%s\\)\\|\\[ -~\200-\U0010ffff]" unicode))
         (nmstart (format "[a-z]\\|%s\\|\\(?:%s\\)" nonascii escape))
         (nmchar (format "[a-z0-9-]\\|%s\\|\\(?:%s\\)" nonascii escape))
         (num "[0-9]+\\|[0-9]*\\.[0-9]+")
         (string1 (format "\"\\(?:[\t !#$%%&(-~]\\|\\(?:%s\\)\\|\'\\|%s\\|\\(?:%s\\)\\)*\"" nl nonascii escape))
         (string2 (format "'\\(?:[\t !#$%%&(-~]\\|\\(?:%s\\)\\|\"\\|%s\\|\\(?:%s\\)\\)*'" nl nonascii escape))
         (ident (format "[-]?\\(?:%s\\)\\(?:%s\\)*" nmstart nmchar))
         (unit (format "[-]?\\(?:%s\\)\\(?:%s\\)+" nmstart nmchar))
         (name (format "\\(?:%s\\)+" nmchar)))

    `((whitespace . "[ \t\r\n\f]+")
      (string . ,(format "\\(?:%s\\|%s\\)" string1 string2))
      (ident . ,ident)
      (hash . ,(format "#%s" name))
      (function . ,(format "%s(" ident))
      (number . ,num)
      (dimension . ,(format "\\(?:%s\\)%s" num unit))
      (prefix-match . "\\^=")
      (suffix-match . "\\$=")
      (substring-match . "\\*=")
      (include-match . "~=")
      (dash-match . "|=")
      (comma . ",")
      (gt . ">")
      (plus . "\\+")
      (minus . "-")
      (tilde . "~")
      (asterisk . "\\*")
      (period . "\\.")
      (equals . "=")
      (colon . ":")
      (lbracket . "\\[")
      (rbracket . "\\]")
      (rparen . ")"))))

(defun esxml-tokenize-css-selector (string)
  (let (result)
    (with-temp-buffer
      (insert-string string)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((max-length 0)
              longest)
          (dolist (matcher esxml-css-selector-token-matchers)
            (let ((id (car matcher))
                  (re (cdr matcher)))
              (when (looking-at re)
                (let* ((token (match-string 0))
                       (length (length token)))
                  (when (> length max-length)
                    (setq max-length length)
                    (setq longest (cons id token)))))))
          (when (not longest)
            (error "Invalid token detected: %s"
                   (buffer-substring (point) (point-max))))
          (push longest result)
          (goto-char (+ (point) max-length)))))
    (nreverse result)))

;; the alternative is creating a mutable object with peek/next methods
;; and passing it around, so I chose the one requiring less typing, a
;; dynamically bound variable :<

(defvar esxml-token-stream)

;; css-selector:
;;   css-selector-list;
;; css-selector-list:
;;   complex-css-selector [ comma whitespace* complex-css-selector ]*;
;; complex-css-selector:
;;   compound-css-selector [ css-combinator compound-css-selector ]* whitespace*;
;; css-combinator:
;;   whitespace+ | whitespace* [ '>' | '+' | '~' ] whitespace*;
;; compound-css-selector:
;;   css-type-selector css-modifier* | css-modifier+;
;; css-type-selector:
;;   IDENT | *;
;; css-modifier:
;;    css-id | css-class | css-attrib | css-pseudo;
;; css-id:
;;   HASH;
;; css-class:
;;   '.' IDENT;
;; css-attrib:
;;   '[' whitespace* css-attrib-name ']'
;;   | '[' whitespace* css-attrib-name css-attrib-match css-attrib-value whitespace* ']';
;; css-attrib-name:
;;   IDENT whitespace*;
;; css-attrib-match:
;;   [ '=' | PREFIX-MATCH | SUFFIX-MATCH | SUBSTRING-MATCH | INCLUDE-MATCH | DASH-MATCH ] whitespace*;
;; css-attrib-value:
;;   IDENT | STRING;
;; css-pseudo:
;;   ':' ':'? [ IDENT | css-functional-pseudo ];
;; css-functional-pseudo:
;;   FUNCTION whitespace* [ css-expression whitespace* ]+ ')';
;; css-expression:
;;   '+' | '-' | DIMENSION | NUMBER | STRING | IDENT

(defmacro esxml-with-parse-shorthands (&rest body)
  `(cl-macrolet ((peek () '(car esxml-token-stream))
                 (next () '(pop esxml-token-stream))
                 (accept (type) `(and (peek) (eq (car (peek)) ,type)
                                      (cdr (next))))
                 (eat-whitespace () '(while (accept 'whitespace))))
     ,@body))
(def-edebug-spec esxml-with-parse-shorthands (body))

;; TODO: error out on invalid pseudo-class arguments
(defun esxml-parse-css-selector (string)
  (let* ((esxml-token-stream (esxml-tokenize-css-selector string))
         (result (esxml-parse-css-selector-list)))
    (when esxml-token-stream
      (error "Trailing garbage: %s"
             (mapconcat 'cdr esxml-token-stream "")))
    result))

(defun esxml-parse-css-selector-list ()
  (esxml-with-parse-shorthands
   (let ((first (esxml-parse-complex-css-selector))
         result)
     (when (not first)
       (error "Expected at least one selector"))
     (push first result)

     (while (accept 'comma)
       (eat-whitespace)
       (let ((selector (esxml-parse-complex-css-selector)))
         (when (not selector)
           (error "Expected selector after comma"))
         (push selector result)))
     (nreverse result))))

(defun esxml-parse-complex-css-selector ()
  (esxml-with-parse-shorthands
   (let ((first (esxml-parse-compound-css-selector))
         result done)
     (when first
       (push first result)

       (while (not done)
         (let ((combinator (esxml-parse-css-combinator)))
           (if combinator
               (let ((compound (esxml-parse-compound-css-selector)))
                 (cond
                  (compound
                   (setq result (append (list compound combinator) result)))
                  ;; allow whitespace before comma
                  ((not (eq (car (peek)) 'comma))
                   (error "Trailing combinator"))))
             (setq done t))))
       (nreverse result)))))

(defun esxml-parse-css-combinator ()
  (esxml-with-parse-shorthands
   ;; NOTE: whitespace-surrounded combinators are distinguished from
   ;; whitespace-only ones by checking whether there has been
   ;; whitespace followed by a non-blank combinator
   (let ((leading-whitespace-p (eq (car (peek)) 'whitespace))
         result)
     (eat-whitespace)
     (let ((type (car (peek))))
       (cond
        ((member type '(gt plus tilde))
         (next)
         (cond
          ((eq type 'gt)
           (setq result '((combinator . child))))
          ((eq type 'plus)
           (setq result '((combinator . direct-sibling))))
          ((eq type 'tilde)
           (setq result '((combinator . indirect-sibling)))))
         (eat-whitespace))
        (leading-whitespace-p
         (setq result '((combinator . descendant))))
        (t nil)))
     result)))

(defun esxml-parse-compound-css-selector ()
  (esxml-with-parse-shorthands
   (let ((type-selector (esxml-parse-css-type-selector))
         done
         result)
     ;; NOTE: css-type-selector css-modifier* | css-modifier+; is
     ;; equivalent to: [ css-type-selector | css-modifier ] css-modifier*;
     (if type-selector
         (push type-selector result)
       (let ((modifier (esxml-parse-css-modifier)))
         (if modifier
             (push modifier result)
           ;; NOTE: this allows the trailing combinator error to be thrown
           (setq done t))))

     (while (not done)
       (let ((modifier (esxml-parse-css-modifier)))
         (if modifier
             (push modifier result)
           (setq done t))))
     (when (> (cl-count 'id result :key 'car) 1)
       (error "Only one id selector allowed per compound"))
     (nreverse result))))

(defun esxml-parse-css-type-selector ()
  (esxml-with-parse-shorthands
   (let ((token (peek)))
     (cond
      ((eq (car token) 'ident)
       (next)
       (cons 'tag (intern (cdr token))))
      ((eq (car token) 'asterisk)
       (next)
       '(wildcard))
      (t nil)))))

(defun esxml-parse-css-modifier ()
  (or (esxml-parse-css-id)
      (esxml-parse-css-class)
      (esxml-parse-css-attrib)
      (esxml-parse-css-pseudo)))

(defun esxml-parse-css-id ()
  (esxml-with-parse-shorthands
   (let ((value (accept 'hash)))
     (when value
       (cons 'id (substring value 1))))))

(defun esxml-parse-css-class ()
  (esxml-with-parse-shorthands
   (when (accept 'period)
     (let ((value (accept 'ident)))
       (if value
           (cons 'class value)
         (error "Expected identifier after period"))))))

(defun esxml-parse-css-attrib ()
  (esxml-with-parse-shorthands
   (let (result)
     (when (accept 'lbracket)
       (eat-whitespace)
       (let ((name (esxml-parse-css-attrib-name)))
         (when (not name)
           (error "Expected attribute name"))
         (push (cons 'name name) result)
         (when (not (accept 'rbracket))
           (let ((match (esxml-parse-css-attrib-match)))
             (when (not match)
               (error "Expected attribute matcher"))
             (let ((value (esxml-parse-css-attrib-value)))
               (when (not value)
                 (error "Expected attribute value"))
               (eat-whitespace)
               (when (not (accept 'rbracket))
                 (error "Unterminated attribute"))
               (push (cons match value) result)))))
       (cons 'attribute (nreverse result))))))

(defun esxml-parse-css-attrib-name ()
  (esxml-with-parse-shorthands
   (let ((name (accept 'ident)))
     (when name
       (eat-whitespace)
       name))))

(defun esxml-parse-css-attrib-match ()
  (esxml-with-parse-shorthands
   (let (result)
     (cond
      ((accept 'equals)
       (setq result 'exact-match))
      ((accept 'prefix-match)
       (setq result 'prefix-match))
      ((accept 'suffix-match)
       (setq result 'suffix-match))
      ((accept 'substring-match)
       (setq result 'substring-match))
      ((accept 'include-match)
       (setq result 'include-match))
      ((accept 'dash-match)
       (setq result 'dash-match)))
     (eat-whitespace)
     result)))

(defun esxml-parse-css-attrib-value ()
  (esxml-with-parse-shorthands
   (let ((token (peek)))
     (cond
      ((eq (car token) 'ident)
       (next)
       (cdr token))
      ((eq (car token) 'string)
       (next)
       (substring (cdr token) 1 -1))
      (t nil)))))

(defun esxml-parse-css-pseudo ()
  (esxml-with-parse-shorthands
   (let (result type)
     (when (accept 'colon)
       (if (accept 'colon)
           (setq type 'pseudo-element)
         (setq type 'pseudo-class))
       (let ((functional (esxml-parse-css-functional-pseudo)))
         (if functional
             (if (eq type 'pseudo-class)
                 (let ((value (car functional))
                       (args (cdr functional)))
                   (push (cons 'name value) result)
                   (push (cons 'args args) result))
               (error "Pseudo-elements may not have arguments"))
           (let ((value (accept 'ident)))
             (if value
                 (push (cons 'name value) result)
               (error "Expected function or identifier")))))
       (cons type (nreverse result))))))

(defun esxml-parse-css-functional-pseudo ()
  (esxml-with-parse-shorthands
   (let ((function (accept 'function))
         result)
     (when function
       (push (substring function 0 -1) result)
       (eat-whitespace)
       (let ((expression (esxml-parse-css-expression))
             done)
         (eat-whitespace)
         (when (not expression)
           (error "Expected at least one expression for function"))
         (push expression result)
         (while (not done)
           (setq expression (esxml-parse-css-expression))
           (if expression
               (progn
                 (push expression result)
                 (eat-whitespace))
             (setq done t))))
       (when (not (accept 'rparen))
         (error "Unterminated function argument list"))
       (nreverse result)))))

(defun esxml-parse-css-expression ()
  (esxml-with-parse-shorthands
   (let ((token (peek)))
     (cond
      ((accept 'plus)
       '(operator . +))
      ((accept 'minus)
       '(operator . -))
      ((eq (car token) 'dimension)
       (next)
       (cons 'dimension (cdr token)))
      ((eq (car token) 'number)
       (next)
       (cons 'number (string-to-number (cdr token))))
      ((eq (car token) 'string)
       (next)
       (cons 'string (substring (cdr token) 1 -1)))
      ((eq (car token) 'ident)
       (next)
       (cons 'ident (cdr token)))
      (t nil)))))


;;; tree traversal

;; TODO: these helpers should be part of esxml.el
(defun esxml-branch-p (node)
  (and (listp node)
       (>= (length node) 2)
       (symbolp (car node))
       (listp (cadr node))))

(defun esxml-node-tag (node)
  (and (esxml-branch-p node)
       (car node)))

(defun esxml-node-attributes (node)
  (and (esxml-branch-p node)
       (cadr node)))

(defun esxml-node-children (node)
  (and (esxml-branch-p node)
       (nthcdr 2 node)))

(defun esxml-find-node (pred root)
  (if (funcall pred root)
      root
    (cl-some (lambda (node) (esxml-find-node pred node))
             (esxml-node-children root))))

(defun esxml-visit-nodes (function root)
  (funcall function root)
  (mapc (lambda (node) (esxml-visit-nodes function node))
        (esxml-node-children root)))

(defun esxml-find-nodes (pred root)
  (let ((acc '()))
    (esxml-visit-nodes
     (lambda (node)
       (when (funcall pred node)
         (push node acc)))
     root)
    (nreverse acc)))


;;; querying

(defun esxml--query (selector root)
  selector)

(defun esxml-query (selector root)
  (when (stringp selector)
    (setq selector (esxml-parse-css-selector selector)))
  (when (> (length selector) 1)
    (error "Comma syntax is unsupported"))
  (esxml--query (car selector) root))

(provide 'esxml-query)
;;; esxml-query.el ends here
