(require 'esxml)
(require 'esxml-query)
(require 'ert)

(ert-deftest esxml-parse-css-selector-test ()
  (should-error (esxml-parse-css-selector ""))
  (should-error (esxml-parse-css-selector "'foo'"))
  (should (equal (esxml-parse-css-selector "*")
                 '((((wildcard))))))
  (should (equal (esxml-parse-css-selector "foo")
                 '((((tag . foo))))))
  (should-error (esxml-parse-css-selector "foo 123"))
  (should (equal (esxml-parse-css-selector "foo bar")
                 '((((tag . foo))
                    ((combinator . descendant))
                    ((tag . bar))))))
  (should (equal (esxml-parse-css-selector "foo,bar")
                 '((((tag . foo)))
                   (((tag . bar))))))
  (should (equal (esxml-parse-css-selector "foo, bar")
  (should-error (esxml-parse-css-selector "foo ,bar"))
                 '((((tag . foo)))
                   (((tag . bar))))))
                 '((((tag . foo)))
                   (((tag . bar))))))
  (should-error (esxml-parse-css-selector "foo, "))
  (should (equal (esxml-parse-css-selector "foo > bar")
                 '((((tag . foo))
                    ((combinator . child))
                    ((tag . bar))))))
  (should (equal (esxml-parse-css-selector "foo>bar")
                 '((((tag . foo))
                    ((combinator . child))
                    ((tag . bar))))))
  (should (equal (esxml-parse-css-selector "foo ~ bar")
                 '((((tag . foo))
                    ((combinator . indirect-sibling))
                    ((tag . bar))))))
  (should (equal (esxml-parse-css-selector "foo + bar")
                 '((((tag . foo))
                    ((combinator . direct-sibling))
                    ((tag . bar))))))
  (should-error (esxml-parse-css-selector "foo >"))
  (should (equal (esxml-parse-css-selector "foo#bar.baz.qux")
                 '((((tag . foo)
                     (id . "bar")
                     (class . "baz")
                     (class . "qux"))))))
  (should (equal (esxml-parse-css-selector "#foo.bar.baz[qux=quux]:foo(bar baz)")
                 '((((id . "foo")
                     (class . "bar")
                     (class . "baz")
                     (attribute
                      (name . "qux")
                      (exact-match . "quux"))
                     (pseudo-class
                      (name . "foo")
                      (args
                       (ident . "bar")
                       (ident . "baz"))))))))
  (should-error (esxml-parse-css-selector "foo#bar#baz"))
  (should (equal (esxml-parse-css-selector "foo[bar=baz][qux=quux]")
                 '((((tag . foo)
                     (attribute
                      (name . "bar")
                      (exact-match . "baz"))
                     (attribute
                      (name . "qux")
                      (exact-match . "quux")))))))
  (should (equal (esxml-parse-css-selector "foo::bar:baz(qux quux)")
                 '((((tag . foo)
                     (pseudo-element
                      (name . "bar"))
                     (pseudo-class
                      (name . "baz")
                      (args
                       (ident . "qux")
                       (ident . "quux"))))))))
  (should (equal (esxml-parse-css-selector "#foo")
                 '((((id . "foo"))))))
  (should-error (esxml-parse-css-selector "# #bar"))
  (should (equal (esxml-parse-css-selector ".foo")
                 '((((class . "foo"))))))
  (should-error (esxml-parse-css-selector ". .bar"))
  (should (equal (esxml-parse-css-selector "[foo]")
                 '((((attribute
                      (name . "foo")))))))
  (should-error (esxml-parse-css-selector "[foo=]"))
  (should (equal (esxml-parse-css-selector "[foo=bar]")
                 '((((attribute
                      (name . "foo")
                      (exact-match . "bar")))))))
  (should (equal (esxml-parse-css-selector "[foo^=bar]")
                 '((((attribute
                      (name . "foo")
                      (prefix-match . "bar")))))))
  (should (equal (esxml-parse-css-selector "[foo$=bar]")
                 '((((attribute
                      (name . "foo")
                      (suffix-match . "bar")))))))
  (should (equal (esxml-parse-css-selector "[foo*=bar]")
                 '((((attribute
                      (name . "foo")
                      (substring-match . "bar")))))))
  (should (equal (esxml-parse-css-selector "[foo~=bar]")
                 '((((attribute
                      (name . "foo")
                      (include-match . "bar")))))))
  (should (equal (esxml-parse-css-selector "[foo|=bar]")
                 '((((attribute
                      (name . "foo")
                      (dash-match . "bar")))))))
  (should-error (esxml-parse-css-selector "[foo=bar"))
  (should (equal (esxml-parse-css-selector "[foo='bar']")
                 '((((attribute
                      (name . "foo")
                      (exact-match . "bar")))))))
  (should (equal (esxml-parse-css-selector "[foo=\"bar\"]")
                 '((((attribute
                      (name . "foo")
                      (exact-match . "bar")))))))
  (should-error (esxml-parse-css-selector "[foo='bar]"))
  (should (equal (esxml-parse-css-selector "[ foo = bar ]")
                 '((((attribute
                      (name . "foo")
                      (exact-match . "bar")))))))
  (should (equal (esxml-parse-css-selector ":foo")
                 '((((pseudo-class
                      (name . "foo")))))))
  (should (equal (esxml-parse-css-selector "::foo")
                 '((((pseudo-element
                      (name . "foo")))))))
  (should-error (esxml-parse-css-selector ": foo"))
  (should-error (esxml-parse-css-selector ":: foo"))
  (should-error (esxml-parse-css-selector ":foo()"))
  (should (equal (esxml-parse-css-selector ":foo(bar)")
                 '((((pseudo-class
                      (name . "foo")
                      (args
                       (ident . "bar"))))))))
  (should-error (esxml-parse-css-selector "::foo(bar)"))
  (should (equal (esxml-parse-css-selector ":foo(bar baz)")
                 '((((pseudo-class
                      (name . "foo")
                      (args
                       (ident . "bar")
                       (ident . "baz"))))))))
  (should (equal (esxml-parse-css-selector ":foo(1)")
                 '((((pseudo-class
                      (name . "foo")
                      (args
                       (number . 1))))))))
  (should (equal (esxml-parse-css-selector ":foo(42rem)")
                 '((((pseudo-class
                      (name . "foo")
                      (args
                       (dimension . "42rem"))))))))
  (should (equal (esxml-parse-css-selector ":foo(2n+1)")
                 '((((pseudo-class
                      (name . "foo")
                      (args
                       (number . 2)
                       (ident . "n")
                       (operator . +)
                       (number . 1))))))))
  (should (equal (esxml-parse-css-selector ":foo(1-1)")
                 '((((pseudo-class
                      (name . "foo")
                      (args
                       (number . 1)
                       (operator . -)
                       (number . 1))))))))
  (should-error (esxml-parse-css-selector ":foo(bar")))

(defvar my-document
  "<!DOCTYPE html>
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">
  <head>
    <meta charset=\"utf-8\" />
    <link rel=\"self\" />
    <title>Foobar</title>
  </head>
  <body>
    <table>
      <tr class=\"even\">
        <td class=\"key\">Foo</td>
        <td class=\"value\">1</td>
      </tr>
      <tr class=\"odd\">
        <td class=\"key\">Bar</td>
        <td class=\"value\">2</td>
      </tr>
    </table>
  </body>
</html>")

(defvar my-tree (xml-to-esxml my-document))

(esxml-find-node
 (lambda (node)
   (and (eq (esxml-node-tag node) 'tr)
        (equal (cdr (assoc 'class (esxml-node-attributes node)))
               "even")))
 my-tree)

(esxml-find-nodes (lambda (node) (eq (esxml-node-tag node) 'tr)) my-tree)
