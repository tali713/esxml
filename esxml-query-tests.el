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
                 '((((tag . foo)))
                   (((tag . bar))))))
  (should (equal (esxml-parse-css-selector "foo ,bar")
                 '((((tag . foo)))
                   (((tag . bar))))))
  (should (equal (esxml-parse-css-selector "foo , bar")
                 '((((tag . foo)))
                   (((tag . bar))))))
  (should-error (esxml-parse-css-selector "foo, "))
  (should-error (esxml-parse-css-selector "foo,"))
  (should-error (esxml-parse-css-selector "foo ,"))
  (should-error (esxml-parse-css-selector "foo , "))
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

(defvar esxml-query-document
  (xml-to-esxml
   "<!DOCTYPE html>
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">
  <head>
    <meta charset=\"utf-8\" />
    <link rel=\"self\" />
    <title>Foobar</title>
  </head>
  <body>
    <table>
      <thead>
        <th class=\"row\" id=\"heading\">
          <td class=\"col\">Key</td>
          <td class=\"col\">Value</td>
        </th>
      </thead>
      <tbody>
        <tr class=\"row even\">
          <td class=\"col key\">Foo</td>
          <td class=\"col value\">1</td>
        </tr>
        <tr class=\"row odd\">
          <td class=\"col key\">Bar</td>
          <td class=\"col value\">2</td>
        </tr>
      </tbody>
    </table>
  </body>
</html>"))

(ert-deftest esxml-query-test ()
  (let ((root esxml-query-document))
    (should (eq (esxml-node-tag (esxml-query "*" root)) 'html))
    (should (eq (esxml-node-tag (esxml-query "table" root)) 'table))
    (should-not (esxml-query "foo" root))
    (should (eq (esxml-node-tag (esxml-query "table, foo" root)) 'table))
    (should (eq (esxml-node-tag (esxml-query "foo, table" root)) 'table))
    (should-not (esxml-query "foo, bar" root))
    (should (eq (esxml-node-tag (esxml-query "tbody, thead" root)) 'tbody))

    (should-not (esxml-query "table table" root))
    (should (equal (esxml-node-children (esxml-query "table thead td" root))
                   '("Key")))
    (should (equal (esxml-node-children (esxml-query "table td" root))
                   '("Key")))
    (should (equal (esxml-node-children (esxml-query "table * td" root))
                   '("Key")))
    (should-not (esxml-query "td foo" root))
    (should-not (esxml-query "tr foo td" root))
    (should (equal (esxml-node-children (esxml-query "tbody>tr>td" root))
                   '("Foo")))
    (should-not (esxml-query "tr>foo" root))
    (should-not (esxml-query "foo>td" root))

    (should (equal (esxml-node-children (esxml-query "#heading>td" root))
                   '("Key")))
    (should (equal (esxml-node-tag (esxml-query "th#heading" root)) 'th))
    (should (equal (esxml-node-tag (esxml-query "#heading" root)) 'th))
    (should-not (esxml-query "tr#heading" root))
    (should-not (esxml-query "th #heading" root))

    (should (equal (esxml-node-children (esxml-query ".row>td" root))
                   '("Key")))
    (should-not (esxml-query ".foo" root))
    (should (esxml-query ".row.even" root))
    (should-not (esxml-query ".row.foo" root))
    (should (equal (esxml-node-children (esxml-query ".row .value" root))
                   '("1")))
    (should (equal (esxml-node-children (esxml-query ".row.odd .value" root))
                   '("2")))
    (should (eq (esxml-node-tag (esxml-query ".even, .odd" root)) 'tr))
    ))

(ert-deftest esxml-query-all-test ()
  (let ((root esxml-query-document))
    (should (equal (cl-remove-if 'not (mapcar 'esxml-node-tag
                                              (esxml-query-all "*" root)))
                   '(html head meta link title
                          body table thead th td td tbody tr td td tr td td)))
    (should (equal (mapcar 'esxml-node-tag (esxml-query-all "table" root))
                   '(table)))
    (should-not (esxml-query-all "foo" root))
    (should (equal (mapcar 'esxml-node-tag (esxml-query-all "table, foo" root))
                   '(table)))
    (should (equal (mapcar 'esxml-node-tag (esxml-query-all "foo, table" root))
                   '(table)))
    (should-not (esxml-query-all "foo, bar" root))
    (should (equal (mapcar 'esxml-node-tag (esxml-query-all "tbody, thead" root))
                   '(tbody thead)))

    (should-not (esxml-query-all "table table" root))
    (should (equal (mapcar 'car (mapcar 'esxml-node-children (esxml-query-all "table thead td" root)))
                   '("Key" "Value")))
    ;; FIXME: this returns duplicates
    ;; NOTE: you could solve this by decorating the tree with IDs,
    ;; fetching results, using cl-delete-duplicate with :key, then
    ;; undecorating the results
    ;; NOTE: alternatively, keep a list of IDs while traversing the
    ;; decorated tree and don't traverse from already seen nodes again
    ;; (should (equal (mapcar 'car (mapcar 'esxml-node-children (esxml-query-all "table * td" root)))
    ;;                '("Key" "Value" "Foo" "1" "Bar" "2")))
    (should (equal (mapcar 'car (mapcar 'esxml-node-children (esxml-query-all "table td" root)))
                   '("Key" "Value" "Foo" "1" "Bar" "2")))
    (should-not (esxml-query-all "td foo" root))
    (should-not (esxml-query-all "tr foo td" root))
    (should (equal (mapcar 'car (mapcar 'esxml-node-children (esxml-query-all "tbody>tr>td" root)))
                   '("Foo" "1" "Bar" "2")))
    (should-not (esxml-query-all "tr>foo" root))
    (should-not (esxml-query-all "foo>td" root))

    (should (equal (mapcar 'car (mapcar 'esxml-node-children (esxml-query-all "#heading>td" root)))
                   '("Key" "Value")))
    (should (equal (mapcar 'esxml-node-tag (esxml-query-all "th#heading" root))
                   '(th)))
    (should (equal (mapcar 'esxml-node-tag (esxml-query-all "#heading" root))
                   '(th)))
    (should-not (esxml-query-all "tr#heading" root))
    (should-not (esxml-query-all "th #heading" root))

    (should (equal (mapcar 'car (mapcar 'esxml-node-children (esxml-query-all ".row>td" root)))
                   '("Key" "Value" "Foo" "1" "Bar" "2")))
    (should-not (esxml-query-all ".foo" root))
    (should (= (length (esxml-query-all ".row.even" root)) 1))
    (should-not (esxml-query-all ".row.foo" root))
    (should (equal (mapcar 'car (mapcar 'esxml-node-children (esxml-query-all ".row .value" root)))
                   '("1" "2")))
    (should (equal (mapcar 'car (mapcar 'esxml-node-children (esxml-query-all ".row.odd .value" root)))
                   '("2")))
    (should (= (length (esxml-query-all ".even, .odd" root)) 2))
    ))
