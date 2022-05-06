(require 'esxml)
(require 'ert)

(ert-deftest esxml-raw-string ()
  (should (equal (esxml-to-xml "foo") "foo"))
  (should (equal (esxml-to-xml "<br>") "&lt;br&gt;"))
  (should (equal (esxml-to-xml '(raw-string "<br>")) "<br>"))
  (should (equal (esxml-to-xml '(p nil "<br>")) "<p>&lt;br&gt;</p>"))
  (should (equal (esxml-to-xml '(p nil (raw-string "<br>"))) "<p><br></p>")))
