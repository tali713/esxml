;; tests for field-set
(require 'kv)
(require 'db)
(require 'uuid)

(defvar my-test-db nil
  "Test database variable.")

(defmacro esxml-form-field-set--test-defaults (&rest body)
  (declare (debug (&rest form)))
  `(let ((db-hash-do-not-save t))
     (let* ((uuid-str (uuid-string))
            (my-test-db
             (db-make
              `(db-hash :filename
                        ,(format "/tmp/esxml-form-test-db-%s" uuid-str))))
            (fs
             (esxml-form
              (:db my-test-db :db-key "username")
              (username
               :regex "[A-Za-z]+"
               :db-key t
               :db-check (= "name" $))
              (key
               :regex "[A-Za-z0-9=]+"
               :html :textarea))))
       ,@body)))

(ert-deftest esxml-form-structure ()
  (esxml-form-field-set--test-defaults
   (should (listp (esxml-form-fields fs)))
   (should (equal '(username key)
                  (kvalist->keys (esxml-form-fields fs))))
   (should (listp (aget (esxml-form-fields fs) 'username)))))

(ert-deftest esxml--field-check ()
  (esxml-form-field-set--test-defaults
   (let* ((fields (esxml-form-fields fs))
          (username-field (aget fields 'username)))
     (should (esxml--field-check username-field "NicFerrier"))
     (should-not
      (esxml--field-check username-field "!NicFerrier")))))

(ert-deftest esxml-field-set-check ()
  (esxml-form-field-set--test-defaults
   (let ((params '(("username" . "nic") ; these match the fields
                   ("key" . "ssh-dss sadwqqwdqdqd="))))
     (let (my-test-db) ; don't do db validation
       (should-not (esxml-field-set-check fs params))
       ;; Now with an invalid field
       (let ((params '(("username" . "!nic")
                       ("key" . "ssh-dss sadwqqwdqdqd="))))
         (should
          (equal
           '((username "!nic" "the content of the field was wrong"))
           (esxml-field-set-check fs params)))))
     ;; Now do one with db-validation
     (should-not (esxml-field-set-check fs params))
     ;; And now add a row first and then do it
     (db-put "001" '((name . "nic")
                     (key . "311ndknd1")) my-test-db)
     (should (esxml-field-set-check fs params)))))

(ert-deftest esxml-form-test-db ()
  "Test that the db is there."
  (esxml-form-field-set--test-defaults
   (should my-test-db)))

(ert-deftest esxml-field-set->esxml ()
  (esxml-form-field-set--test-defaults
   ;; Test that the HTML is constructed properly.
   (should
    (equal
     '(fieldset ()
       (label ()
        "username: "
        (input ((name . "username")
                (type . "text"))))
       (label ()
        "key: "
        (textarea ((name . "key")))))
     (esxml-field-set->esxml fs)))
   ;; Test it all renders properly
   (should
    (equal
     (concat
      "<fieldset>"
      "<label>username: <input name=\"username\" type=\"text\"/></label>"
      "<label>key: <textarea name=\"key\"/></label>"
      "</fieldset>")
     (esxml-to-xml (esxml-field-set->esxml fs))))
   ;; Test the structure with values
   (let* (my-test-db ; bind the db to nothing
          (params '(("username" . "test")))
          (errors (esxml-field-set-check fs params)))
     (should
      (equal
       '(fieldset ()
         (label ()
          "username: "
          (input ((name . "username")
                  (type . "text")
                  (value . "test"))))
         (label ()
          "key: "
          (textarea ((name . "key")))
          (div ((class . "error")) "the content of the field was wrong")))
       (esxml-field-set->esxml fs params errors))))))

(ert-deftest esxml-form-save ()
  (esxml-form-field-set--test-defaults
   (let* ((params '(("username" . "test001")
                    ("key" . "wadkwqdnwdNJNSJNJSw")))
          (errors (esxml-field-set-check fs params)))
     (unless errors (esxml-form-save fs params))
     (let ((value (db-get "test001" my-test-db)))
       (should value)))))


;; ends
