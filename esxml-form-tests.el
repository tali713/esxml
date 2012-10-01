;; tests for field-set
(require 'kv)
(require 'db)
(require 'uuid)

(defmacro esxml-form-field-set--test-defaults (&rest body)
  (declare (debug (&rest form)))
  `(let ((db-hash-do-not-save t))
     (let* ((swarmweb--user-db
             (db-make
              `(db-hash
                :filename ,(format "/tmp/swarm-user-db-%s" (uuid-string))))))
       (let* ((fs
               (esxml-form-field-set
                (:db swarmweb--user-db)
                (username
                 :regex "[A-Za-z]+"
                 :db-key t
                 :db-check '(= name $))
                (key
                 :regex "[A-Za-z0-9=]+"
                 :html :textarea))))
         ,@body))))

(ert-deftest esxml-form-field-set--structure ()
  (esxml-form-field-set--test-defaults
   (should (listp (esxml-form-field-set-fields fs)))
   (should (equal '(username key)
                  (kvalist->keys (esxml-form-field-set-fields fs))))
   (should (listp (aget (esxml-form-field-set-fields fs) 'username)))))

(ert-deftest esxml-form-field-set--validity-check ()
  (esxml-form-field-set--test-defaults
   (let* ((fields (esxml-form-field-set-fields fs))
          (username-field (aget fields 'username)))
     (should (esxml-form-field-set--validity-check username-field "NicFerrier"))
     (should-not
      (esxml-form-field-set--validity-check username-field "!NicFerrier")))))

(ert-deftest esxml-form-field-set-check ()
  (esxml-form-field-set--test-defaults
   (let ((params '(("username" . "nic")
                   ("key" . "ssh-dss sadwqqwdqdqd=")))) ; these match the fields
     (should-not (esxml-form-field-set-check fs params))
     (setq params '(("username" . "!nic")
                    ("key" . "ssh-dss sadwqqwdqdqd=")))
     (should
      (equal
       '((username "!nic" "the content of the field was wrong"))
       (esxml-form-field-set-check fs params))))))

(ert-deftest esxml-form-field-set->esxml ()
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
     (esxml-form-field-set->esxml fs)))
   ;; Test it all renders properly
   (should
    (equal
     (concat
      "<fieldset>"
      "<label>username: <input name=\"username\" type=\"text\"/></label>"
      "<label>key: <textarea name=\"key\"/></label>"
      "</fieldset>")
     (esxml-to-xml (esxml-form-field-set->esxml fs))))
   ;; Test the structure with values
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
     (esxml-form-field-set->esxml fs '(("username" . "test")))))))

;; ends
