;; -*- lexical-binding: t -*-

(require 'sarso)

(describe "Environment case conversion"
  (it "works"
    (expect (kebab-case-to-env-case "name") :to-equal "NAME")
    (expect (kebab-case-to-env-case "t--e") :to-equal "T__E")
    (expect (kebab-case-to-env-case "") :to-equal "")
    (expect (kebab-case-to-env-case "first-last-third") :to-equal "FIRST_LAST_THIRD")))

(describe "with-env"
  (before-each
    (setenv "SARSO_TEST_VAR" "1"))
  (after-each
    (setenv "SARSO_TEST_VAR"))

  (it "resets"
    (with-env ((sarso-test-var "2")))
    (expect (getenv "SARSO_TEST_VAR") :to-equal "1"))

  (it "works"
    (with-env ((sarso-test-var "2")) (expect (getenv "SARSO_TEST_VAR") :to-equal "2"))))
