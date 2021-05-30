;; -*- lexical-binding: t -*-

(require 'sarso)

(describe "Environment case conversion"
  (it "works"
    (expect (kebab-case-to-env-case "name") :to-equal "NAME")
    (expect (kebab-case-to-env-case "t--e") :to-equal "T__E")
    (expect (kebab-case-to-env-case "") :to-equal "")
    (expect (kebab-case-to-env-case "first-last-third") :to-equal "FIRST_LAST_THIRD")))
