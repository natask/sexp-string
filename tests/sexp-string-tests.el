;;; sexp-string-tests.el --- Tests for sexp-string.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Natnael Kahssay
;;
;; Author: Natnael Kahssay <https://github.com/natask>
;; Maintainer: Natnael Kahssay <thisnkk@gmail.com>
;; Created: December 06, 2021
;; Modified: December 06, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/natask/sexp-string-tests
;; Package-Requires: ((emacs "24.3") (buttercup))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for sexp-string.el
;;
;;; Code:
(require 'buttercup)
(require 'sexp-string)

(describe "sexp-string--predicate-names"
  (it "handles no :name"
    (let* ((predicates '((or  :name or)
                         (and :name and)
                         (not)
                         (second :name second :aliases (2nd))
                         (first :name first :aliases (1st)))))
      (expect (sexp-string--predicate-names predicates)
              :to-have-same-items-as
              '("or" "and" "not" "second" "2nd" "first" "1st"))
      )))

(describe "sexp-string--replace-alias-with-name"
  (it "replaces alias with corrosponding name"
    (let* ((predicates '((or  :name or)
                         (and :name and)
                         (not)
                         (second :name second :aliases (2nd))
                         (first :name first :aliases (1st)))))
      (expect (sexp-string--replace-alias-with-name predicates "2nd")
              :to-equal
              "second")))
  (it "doesn't touch names"
    (let* ((predicates '((or  :name or)
                         (and :name and)
                         (not)
                         (second :name second :aliases (2nd))
                         (first :name first :aliases (1st)))))
      (expect (sexp-string--replace-alias-with-name predicates "first")
              :to-equal
              "first"))))

(describe "sexp-string-query-string-to-sexp"
  (describe "custom pex"
    (let* ((predicates '((or  :name or)
                         (and :name and)
                         (not :name not)
                         (second :name second :aliases (2nd))
                         (first :name first :aliases (1st))))
           (default-predicate 'first)
           (default-boolean 'and)
           (args (list :predicates predicates :default-predicate default-predicate :default-boolean default-boolean)))
      (it "works"
        (expect (apply 'sexp-string--query-string-to-sexp (append '(:input "hello") args))
                :to-equal
                '(first "hello")))
      (it "handles empty string"
        (expect (apply 'sexp-string--query-string-to-sexp (append '(:input "") args))
                :to-equal
                'nil))
      (it "handles negation"
        (expect (apply 'sexp-string--query-string-to-sexp (append '(:input "!first:world") args))
                :to-equal
                '(not (first "world"))))
      (it "complex boolean"
        (expect (apply 'sexp-string--query-string-to-sexp (append '(:input "`(hello `or `(2nd:there,hello,\"man with two feet\" !1st:aka,friend`)`) this !right here \"verbatum 2nd:there\" (what, is. or `and going and on?)") args))
                :to-have-same-items-as
                '(and (or (first "hello") (and (second "there" "hello" "man with two feet") (not (first "aka" "friend"))))
                      (first "this") (not (first "right")) (first "here") (first "verbatum 2nd:there")
                      (first "(" "what" ",") (first "is" ".") (first "or") (first "going") (first "and") (first "on" "?" ")")))))
    ;; TODO: not implemented yet
    ;; (it "handles new line"
    ;;   (expect (apply 'sexp-string--query-string-to-sexp (append '(:input "well man\nsucks") args))
    ;;           :to-equal
    ;;           '(and (first "well") (first "man") (first "sucks"))))
    ;; TODO: not implemented yet
    ;; (it "boolean within a predicate"
    ;;   (expect (apply 'sexp-string--query-string-to-sexp (append '(:input "`(2nd:there/here,fellow/wondering or !1st:friend/man`) this") args))
    ;;           :to-equal
    ;;           '(and (or (second (and (or (and (or "there" "here") "fellow") "wondering"))) (first (or "friend" "man"))) (first "this"))))
    ;; TODO: not implemented yet
    ;; (it "braces within a predicate"
    ;;   (expect (apply 'sexp-string--query-string-to-sexp (append '(:input "(2nd:`(there/here),(fellow/wondering) -1st:friend/man)) this -right here") args))
    ;;           :to-equal
    ;;           '(and (or (first "hello") (and (second (and (or "there" "here") (or "fellow wondering"))) (not (first (or "friend" "man")))) (first "this") (not (first "right")) (first "here")))))
    )
  (describe "recoll pex"
    (let* ((predicates '((or  :name or)
                         (and :name and)
                         (not :name not)
                         (second :name second :aliases (2nd))
                         (first :name first :aliases (1st))))
           (default-predicate 'first)
           (default-boolean 'and)
           (pex-function 'sexp-string--recoll-pexs)
           (args (list :predicates predicates :default-predicate default-predicate :default-boolean default-boolean :pex-function pex-function)))
      (it "works"
        (expect (apply 'sexp-string--query-string-to-sexp (append '(:input "hello") args))
                :to-equal
                '(first "hello")))
      (it "handles empty string"
        (expect (apply 'sexp-string--query-string-to-sexp (append '(:input "") args))
                :to-equal
                'nil))

      (it "handles negation"
        (expect (apply 'sexp-string--query-string-to-sexp (append '(:input "-first:world") args))
                :to-equal
                '(not (first "world"))))
      (it "complex boolean"
        (expect (apply 'sexp-string--query-string-to-sexp (append '(:input "(hello OR (2nd:there/here,fellow/wondering -1st:friend/\"man or woman\")) \"verbatim 2nd:there\" this -right here") args))
                :to-equal
                '(and (or (first "hello") (and (second (or (and (or "there" "here") "fellow") "wondering")) (not (first (or "friend" "man or woman"))))) (first "verbatim 2nd:there") (first "this") (not (first "right")) (first "here"))))
      ;; TODO: not implemented yet
      ;; (it "handles new line"
      ;;   (expect (apply 'sexp-string--query-string-to-sexp (append '(:input "well man\nsucks") args))
      ;;           :to-equal
      ;;           '(and (first "well") (first "man") (first "sucks"))))
      ;; TODO: not implemented yet
      ;; (it "braces within a predicate"
      ;;   (expect (apply 'sexp-string--query-string-to-sexp (append '(:input "(hello OR (2nd:(there/here),(fellow/wondering) -1st:friend/man)) this -right here") args))
      ;;           :to-equal
      ;;           '(and (or (first "hello") (and (second (and (or "there" "here") (or "fellow wondering"))) (not (first (or "friend" "man")))) (first "this") (not (first "right")) (first "here")))))
      )))

(provide 'sexp-string-tests)
;;; sexp-string-tests.el ends here
