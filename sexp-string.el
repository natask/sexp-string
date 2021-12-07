;;; sexp-string.el --- One to one corrospondence between boolean sexp and string -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Natnael Kahssay, Adam Porter
;;
;; Authors: Natnael Kahssay <https://github.com/natask>, Adam Porter <https://github.com/alphapapa/>
;; Maintainer: Natnael Kahssay <thisnkk@gmail.com>
;; Created: August 26, 2021
;; Modified: August 26, 2021
;; Version: 0.0.1
;; Keywords: tools matching alternate-syntax
;; Homepage: https://github.com/savnkk/sexp-string
;; Package-Requires: ((emacs "25.1") (peg) (dash))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  One to one corrospondence between boolean sexp and string
;;
;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; req;
(require 'dash)
(require 'cl-lib)
(require 'subr-x)
(require 'peg)

;;; Code:
(defun sexp-string--predicate-names (predicates)
  "Get predicate names for PREDICATES."
  (let* ((names (--map (symbol-name (or (plist-get (cdr it) :name) (car it)))
                       predicates))
         (aliases (->> predicates
                       (--map (plist-get (cdr it) :aliases))
                       -non-nil
                       -flatten
                       (-map #'symbol-name)))
         (predicate-names (->> (append names aliases)
                               -uniq
                               ;; Sort the keywords longest-first to work around what seems to be an
                               ;; obscure bug in `peg': when one keyword is a substring of another,
                               ;; and the shorter one is listed first, the shorter one fails to match.
                               (-sort (-on #'> #'length)))))
    predicate-names))

(defun sexp-string-collapse-list (lt)
"Recursively collapse list starting with LT.
If first of inner list is `equal' to first element of outer list.
Assumes that car of LT is an operator: one of `and' or `or'.
E.g.
\(and (and v1 v2)
     (or v3 v4)
     (and (or v5
              (and v6 v7))
           v8))

\(and v1 v2
     (or v3 v4)
     (or v5
        (and v6 v7))
     v8)"
 (sexp-string--collapse-list lt (car lt)))

(defun sexp-string--collapse-list (lt op)
  "Helper function of `sexp-string-collapse-list' where LT is current list and OP is first element of outer list."
  (mapcan (lambda (val) (pcase val
            (`(,(and op2 (guard (equal op op2))) . ,rest) (sexp-string--collapse-list rest op))
            ((pred listp) (list (if (member (car val) '(and or))
                                        (sexp-string--collapse-list val (car val))
                                    (sexp-string--collapse-list val 'nil))))
            (_ (list val))))
           lt))

(defun sexp-string--recoll-pexs (predicates)
  "Implementation of recoll's search syntax on PREDICATES."
 `((query sum (opt eol))
                 (sum value  (* (or (and _ and-op _ value `(a b -- (list 'and a b)))
                                    (and _ or-op _ value `(a b -- (list 'or a b)))
                                    (and _ value `(a b -- (list default-boolean a b))))))
                 (value
                  (or (and "(" (opt _) sum (opt _) ")")
                      term))
                 (term (or (and negation (list positive-term)
                                ;; This is a bit confusing, but it seems to work.  There's probably a better way.
                                `(pred -- (list 'not (car pred))))
                           positive-term))
                 (positive-term (or
                                    (and predicate-with-args `(pred args -- (cons (intern pred) args)))
                                    (and predicate-without-args `(pred -- (list (intern pred))))
                                    (and plain-string `(s -- (list (intern (sexp-string--replace-alias-with-name predicates (symbol-name default-predicate))) s)))))
                 (predicate-with-args (and predicate-shown ":" args))
                 (predicate-without-args (and predicate-shown ":"))
                 (predicate-shown (and (substring predicate) `(pred -- (sexp-string--replace-alias-with-name predicates pred))))
                 (predicate (or ,@(sexp-string--predicate-names (reverse predicates))))
                 (args (list arg (* (or (and and-separator arg `(a b -- (list 'and a b)))
                                  (and or-separator arg `(a b -- (list 'or a b)))))))
                 (arg (or keyword-arg quoted-arg unquoted-arg))
                 (keyword-arg (and keyword "=" `(kw -- (intern (concat ":" kw)))))
                 (keyword (substring (+ (not (or brackets separator "=" "\"" (syntax-class whitespace))) (any))))
                 (quoted-arg "\"" (substring (+ (not "\"") (any))) "\"")
                 (unquoted-arg (substring (+ (not (or brackets separator "\"" (syntax-class whitespace))) (any))))
                 (plain-string (or unquoted-arg quoted-arg separator))
                 (operator (or and-op or-op))
                 (separator (or or-separator and-separator))
                 (negation "-")
                 (or-separator "/")
                 (and-separator ",")
                 (and-op "AND")
                 (or-op "OR")
                 (brackets (or "(" ")"))
                 (_ (+ [" \t"]))
                 (eol (or  "\n" "\r\n" "\r"))))

(defun sexp-string--custom-pexs (predicates)
  "Custom pexs that focuses on plain entries based on PREDICATES.
It requires substituting `default-boolean' and `default-predicate' in pex.

It has the least cognative load when looking to enter a plain string query
that potentially conflicts with pex rules.

For example, the following entry

\"this is a plain entry with (many types) of !symbols
that would, when parsed normally, conflict with pex rules\"

Is interpreted verbatim."
 `((query sum (opt eol))
                 (sum value  (* (or (and _ and-op _ value `(a b -- (list 'and a b)))
                                    (and _ or-op _ value `(a b -- (list 'or a b)))
                                    (and _ value `(a b -- (list default-boolean a b))))))
                 (value
                  (or (and "`(" (opt _) sum (opt _) "`)")
                      term))
                 (term (or (and negation (list positive-term)
                                ;; This is a bit confusing, but it seems to work.  There's probably a better way.
                                `(pred -- (list 'not (car pred))))
                           positive-term))
                 (positive-term (or
                                    (and predicate-with-args `(pred args -- (cons (intern pred) args)))
                                    (and predicate-without-args `(pred -- (list (intern pred))))
                                    (and plain-string `(s -- (cons (intern (sexp-string--replace-alias-with-name predicates (symbol-name default-predicate))) s)))))
                 (predicate-with-args (and predicate-shown ":" args))
                 (predicate-without-args (and predicate-shown ":"))
                 (predicate-shown (and (substring predicate) `(pred -- (sexp-string--replace-alias-with-name predicates pred))))
                 (predicate (or ,@(sexp-string--predicate-names (reverse predicates))))
                 (args (list (+ (and (or keyword-arg quoted-arg unquoted-arg) (opt separator)))))
                 (keyword-arg (and keyword "=" `(kw -- (intern (concat ":" kw)))))
                 (keyword (substring (+ (not (or brackets brackets2 separator "=" "\"" (syntax-class whitespace))) (any))))
                 (quoted-arg "\"" (substring (+ (not "\"") (any))) "\"")
                 (unquoted-arg (substring (+ (not (or brackets brackets2 separator "\"" (syntax-class whitespace))) (any))))
                 (limited-unquoted-arg (substring (+ (not (or brackets brackets2 separator symbols negation "\"" (syntax-class whitespace))) (any))))
                 (plain-string (list (+ (and (or (substring separator) (substring negation) (substring symbols) (substring brackets) limited-unquoted-arg quoted-arg) (not operator brackets2)))))

                 (negation "!")
                 (symbols (or "?" "."))
                 (separator "," )
                 (operator (or and-op or-op))
                 (and-op "`and")
                 (or-op "`or")
                 (operator (or "`and" "`or"))
                 (brackets (or "(" ")"))
                 (brackets2 (or "`(" "`)"))
                 (_ (+ [" \t"]))
                 (eol (or  "\n" "\r\n" "\r"))))

(cl-defun sexp-string--query-string-to-sexp (&key input predicates default-predicate default-boolean pex-function)
  "Parse string INPUT based upon PREDICATES.

PREDICATES        -> list of predicates
DEFAULT-PREDICATE -> default predicate
DEFAULT-BOOLEAN   -> default boolean
PEX-FUNCTION      -> function to return pexs template with which to parse INPUT

Borrowed from `org-ql'."
  (unless (string-empty-p input)
    (when-let* ((default-boolean (or default-boolean 'and))
                (predicates predicates)
                (default-predicate (or default-predicate (car (-last-item predicates))))
                (pex-function (or pex-function 'sexp-string--custom-pexs))
                (pexs (funcall pex-function predicates))
                (parsed-sexp
                 (with-temp-buffer
                   (insert input)
                   (goto-char (point-min))
                   ;; Copied from `peg-parse'.  There is no function in `peg' that
                   ;; returns a matcher function--every entry point is a macro,
                   ;; which means that, since we define our PEG rules at runtime when
                   ;; predicate-names are defined, we either have to use `eval', or we
                   ;; have to borrow some code.  It ends up that we only have to
                   ;; borrow this `with-peg-rules' call, which isn't too bad.
                   (eval `(with-peg-rules ,pexs
                            (peg-run (peg ,(caar pexs)) #'peg-signal-failure)) (list (cons 'predicates predicates) (cons 'default-boolean default-boolean) (cons 'default-predicate default-predicate))))))
      (pcase parsed-sexp
        (`(,_) (->> (car parsed-sexp)
                    sexp-string-collapse-list))
        (_ nil)))))

(defun sexp-string--replace-alias-with-name (predicates predicate)
  "Replace an :aliases PREDICATE with its :name counterpart based on PREDICATES.
In the future can do on directly on the input string."
  (let* ((names (--map (symbol-name (or (plist-get (cdr it) :name) (car it)))
                       predicates))
         (aliases-map (->> predicates
                       (--map (cons (or (plist-get (cdr it) :name) (car it)) (plist-get (cdr it) :aliases)))
                       (--filter (and (car it) (cdr it)))
                       (--map (cons (symbol-name (car it)) (-map #'symbol-name (cdr it)))))))
    (if (--find (equal predicate it) names)
        predicate
    (car (--find (--find (equal predicate it) (cdr it)) aliases-map)))))

(cl-defun sexp-string--transform-query (&key query predicates type ignore)
  "Return transformed form of QUERY based on PREDICATES against TYPE.
QUERY is the output of `sexp-string--string-to-sexp'.
PREDICATES list of predicates.
TYPE is as a plist key defined for each predicate in PREDICATES.
If IGNORE, ignore elements of query that don't have TYPE defined in PREDICATE.
Borrowed from `org-ql'."
  (let ((transformer-patterns (->> predicates
                                   (--map (plist-get (cdr it) type))
                                   (-flatten-n 1)))
        (query (if ignore
                   (let ((filter-predicates (->> predicates
                                                 (--filter (plist-get (cdr it) type))
                                                 (--map (or (plist-get (cdr it) :name) (car it))))))
                   (sexp-string--filter-predicates query filter-predicates))
                 query)))
    (->> (eval
          `(cl-labels ((rec (element &optional accum)
                            (ignore accum)
                            (pcase element
                              ,@transformer-patterns
                              (_ (error "Element didn't match transformer: %S" element)))))
             (rec query)) (list (cons 'transformer-patterns transformer-patterns) (cons 'query query)))
         (sexp-string-collapse-list))))

(defun sexp-string--filter-predicates (query filter-predicates)
  "Filter QUERY by FILTER-PREDICATES.
PREDICATES not within FILTER-PREDICATES are filtered out from QUERY."
  (car (sexp-string--filter-predicates-helper (list query) filter-predicates)))

(defun sexp-string--filter-predicates-helper (query filter-predicates)
  "Filter QUERY by FILTER-PREDICATES helper."
  (mapcan (lambda (element) (pcase element
                          (`(,(and predicate (guard (member predicate filter-predicates))) . ,rest) (pcase (sexp-string--filter-predicates-helper rest filter-predicates)
                                                                                                      ((and res (guard (= (length res)  0))) 'nil)
                                                                                                      ((and res (guard (>= (length res)  1))) (list (cons predicate res)))))
                          (`(,(and predicate (guard (not (member predicate filter-predicates)))) . ,rest) 'nil)
                          (_ (list element))))
           query))

(defun sexp-string--query-sexp-to-string (query)
  "Return a string query for sexp QUERY.
If QUERY can't be converted to a string, return nil."
  ;; This started out pretty simple...but at least it's not just one long function, right?
  (cl-labels ((complex-p (query)
                         (or (contains-p 'or query)
                             (contains-p 'ancestors query)
                             (contains-p 'children query)
                             (contains-p 'descendants query)
                             (contains-p 'parent query)))
              (contains-p (symbol list)
                          (cl-loop for element in list
                                   thereis (or (eq symbol element)
                                               (and (listp element)
                                                    (contains-p symbol element)))))
              (format-args
               (args) (let (non-paired paired next-keyword)
                        (cl-loop for arg in args
                                 do (cond (next-keyword (push (cons next-keyword arg) paired)
                                                        (setf next-keyword nil))
                                          ((keywordp arg) (setf next-keyword (substring (symbol-name arg) 1)))
                                          (t (push arg non-paired))))
                        (string-join (append (mapcar #'format-atom non-paired)
                                             (nreverse (--map (format "%s=%s" (car it) (cdr it))
                                                              paired)))
                                     ",")))
              (format-atom
               (atom) (cl-typecase atom
                        (string (if (string-match (rx space) atom)
                                    (format "%S" atom)
                                  (format "%s" atom)))
                        (t (format "%s" atom))))
              (format-form
               (form) (pcase form
                        (`(not . (,rest)) (concat "!" (format-form rest)))
                        (`(priority . ,_) (format-priority form))
                        ;; FIXME: Convert (src) queries to non-sexp form...someday...
                        (`(src . ,_) (user-error "Converting (src ...) queries to non-sexp form is not implemented"))
                        (_ (pcase-let* ((`(,pred . ,args) form)
                                        (args-string (pcase args
                                                       ('() "")
                                                       ((guard (= 1 (length args))) (format "%s" (car args)))
                                                       (_ (format-args args)))))
                             (format "%s:%s" pred args-string)))))
              (format-and
               (form) (pcase-let* ((`(and . ,rest) form))
                        (string-join (mapcar #'format-form rest) " ")))
              (format-priority
               (form) (pcase-let* ((`(priority . ,rest) form)
                                   (args (pcase rest
                                           (`(,(and comparator (or '< '<= '> '>= '=)) ,letter)
                                            (priority-letters comparator letter))
                                           (_ rest))))
                        (concat "priority:" (string-join args ","))))
              (priority-letters
               (comparator letter) (let* ((char (string-to-char (upcase (symbol-name letter))))
                                          (numeric-priorities '(?A ?B ?C))
                                          ;; NOTE: The comparator inversion is intentional.
                                          (others (pcase comparator
                                                    ('< (--select (> it char) numeric-priorities))
                                                    ('<= (--select (>= it char) numeric-priorities))
                                                    ('> (--select (< it char) numeric-priorities))
                                                    ('>= (--select (<= it char) numeric-priorities))
                                                    ('= (--select (= it char) numeric-priorities)))))
                                     (mapcar #'char-to-string others))))
    ;; FIXME: Error out for ts structs passed to `ts' predicate (very unlikely to be linked to).
    (unless (complex-p query)
      (pcase query
        (`(and . ,_) (format-and query))
        (_ (format-form query))))))

(provide 'sexp-string)
;;; sexp-string.el ends here
