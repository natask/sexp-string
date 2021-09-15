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
;; Package-Requires: ((emacs "25.1"))
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

;;; usage:
;;; ;; HACK: These functions *will* be defined at runtime, so we silence
;; compiler warnings about them:
;; (defvar LIBRARY-predicates
;; '((or  :name or)
;;   (and :name and)
;;   (titles :name titles :aliases '(title))
;;   (tags :name tags :aliases '(tag))
;;   (both :name both)))
;; (defvar LIBRARY-default-predicate-boolean 'and)
;; (defvar LIBRARY-default-predicate 'both)
;;
;; (declare-function LIBRARY--query-string-to-sexp "ext:LIBRARY" (query) t)
;; (declare-function LIBRARY--transform-query "ext:LIBRARY" (query) t)
;; (declare-function LIBRARY--normalize-query "ext:LIBRARY" (query) t)
;; (declare-function LIBRARY--query-preamble "ext:LIBRARY" (query) t)
;;
;; (fset 'LIBRARY--query-string-to-sexp (sexp-string--define-query-string-to-sexp-fn "LIBRARY"))
;; (fset 'LIBRARY--transform-query (sexp-string--define-transform-query-fn "LIBRARY" type))
;; (fset 'LIBRARY--normalize-query (sexp-string--define-normalize-query-fn "LIBRARY"))
;; (fset 'LIBRARY--query-preamble (sexp-string--define-query-preamble-fn "LIBRARY"))

;;; Code:
;;; vars:
(defvar sexp-string-use-preamble t
  ;; MAYBE: Naming things is hard.  There must be a better term than "preamble."
  "Use query preambles to speed up searches.
May be disabled for debugging, benchmarks, etc.")

(defvar sexp-string-predicates 'nil
  ;; '((or  :name or
  ;;        :transform
  ;;        ((`(or . ,clauses) (--> clauses
  ;;                                (cl-reduce (lambda (x y) (concat x " or " y)) (mapcar #'rec it))
  ;;                                (concat "(" it ")")))))
  ;;   (and :name and
  ;;        :transform
  ;;        ((`(and . ,clauses) (--> clauses
  ;;                                 (cl-reduce (lambda (x y) (concat x " and " y)) (mapcar #'rec it))
  ;;                                 (concat "(" it ")")))))
  ;;   (url :name url :docstring "Return non-nil if current heading has one or more of TAGS (a list of strings).\nTests both inherited and local tags." :args (&rest titles)
  ;;        :transform
  ;;        ((`(url . ,rest)
  ;;          (--> rest
  ;;               (mapcar (apply-partially #'format "url like '%%%%%s%%%%'") it)
  ;;               (cl-reduce (lambda (x y) (concat x " or " y)) it)
  ;;               (concat "(" it ")")))))
  ;;   (regexp :name regexp  :docstring "Return non-nil if current heading has one or more of TAGS (a list of strings).\nTests both inherited and local tags." :args (&rest regexp)
  ;;           :transform
  ;;           ((`(regexp . ,rest)
  ;;             (--> rest
  ;;                  (mapcar (lambda (x) (concat (format "title like '%%%% %s%%%%'" x)
  ;;                                              " or "
  ;;                                              (format "description like '%%%% %s%%%%'" x))) it)
  ;;                  (cl-reduce (lambda (x y) (concat x " or " y)) it)
  ;;                  (concat "(" it ")"))))))
  )

(defvar sexp-string-default-predicate-boolean 'and)
;;; Code:

(defun sexp-string--predicate-names (predicates)
  "Get predicate names for PREDICATES."
  (let* (
         (names (--map (symbol-name (plist-get (cdr it) :name))
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
                                    (sexp-string--collapse-list val op))))
            (_ (list val))))
           lt))
(defun sexp-string--define-query-string-to-sexp-fn (library-name)
  "Define function `sexp-string--query-string-to-sexp' for LIBRARY-NAME.
Builds the PEG expression using PREDICATES (which should be the
value of `sexp-string-predicates').
Borrowed from `org-ql'."
  (let* ((predicates (intern-soft (concat library-name "-predicates")))
         (boolean-variable (intern-soft (concat library-name "-default-predicate-boolean")))
         (default (intern-soft (concat library-name "-default-predicate")))
         (pexs `((query sum (opt eol))
                 (sum value  (* (or (and _ "`and" _ value `(a b -- (list 'and a b)))
                                    (and _ "`or" _ value `(a b -- (list 'or a b)))
                                    (and _ value `(a b -- (list ',,boolean-variable a b))))))
                 (value
                  (or (and "`(" (opt _) sum (opt _) "`)")
                      term
                      ))
                 (term (or (and negation (list positive-term)
                                ;; This is a bit confusing, but it seems to work.  There's probably a better way.
                                `(pred -- (list 'not (car pred))))
                           positive-term))
                 (positive-term (or
                                    (and predicate-with-args `(pred args -- (cons (intern pred) args)))
                                    (and predicate-without-args `(pred -- (list (intern pred))))
                                    (and plain-string `(s -- (cons ',,default s)))))
                 (predicate-with-args (substring predicate) ":" args)
                 (predicate-without-args (substring predicate) ":")
                 (predicate (or ,@(sexp-string--predicate-names (reverse (symbol-value predicates)))))
                 (args (list (+ (and (or keyword-arg quoted-arg unquoted-arg) (opt separator)))))
                 (keyword-arg (and keyword "=" `(kw -- (intern (concat ":" kw)))))
                 (keyword (substring (+ (not (or brackets brackets2 separator "=" "\"" (syntax-class whitespace))) (any))))
                 (quoted-arg "\"" (substring (+ (not "\"") (any))) "\"")
                 (unquoted-arg (substring (+ (not (or brackets brackets2 separator "\"" (syntax-class whitespace))) (any))))
                 (plain-string (list (+ (or unquoted-arg quoted-arg (substring (and (or separator negation brackets) (not brackets2)))))))
                 (negation "!")
                 (separator "," )
                 (operator (or "`and" "`or"))
                 (brackets (or "(" ")"))
                 (brackets2 (or "`(" "`)"))
                 (_ (+ [" \t"]))
                 (eol (or  "\n" "\r\n" "\r"))))
         (closure (lambda (input &optional boolean)
                    "Return query parsed from plain query string INPUT.
  Multiple predicate-names are combined with BOOLEAN (default: `and')."
                    ;; HACK: Silence unused lexical variable warnings.
                    (ignore library-name predicates boolean-variable default)
                    (unless (string-empty-p input)
                      (let ((parsed-sexp
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
                                        (peg-run (peg ,(caar pexs)) #'peg-signal-failure))))))
                        (eval
                         `(let ((,boolean-variable (or boolean ,boolean-variable)))
                         (pcase parsed-sexp
                            (`(,_) (->> `,(backquote ,(car parsed-sexp))
                                        backquote
                                        eval
                                        sexp-string-collapse-list))
                            (_ nil))) (list (cons 'boolean boolean) (cons 'parsed-sexp parsed-sexp))))))))
    (byte-compile closure)))

(defun sexp-string--define-transform-query-fn (library-name type)
  "Return query transformation for LIBRARY-NAME against TYPE.
`predicates' should be the value of `sexp-string-predicates'.
Borrowed from `org-ql'."
  (let* ((predicates (intern-soft (concat library-name "-predicates")))
         (closure  `(lambda (query)
                      "Return transformed form of QUERY expression.
This function is defined by calling
`sexp-string--define-transform-query-fn', which uses transformr forms
defined in `sexp-string-predicates' by calling `sexp-string-defpred'."
                      (let ((transformer-patterns (->> ,predicates
                                                     (--map (plist-get (cdr it) ,type))
                                                     (-flatten-n 1))))
                     (eval
                      `(cl-labels ((rec (element &optional accum)
                                       (ignore accum)
                                        (pcase element
                                         ,@transformer-patterns
                                         (_ (error "Element didn't match transformer: %S" element)))))
                        (rec query)) (list (cons 'transformer-patterns transformer-patterns) (cons 'query query)))))))
    (byte-compile closure)))

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

;;;;; Predicate definition



(defvar sexp-string-defpred-defer nil
  "Defer expensive function redefinitions when defining predicates.
When non-nil, defining a predicate with `sexp-string-defpred' does not
cause the functions `sexp-string--normalize-query',
`sexp-string--query-preamble', and `sexp-string--query-string-to-sexp' to
be redefined.  These functions must be redefined in order to
account for new predicates, but when defining many
predicates (like at load time), that may be deferred for
performance (after which those functions should be updated
manually; see the definition of `sexp-string-defpred').")

;; Yes, these two functions are a little hairy: `pcase' is challenging
;; enough, but splicing forms into one is something else.  But it's
;; worth it to do this ugly stuff here, in one place, so the
;; `sexp-string-defpred' macro becomes easy to use.

(defun sexp-string--define-normalize-query-fn (predicates)
  "Define function `sexp-string--normalize-query' for PREDICATES.
PREDICATES should be the value of `sexp-string-predicates'."
  (let ((normalizer-patterns (->> predicates
                                  (--map (plist-get (cdr it) :normalizers))
                                  (-flatten-n 1))))
    (byte-compile
     `(lambda (query)
        "Return normalized form of QUERY expression.
This function is defined by calling
`org-ql--define-normalize-query-fn', which uses normalizer forms
defined in `org-ql-predicates' by calling `org-ql-defpred'."
        (cl-labels ((rec (element)
                         (pcase element
                           (`(or . ,clauses) `(or ,@(mapcar #'rec clauses)))
                           (`(and . ,clauses) `(and ,@(mapcar #'rec clauses)))
                           (`(not . ,clauses) `(not ,@(mapcar #'rec clauses)))
                           (`(when ,condition . ,clauses) `(when ,(rec condition)
                                                             ,@(mapcar #'rec clauses)))
                           (`(unless ,condition . ,clauses) `(unless ,(rec condition)
                                                               ,@(mapcar #'rec clauses)))
                           ;; TODO: Combine (regexp) when appropriate (i.e. inside an OR, not an AND).
                           ((pred stringp) `(regexp ,element))

                           ,@normalizer-patterns

                           ;; Any other form: passed through unchanged.
                           (_ element))))
          ;; Repeat normalization until result doesn't change (limiting to 10 in case of an infinite-loop bug).
          (cl-loop with limit = 10 and count = 0
                   for new-query = (rec query)
                   until (equal new-query query)
                   do (progn
                        (setf query new-query)
                        (when (eq (cl-incf count) limit)
                          (error "Query normalization limit exceeded: QUERY:%S" query)))
                   finally return new-query))))))

(defun sexp-string--define-query-preamble-fn (predicates)
  "Define function `sexp-string--query-preamble' for PREDICATES.
PREDICATES should be the value of `sexp-string-predicates'."
  ;; NOTE: I don't how the `list' symbol ends up in the list, but anyway...
  (let* ((preamble-patterns
          (-flatten-n 1 (-non-nil
                         ;; NOTE: Using -let instead of pcase-let here because I can't make map 2.1 install in the test sandbox.
                         (--map (-let* (((&plist :preambles) (cdr it)))
                                  (--map (pcase-let* ((`(,pattern ,exp) it))
                                           `(,pattern
                                             (-let* (((&plist :regexp :case-fold :query) ,exp))
                                               (setf sexp-string-preamble regexp
                                                     preamble-case-fold case-fold)
                                               ;; NOTE: Even when `predicate' is nil, it must be returned in the pcase form.
                                               query)))
                                         preambles))
                                predicates))))
         (closure
          `(lambda (query)
             "Return preamble data plist for QUERY.
The plist has the following keys:

  :preamble Regexp to search the Org buffer for to find potential
            matches.  If the regexp doesn't guarantee a match to
            QUERY, the :query key should be an appropriate query
            expression to test against the heading (i.e. usually
            QUERY again).

  :preamble-case-fold What to bind variable `case-fold-search' to
                      around the regexp search.

  :query The query expression to use instead of QUERY (or QUERY
         again, when appropriate).

This function is defined by calling
`sexp-string--define-query-preamble-fn', which uses preamble forms
defined in `sexp-string-predicates' by calling `sexp-string-defpred'."
             (pcase sexp-string-use-preamble
               ('nil (list :query query :preamble nil))
               (_ (let ((preamble-case-fold t)
                        sexp-string-preamble)
                    (cl-labels ((rec (element)
                                     (or (when sexp-string-preamble
                                           ;; Only one preamble is allowed
                                           element)
                                         (pcase element
                                           (`(or _) element)

                                           ,@preamble-patterns

                                           (`(and . ,rest)
                                            (let ((clauses (mapcar #'rec rest)))
                                              `(and ,@(-non-nil clauses))))
                                           (_ element)))))
                      (setq query (pcase (mapcar #'rec (list query))
                                    ((or `(nil)
                                         `((nil))
                                         `((and))
                                         `((or)))
                                     t)
                                    (`(t) t)
                                    (query (-flatten-n 1 query))))
                      (list :query query :preamble sexp-string-preamble :preamble-case-fold preamble-case-fold))))))))
    ;; For some reason, byte-compiling the backquoted lambda form directly causes a warning
    ;; that `query' refers to an unbound variable, even though that's not the case, and the
    ;; function still works.  But to avoid the warning, we byte-compile it afterward.
    (byte-compile closure)))


;; TODO: the following needs to take the library name as input. Currenlty returns a list to be fset to functions.
(cl-defmacro sexp-string-defpred (name args docstring &key body preambles normalizers)
  "Define an `sexp-string' selector predicate named `sexp-string--predicate-NAME'.
NAME may be a symbol or a list of symbols: if a list, the first
is used as NAME and the rest are aliases.  A function is only
created for NAME, not for aliases, so a normalizer should be used
to replace aliases with NAME in queries (keep reading).

ARGS is a `cl-defun'-style argument list.  DOCSTRING is the
function's docstring.

BODY is the body of the predicate.  It will be evaluated with
point on the beginning of an Org heading and should return
non-nil if the heading's entry is a match.

PREAMBLES and NORMALIZERS are lists of `pcase' forms matched
against Org QL query sexps.  They are spliced into `pcase' forms
in the definitions of the functions `sexp-string--query-preamble' and
`sexp-string--normalize-query', which see.  Those functions are
redefined when this macro is expanded, unless variable
`sexp-string-defpred-defer' is non-nil, in which case those functions
should be redefined manually after defining predicates by calling
`sexp-string--define-query-preamble-fn' and `sexp-string--define-normalize-query-fn'.

NORMALIZERS are used to normalize query expressions to standard
forms.  For example, when the predicate has aliases, the aliases
should be replaced with predicate names using a normalizer.
Also, predicate arguments may be put into a more optimal form so
that the predicate has less work to do at query time.  NOTE:
Normalizers are applied to a query repeatedly until the query is
fully normalized, so normalizers should be carefully written to
avoid infinite loops.

PREAMBLES refer to regular expressions which may be used to
search through a buffer directly to a potential match rather than
testing the predicate body on each heading.  (Naming things is
hard.)  In each `pcase' form in PREAMBLES, the `pcase'
expression (not the pattern) should be a plist with the following
keys, each value of which should be an expression which may refer
to variables bound in the pattern:

  :regexp     Regular expression which searches directly to a
              potential match.

  :case-fold  Bound to `case-fold-search' around the regexp search.

  :query      Expression which should replace the query expression,
              or `query' if it should not be changed (e.g. if the
              regexp is insufficient to determine whether a
              heading matches, in which case the predicate's body
              needs to be tested on the heading).  If the regexp
              guarantees a match, this may be simply t, leaving the
              query expression with no work to do, which improves
              performance.

For convenience, within the `pcase' patterns, the symbol
`predicate-names' is a special form which is replaced with a
pattern matching any of the predicate's name and aliases.  For
example, if NAME were:

  (heading h)

Then if NORMALIZERS were:

  ((`(,predicate-names . ,args)
  `(heading ,@args)))

It would be expanded to:

  ((`(,(or 'heading 'h) . ,args)
  `(heading ,@args)))"
  ;; NOTE: The debug form works, completely!  For example, use `edebug-defun'
  ;; on the `heading' predicate, then evaluate this form:
  ;; (let* ((query '(heading "HEADING"))
  ;;        (normalized (sexp-string--normalize-query query))
  ;;        (preamble (sexp-string--query-preamble normalized)))
  ;;   (list :query query
  ;;         :normalized normalized
  ;;         :preamble preamble))
  (declare (debug ([&or symbolp listp] listp stringp
                   &rest [&or [":body" def-body]
                              [":normalizers" (&rest (sexp def-body))]
                              [":preambles" (&rest (sexp def-body))]]))
           (indent defun))
  (let* ((aliases (when (listp name)
                    (cdr name)))
         (name (cl-etypecase name
                 (list (car name))
                 (atom name)))
         (fn-name (intern (concat "sexp-string--predicate-" (symbol-name name))))
         (predicate-name (intern (symbol-name name)))
         (predicate-names (delq nil (cons predicate-name aliases)))
         (normalizers (cl-sublis (list (cons 'predicate-names (cons 'or (--map (list 'quote it) predicate-names))))
                                 normalizers))
         (preambles (cl-sublis (list (cons 'predicate-names (cons 'or (--map (list 'quote it) predicate-names))))
                               preambles)))
    `(progn
       (cl-defun ,fn-name ,args ,docstring ,body)
       ;; SOMEDAY: Use `map-elt' here, after map 2.1 can be automatically installed in CI sandbox...
       (setf (alist-get ',predicate-name sexp-string-predicates)
             `(:name ,',name :aliases ,',aliases :fn ,',fn-name :docstring ,(\, docstring) :args ,',args
               :normalizers ,',normalizers :preambles ,',preambles))
       (unless sexp-string-defpred-defer
         ;; Reversing preserves the order in which predicates were defined.
         (list
          (sexp-string--define-normalize-query-fn (reverse sexp-string-predicates))
          (sexp-string--define-query-preamble-fn (reverse sexp-string-predicates))
          (sexp-string--def-query-string-to-sexp-fn (reverse sexp-string-predicates)))))))

(provide 'sexp-string)
;;; sexp-string.el ends here
