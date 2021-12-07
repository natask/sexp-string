# Sexp-string
``` quote
Come get your sexp strings. Sexp strings going once, sexp string going twice...
```

# Motivation
I Wanted to have a convenient way to write elisp expressions as a string. Keeping track of parenthesises doesn't make for a good user interface. Based on [org-ql's](https://github.com/alphapapa/org-ql) method.

# What it does
Convert to and from a string to a boolean elisp expression.

For example,

``"tag:tag1,tag2 title:title1,title2 query1 query2 `or `(query3 `and query4   `) query5 (to be, or not to be? to not be!)"``

matches the following under this [predicate](https://github.com/natask/org-roam-search/blob/7004fb06b21c26bda56048d531d629425ce25714/org-roam-search.el#L25).

``` elisp
(and
 (or
  (and (tag "tag1" "tag2")
       (title "title1" "title2")
       (both "query1")
       (both "query2"))
  (and (both "query3")
       (both "query4")))
 (both "query5")
 (both "(" "to")
 (both "be" ",")
 (both "or")
 (both "not")
 (both "to")
 (both "be "?" ".")
 (both "to")
 (both "not")
 (both "be" "!" ")"))
```

# Usage
look at how [org-roam-search](https://github.com/natask/org-roam-search/) uses `sexp-string`.
The key functions are
- `sexp-string--query-string-to-sexp`
- `sexp-string--query-sexp-to-string`
- `sexp-string--transform-query`

# Features
## Custom pexs 
Two syntaxs are currently supported.
org-ql like syntax called `sexp-string--custom-pexs`.
- `,` between args after predicates is `and`.
- `` `or`` is `or`.
- `` `and`` is `and`.
- `` `(`)`` are brackets.
- `!` is negation.
- **E.g**
``"tag:tag1,tag2 title:title1,title2 query1 query2 `or `(query3 `and query4   `) query5 (to be, or not to be? to not be!)"``

recoll like syntax (also Notmuch (Xapian) and Xesam) called `sexp-string--recoll-pexs`. 

- `,` between args in predicates is `and`.
- `/` between args in predicates is `or`.
- `OR` is `or`.
- `AND` is `and`.
- `()` are brackets.
- `-` is negation.
- **E.g**
``"tag:tag1,tag2/tag3 OR title:title1,title2 query1 query2 OR (query3 AND query4) query5 \"(to be, or not to be? to not be!)\""``

Look at `sexp-string--custom-pexs` to define your own.

# Featured downstream packages
- [org-roam-search](https://github.com/natask/org-roam-search/).
- [firefox-search](https://github.com/natask/firefox-search/).
- [delve-show](https://github.com/natask/delve-show/).

I know I know, these are all my packages. I don't know any one that uses this package yet. 

