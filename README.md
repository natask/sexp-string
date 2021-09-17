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
