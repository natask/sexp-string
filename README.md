# Sexp-string

Come get your sexp strings. Sexp strings going once, sexp string going twice...

# Motivation
Wanted to have a convient way to write elisp expressions as a string. Keeping track of parenthesises doesn't make for a good user interface. Based on [org-ql's](https://github.com/alphapaps/org-ql) method.

# What it does
Convert to and from a string to a boolean elisp expression.

For example,
``"tag:tag1,tag2 title:title1,title2 query1 query2 `or (query3 `and query4) `and query5"``
``` elisp
(and
 (or
  (and (tag "tag1" "tag2")
       (title "title1" "title2")
       (both "query1")
       (both "query2"))
  (and (both "query3")
       (both "query4")))
 (both "query5"))
```
