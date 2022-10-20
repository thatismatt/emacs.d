;;-*-coding: utf-8;-*-
(define-abbrev-table 'clojure-mode-abbrev-table
  '(
    ("cap" "(matt.capture/capture :id)" nil :count 0)
    ("cb" "(comment ;;\n()\n\n\n\n)" nil :count 0)
    ("td" ";; TODO:" nil :count 0)
   ))

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("dt" "** [0/3] Deploy\n - [ ] QA\n - [ ] Staging\n - [ ] Prod" nil :count 0)
   ))

(define-abbrev-table 'sql-mode-abbrev-table
  '(
    ("cs" "COUNT(*)" nil :count 0)
    ("ct" "CREATE TABLE t (id SERIAL PRIMARY KEY NOT NULL);" nil :count 0)
    ("gb" "GROUP BY" nil :count 0)
    ("ii" "INSERT INTO t () VALUES ();" nil :count 0)
    ("ij" "INNER JOIN" nil :count 0)
    ("l1" "LIMIT 1" nil :count 0)
    ("ob" "ORDER BY" nil :count 0)
    ("pk" "SERIAL PRIMARY KEY NOT NULL" nil :count 0)
    ("scsf" "SELECT COUNT(*) FROM" nil :count 0)
    ("ssf" "SELECT * FROM" nil :count 0)
    ("us" "UPDATE t SET" nil :count 0)
    ("usw" "UPDATE t SET WHERE" nil :count 0)
   ))
