;;; tsp-util.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(defun tsp:extract-ts-from-string (str)
  "The core utility that extracts time stamps from any given
string."
  ;; ;; test cases
  ;; (mapcar #'tsp:extract-ts-from-string
  ;;         (list
  ;;          "This is a long--message- asd -s--20210107 -12--sd ok"
  ;;          "This is a long--message- asd -s--20210107-12--sd ok"
  ;;          "This is a long--message- asd -s--20210131-085932--sd ok"
  ;;          "This is a long--message- asd -s--20210170--sd ok"
  ;;          "This is a long--message- asd -s--202101--sd ok"))
  (let ((result (-uniq
                 (-filter (lambda (x) (tsp:check-ts-format x))
                          (-flatten
                           (s-match-strings-all

                            (rx word-start
                                (** 4 15 (any digit "-"))
                                word-end)

                            str)

                           )))))

    ;; This is just a weird hack to prevent "legal" timestrings
    ;; like "101010" .. and it would still have some edge cases.
    (-filter (lambda (x) (>= (length x) 8)) result)

    ))

(defun tsp:store-list-in-file (lst file)
  "A basic util that stores a lisp list into FILE."
  (if (listp lst)
      (f-write ;; this overwrites FILE!
       (prin1-to-string lst)
       'utf-8 file)
    (error "LST must be a list.")))

(defun tsp:read-list-from-file (file)
  "A basic util that reads a list from FILE."
  (when (f-exists-p file)
    (let ((content (read (f-read file))))
      (if (listp content)
          content
        (error "FILE exists but doesn't contain a proper list.")))))

(defun tsp:ts-of-file (file)
  "A basic util that returns a list of timestamps from FILE's
name."
  (let ((abso (file-truename file)))
    (tsp:extract-ts-from-string abso)))

(defun tsp:-symmetric-difference (x y)
  "A general utility that extends dash.el. It returns the
    symmetric difference of two lists X and Y, treated as sets."
  (-union (-difference x y)
          (-difference x y)))

(defun tsp:last-update-of-file (file)
  "A general util that returns the last update time of FILE."
  (format-time-string tsp:ts-format
                      (nth 5 (file-attributes file))))

(provide 'tsp-util)
