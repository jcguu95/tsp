;;; tsp-util.el -mode -*- coding: utf-8; lexical-binding: t; -*-

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

(provide 'tsp-util)
