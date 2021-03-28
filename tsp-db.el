;;; tsp-db.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(defvar tsp:db "~/.tsp")

(defun tsp:ts-of-file (file)
  (let ((abs (file-truename file)))
    (tsp:extract-ts-from-string abs)))

(defun tsp:update-ts-prop-from-file (file)
  ;; TODO first need a concrete database
  )




(provide 'tsp-db)
