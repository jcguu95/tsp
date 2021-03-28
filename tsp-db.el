;;; tsp-db.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(defvar tsp:db "~/.tsp"
  "The root of TSP's database.")
(defvar tsp:db-ts
  (f-join tsp:db "ts/")
  "The subroot that stores TS related data.")
(mkdir tsp:db t)
(mkdir tsp:db-ts t)

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

(defun tsp:ts-prop-path (ts)
  "Return the path for the db of TS's ts-prop."
  (if (tsp:check-ts-format ts)
      (f-join tsp:db-ts (concat ts ".lisp"))
    (error "TS is expected to be a proper timestamp.")))

(defun tsp:update-ts-prop-from-a-file-and-a-ts (file ts)
  "A basic util that update the ts-prop for TS by the information
  provided by FILE."
  ;; TODO do this later - check file last update time if any, and
  ;; decide to update or not.

  ;; ts-prop = (:ts ts :file-props ((f1 . fp1) (f2. fp2) ..))

  ;; First make sure that ts-prop is non-nil.
  (let* ((ts-prop-path (tsp:ts-prop-path ts))
         (ts-prop (tsp:read-list-from-file ts-prop-path)))
    (when (null ts-prop)
      (progn
        (setf ts-prop `(:ts ,ts :file-props nil))
        (tsp:store-list-in-file ts-prop ts-prop-path))))

  ;; then .. TODO
  (if (member ts (tsp:ts-of-file file))
      (let* ((abso (file-truename file))
             (ts-prop-path (tsp:ts-prop-path ts))
             (ts-prop (tsp:read-list-from-file ts-prop-path))
             (file-props (plist-get ts-prop :file-props))
             (file-prop (alist-get abso file-props nil nil #'string=)))

        (when "needs-update-TODO" ; TODO implement this based on the last change time of FILE
          ;; FIXME --
          ;; use hash-table instead of alist.
          ;; to make the updating process more elegant
          (setf file-prop (tsp:file-prop file))
          (setf file-props
                (-filter (lambda (x)
                           (not (string= abso (car x))))
                         file-props))
          ;; FIXME --
          )
        (add-to-list 'file-props (cons abso (tsp:file-prop file)))
        (plist-put! ts-prop :file-props file-props)

        (tsp:store-list-in-file ts-prop ts-prop-path))
    (error "TS must be a timestring that associates with FILE.")))

(tsp:file-prop "~/20181227-000000.org")
(tsp:update-ts-prop-from-a-file-and-a-ts
 "~/20181227-000000.org" "20181227-000000")


(defun tsp:update-ts-prop-from-file (file)
  ;; ts-prop = (:ts ts :file-props ((f1 . fp1) (f2. fp2) ..))
  (let ((abso (file-truename file))
        (ts-list (tsp:read-list-from-file file))))
  )




(provide 'tsp-db)

      (f-write "(+ 1 2)"
               'utf-8
               "~/see")
