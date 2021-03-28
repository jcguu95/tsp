;;; tsp-db.el -mode -*- coding: utf-8; lexical-binding: t; -*-

;; For updating, entry points are
;;
;; 1. #'tsp:update-ts-prop-from-file
;; 2. #'tsp:update-ts-prop-from-dir
;;
;; For reading, entry points are
;;
;; 1. #'tsp:all-ts
;; 2. #'tsp:ts-prop

;; TODO I think there's a bug.. when a file has been removed,
;; which nukes a timestamp, the ts-prop of that timestamp will
;; still remain in the database.

(defvar tsp:db "~/.tsp"
  "The root of TSP's database.")
(mkdir tsp:db t)

(defvar tsp:db-ts
  (f-join tsp:db "ts/")
  "The subroot that stores TS related data.")
(mkdir tsp:db-ts t)

(defun tsp:all-ts (&key from-db)
  "Return the list of all available timestamps either from the
  database or the names of the files under TSP:LIB."
  (if from-db
      (mapcar #'f-base (f-files tsp:db-ts))
    (-flatten
     (loop for dir in tsp:lib
           collect (loop for file in (f-files dir)
                         collect (tsp:extract-ts-from-string file))))))

(defun tsp:ts-prop-path (ts)
  "Return the path for the db of TS's ts-prop."
  (if (tsp:check-ts-format ts)
      (f-join tsp:db-ts (concat ts ".lisp"))
    (error "TS is expected to be a proper timestamp.")))

(defun tsp:ts-prop (ts)
  "Read ts-prop for TS from the database."
  (tsp:read-list-from-file (tsp:ts-prop-path ts)))

(defun tsp:update-ts-prop-from-a-file-and-a-ts (file ts &optional force)
  "A basic database-util that update the ts-prop for TS by the
  information provided by FILE. This is the bootstrapper for the
  entry point #'tsp:update-ts-prop-from-file.

If FORCE is t, update database no regardless of the last update
of FILE."
  ;; First make sure that ts-prop is non-nil.
  (let* ((ts-prop-path (tsp:ts-prop-path ts))
         (ts-prop (tsp:ts-prop ts)))
    (when (null ts-prop)
      (progn
        (setf ts-prop `(:ts ,ts :file-props nil))
        (tsp:store-list-in-file ts-prop ts-prop-path))))
  ;; Then update database if FILE has changed since last update.
  (if (member ts (tsp:ts-of-file file))
      (let* ((abso (file-truename file))
             (ts-prop-path (tsp:ts-prop-path ts))
             (ts-prop (tsp:ts-prop ts))
             (file-props (plist-get ts-prop :file-props))
             (file-prop (seq-find (lambda (x)
                                    (equal abso
                                           (plist-get x :abs-path)))
                                  file-props)))
        ;; If FILE's last changed time is different from that
        ;; recorded in the db, run the body of the following
        ;; UNLESS block to update information in the db.
        (when (or force
                  (not (equal (tsp:last-update-of-file file)
                              (plist-get file-prop :last-update))))
          ;; FIXME -- use hash-table instead of plist to make the
          ;; updating process more elegant
          (setf file-prop (tsp:file-prop file))
          (setf file-props
                (-filter (lambda (x)
                           (not (string= abso (plist-get x :abs-path))))
                         file-props))
          ;; FIXME ----------------------------------------------
          (add-to-list 'file-props (tsp:file-prop file))
          (plist-put! ts-prop :file-props file-props)
          (tsp:store-list-in-file ts-prop ts-prop-path)))
    (error "TS must be a timestring that associates with FILE.")))

(defun tsp:update-ts-prop-from-file (file &optional force)
  "Entry point for updating the database from the information of
FILE."
  (let ((abso (file-truename file))
        (ts-list (tsp:ts-of-file file)))
    (loop for ts in ts-list
          do (tsp:update-ts-prop-from-a-file-and-a-ts
              file ts force))))

;; Next, write #'tsp:update-ts-prop-from-dir that's built on top
;; of #'tsp:update-ts-prop-from-file.

(defvar tsp:dir-info-db
  (f-join tsp:db "dir-info.lisp")
  "The path to the database that stores the historical content of
  all directories under TSP:LIB.")
(let ((target tsp:dir-info-db))
  (unless (f-exists-p target)
    (tsp:store-list-in-file nil target)))

(defun tsp:dir-content (dir &key from-db)
  "A database-util that returns all contents of DIR."
  (let ((abso (file-truename dir)))
    (if from-db
        (plist-get (tsp:dir-info dir :from-db t) :content)
      (concatenate 'list
                   (f-directories abso)
                   (f-files abso)))))

(defun tsp:dir-info (dir &key from-db)
  "A database-util that reads the database and returns the dir-info for
DIR." ;; TODO define "dir-info" clearly in the DOC.
  (let ((abso (file-truename dir)))
    (if from-db
        ;; Read directly from db.
        (let ((content (tsp:read-list-from-file tsp:dir-info-db)))
          (seq-find (lambda (x)
                      (equal abso (plist-get x :path)))
                    content))
      ;; Compute dir-info on the fly.
      (list
       :path abso
       :last-update (tsp:last-update-of-file abso)
       :content (tsp:dir-content abso :from-db nil)))))

(defun tsp:update-dir-info (dir)
  (let* ((abso (file-truename dir))
         (content (tsp:read-list-from-file tsp:dir-info-db))
         (dir-info (tsp:dir-info dir :from-db t))
         (last-update (plist-get dir-info :last-update)))
    ;; If dir has changed since last update, delete the old data.
    ;; FIXME something is wrong here
    (when (not (equal last-update
                      (tsp:last-update-of-file abso)))
      (plist-put! dir-info
                  :content (tsp:dir-content abso :from-db nil))
      (setf content
            (-filter
             (lambda (x)
               (not (string= abso
                             (plist-get x :path))))
             content)))
    ;; And add the new data, regardlessly.
    (add-to-list 'content
                 (tsp:dir-info abso :from-db nil))
    ;; Then write the data into file.
    (tsp:store-list-in-file content tsp:dir-info-db)))

;; Usage example.
;; (loop for dir in tsp:lib
;;       do (tsp:update-dir-info dir))

(defun tsp:symm-diff-dir-content (dir)
  "Returns the difference of dir-contents from db and from the
DIR itself."
  (let ((old (tsp:dir-content dir :from-db t))
        (new (tsp:dir-content dir :from-db nil)))
    (-uniq (tsp:-symmetric-difference old new))))

(defun tsp:update-ts-prop-from-dir (dir)
  "Another entry point to update ts-prop from the information of
DIR, which is expected to be a member under TSP:LIB.

Seek if there's any new or removed file since last check. If so,
force update ts-prop for that file. Do it for each such file.

Notice - #'tsp:update-ts-prop-from-file bootstraps this
function!"
  (if (member (file-truename dir)
              (mapcar #'file-truename tsp:lib))

      (let* ((symm-diff (tsp:symm-diff-dir-content dir)))
        (unless (null symm-diff)
          (tsp:update-dir-info dir))
        (loop for file in symm-diff
              do (tsp:update-ts-prop-from-file file)))

    (error "DIR must be a member of TSP:LIB.")))

;; Use case.
;; (loop for dir in tsp:lib
;;       do (tsp:update-ts-prop-from-dir dir))

(provide 'tsp-db)
