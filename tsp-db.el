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

(defun tsp:update-ts-prop-from-a-file-and-a-ts (file ts &optional force)
  "A basic util that update the ts-prop for TS by the information
  provided by FILE. This is the bootstrapper for the entry point
  #'tsp:update-ts-prop-from-file.

If FORCE is t, update database no regardless of the last update
of FILE."
  ;; ts-prop = (:ts ts :file-props (fp1 fp2 ..))

  ;; First make sure that ts-prop is non-nil.
  (let* ((ts-prop-path (tsp:ts-prop-path ts))
         (ts-prop (tsp:read-list-from-file ts-prop-path)))
    (when (null ts-prop)
      (progn
        (setf ts-prop `(:ts ,ts :file-props nil))
        (tsp:store-list-in-file ts-prop ts-prop-path))))

  ;; Then update database if FILE has changed since last update.
  (if (member ts (tsp:ts-of-file file))
      (let* ((abso (file-truename file))
             (ts-prop-path (tsp:ts-prop-path ts))
             (ts-prop (tsp:read-list-from-file ts-prop-path))
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







(defvar tsp:dir-info-db
  (f-join tsp:db "dir-info.lisp")
  "The path to the database that stores the historical content of
  all directories under TSP:LIB.")
(let ((target tsp:dir-info-db))
  (unless (f-exists-p target)
    (tsp:store-list-in-file nil target)))

;; (
;;  (:path dir
;;   :content (f-files dir))
;;  (:path dir
;;   :content (f-files dir))
;; )

(defun tsp:dir-content (dir &key from-db)
  "A general util that returns all contents of DIR."
  (let ((abso (file-truename dir)))
    (if from-db
        (plist-get (tsp:dir-info dir) :content)
      (concatenate 'list
                   (f-directories abso)
                   (f-files abso)))))

(defun tsp:dir-info (dir)
  "A utility that reads the database and returns the dir-info
for DIR." ;; TODO define "dir-info"
  (let ((abso (file-truename dir))
        (content (tsp:read-list-from-file tsp:dir-info-db)))
    (seq-find (lambda (x)
                (equal abso (plist-get x :path)))
              content)))

(defun tsp:update-dir-info (dir)
  (let* ((abso (file-truename dir))
         (content (tsp:read-list-from-file tsp:dir-info-db))
         (dir-info (tsp:dir-info dir))
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
                 `(:path ,abso
                   :last-update ,(tsp:last-update-of-file abso)
                   :content ,(tsp:dir-content abso :from-db nil)))
    ;; Then write the data into file.
    (tsp:store-list-in-file content tsp:dir-info-db)))

;; Usage example.
;; (loop for dir in tsp:lib
;;       do (tsp:update-dir-info dir))

(defun tsp:symm-diff-dir-content (dir)
  "A utility that returns the difference of dir-contents from db
and from the DIR itself."
  (let ((old (tsp:dir-content dir :from-db t))
        (new (tsp:dir-content dir :from-db nil)))
    (-uniq
     (-union (-difference old new)
             (-difference new old)))))

(defun tsp:update-ts-prop-from-dir (dir)
  "Seek if there's any new or removed file since last check. If
so, force update ts-prop for that file. Do it for each such file.

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

(provide 'tsp-db)
