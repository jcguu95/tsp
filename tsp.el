;;; tsp.el -mode -*- coding: utf-8; lexical-binding: t; -*-

;; External dependencies
(require 'f)
(require 'rx)
(require 'ts)
(require 'dash)

;; Internal dependencies
(let ((here (nth 1 (s-split " " (pwd)))))
  (add-to-list 'load-path here))
(require 'tsp-db)
(require 'tsp-util)
(require 'tsp-setup)
(require 'tsp-format)
(require 'tsp-org-parser)

(defun tsp:files<-ts (ts &key from-db)
  "Force fetch the list of files under TSP:LIB whose names
contain the given timestamp TS. Expect TS to be a full
timestring."
  (if from-db
      "TODO to implement"
    (progn
      ;; Check format.
      (unless (tsp:check-full-ts-format ts)
        (error "TS is not a full timestamp."))
      (unless (tsp:check-ts-format ts)
        (error "TS is not a timestamp."))
      ;; And grab all files.
      (-flatten
       (loop for dir in tsp:lib
             collect (loop for file in (-flatten (f-files dir))
                           if (string-match ts (f-base file))
                           collect file))))))

(defun tsp:last-update-of-file (file)
  "A general util that returns the last update time of FILE."
  ;; TODO add option for it to read from db.
  (format-time-string tsp:ts-format
                      (nth 5 (file-attributes file))))

(defun tsp:file-prop (file)
  "Return the properties of the given FILE."
  (let* ((abs-path (file-truename file))
         (extension (f-ext file))
         (size (f-size file))
         (last-update (tsp:last-update-of-file file))
         (timestamps (tsp:extract-ts-from-string abs-path))
         org-title
         org-header
         org-links)

    ;; If plain-text, add timestamps from the content of FILE.
    (when (member extension '("org" "md" "txt"))
      (setf timestamps
            (concatenate 'list
                         timestamps
                         (tsp:extract-ts-from-string (f-read file)))))

    ;; If org, to the additional work.
    (when (member extension '("org"))
      ;; Add timestamps from the org timestamps in FILE.
      (setf timestamps (concatenate 'list
                                    timestamps
                                    (tsp:get-ts-from-org file)))
      ;; Get other org data.
      (setf org-title (tsp:get-org-title file))
      (setf org-header (tsp:get-org-header file))
      (setf org-links (tsp:get-org-links file)))

    (list
     :abs-path abs-path
     :extension extension
     :size size
     :last-update last-update
     :timestamps timestamps

     ;; Org files specifics.
     :org-title org-title
     :org-header org-header
     :org-links org-links ;; TODO resolve file type links, attachment type.. etc?
     )))
