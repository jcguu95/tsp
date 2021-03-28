;;; tsp.el -mode -*- coding: utf-8; lexical-binding: t; -*-

;; External dependencies
(require 'f)
(require 'rx)

;; Internal dependencies
(let ((here (nth 1 (s-split " " (pwd)))))
  (add-to-list 'load-path here))
(require 'tsp-db)
(require 'tsp-setup)
(require 'tsp-format)
(require 'tsp-org-parser)

(defun tsp:files<-ts (ts)
  "Force fetch the list of files under TSP:LIB whose names
contain the given timestamp TS. Expect TS to be a full
timestring."
  ;; Check format.
  (unless (tsp:check-full-ts-format ts)
    (error "TS is not a full timestamp."))
  (unless (tsp:check-ts-format ts)
    (error "TS is not a timestamp."))

  ;; Grab all files.
  (-flatten
   (loop for dir in tsp:lib
         collect (loop for file in (-flatten (f-files dir))
                       if (string-match ts (f-base file))
                       collect file))))

(defun tsp:last-update-of-file (file)
  "A general util that returns the last update time of FILE."
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

(defun tsp:all-ts ()
  "Return the list of all available timestamps from the names of
  the files under TSP:LIB."
  (-flatten
   (loop for dir in tsp:lib
         collect (loop for file in (f-files dir)
                       collect (tsp:extract-ts-from-string file)))))
