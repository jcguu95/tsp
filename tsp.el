;;; tsp.el -mode -*- coding: utf-8; lexical-binding: t; -*-

;; tsp.el -- Timestamp Properties
;;
;; The main function should do exactly one thing: provided a
;; timestamp (e.g. "20210325-160115"), it pulls everything of
;; interest into a list.

(require 'f)
(require 'rx)

(let ((here (nth 1 (s-split " " (pwd)))))
  (add-to-list 'load-path here))
(require 'tsp-setup)
(require 'tsp-format)

(defun tsp:file-paths<-ts (ts)
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

(defun tsp:file-prop (file)
  "Return the properties of the given FILE."
  (let* ((abs-path (file-truename file))
         (extension (f-ext file))
         (size (f-size file))
         (last-update (format-time-string
                       "%Y%m%d-%H%M%S"
                       (nth 5 (file-attributes "~"))))
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

    (list :abs-path abs-path
          :extension extension
          :size size
          :last-update last-update
          :timestamps timestamps
          ;; The followings only work for org files.
          :org-title org-title
          :org-header org-header
          :org-links org-links)))

(defun tsp:search (ts)
  "TODO"

  ;; return files
  (let* ((files (-flatten
                 (loop for dir in tsp:lib
                       collect (loop for file in (-flatten (f-files dir))
                                     if (string-match ts (f-base file))
                                     collect file))))
         (org-files (loop for file in files
                          if (equal (f-ext file) "org")
                          collect file)))
    (list :files files
          :org-files (mapcar (lambda (o) (list
                                          :path o
                                          :title (tsp:get-org-title o)
                                          :header (tsp:get-org-header o)
                                          :ts (tsp:extract-ts-from-string (f-read o))))
                             org-files))))

;; testing -- main entry point
;; (tsp:search "20181229-000000")
;; (tsp:search "20190226-000000")
;; (tsp:search "20210325-093001")

(defun tsp:read-org-file (file)
  "Expect FILE to be an org file. Return its org data."
  (with-temp-buffer
    (goto-char (point-max))
    (insert (f-read file))
    (goto-char (point-min))
    (org-element-parse-buffer)))

(defun tsp:get-org-title (file)
  "Expect FILE to be an org file with a title. Return the title
as a string."
  (let* ((data (tsp:read-org-file file))
         (keyword (org-element-map data 'keyword 'identity nil t)))
    (and keyword (org-element-property :value keyword))))

(defun tsp:get-org-header (file)
  "Expect FILE to be an org file. Return its content, as a
string, up to the first headline."
  (let ((str (f-read file)))
    (subseq str 0 (string-match "\n\\*" str))))

;; A quick narrow-down timestamp search utility:
(defun tsp:extract-ts-from-string (str)
;; ;; test cases
;; (mapcar #'tsp:extract-ts-from-string
;;         (list
;;          "This is a long--message- asd -s--20210107 -12--sd ok"
;;          "This is a long--message- asd -s--20210107-12--sd ok"
;;          "This is a long--message- asd -s--20210131-085932--sd ok"
;;          "This is a long--message- asd -s--20210170--sd ok"
;;          "This is a long--message- asd -s--202101--sd ok"))
  (-uniq
   (-filter (lambda (x) (tsp:check-ts-format x))
            (-flatten
             (s-match-strings-all

              (rx word-start
                  (** 4 15 (any digit "-"))
                  word-end)

              str)))))

;;; then lemme write an exporter
;;;
;;; here is an example data

;; (:files ("/home/jin/data/storage/+org/wiki/fleeting/20190226-000000.org")
;;  :org-files
;;  ((:path "/home/jin/data/storage/+org/wiki/fleeting/20190226-000000.org"
;;    :title "20190226-000000"
;;    :header "#+TITLE: 20190226-000000

;; #+ATTR_ORG: :width 500
;; [[file:./img_store/20190226000000.jpg]]
;; [[file:../../../wiki/research-project--macdonald-polynomial.org][research project: macdonald polynomial]]
;; [[file:../research-project--macdonald-polynomial.org][research project: macdonald polynomial]]
;; "
;;    :ts ("20190226-000000"))))

(defun tsp:export-ts-property (ts)
  "Return the properties of TS into an org string."
  (let* ((data (tsp:search ts))
         (files (plist-get data :files))
         (org-files (plist-get data :org-files)))
    (apply #'concat
           (concatenate 'list
                        (list (format "* %s\n" ts)
                              (format "+ files ::\n"))
                        (loop for f in files
                              collect (format "\n[[file:%s]]\n\n" f))
                        (loop for o in org-files
                              collect (format "** %s\n\n*** timestamps\n%s\n\n*** header\n%s\n\n"
                                              (plist-get o :title)
                                              (plist-get o :ts)
                                              (plist-get o :header)))))
    ))

;; (tsp:export-ts-property "20190226-000000")
;; (tsp:export-ts-property "20210325-093001")

(defun tsp:get-ts-from-org (org-file)
  "Expect ORG-FILE to be an org file. Parse all org timestamps in
it, and translate them into our ts format."
  (cl-labels ((ts<-time (time) (ts-format tsp:ts-format time)))
    (mapcar #'ts<-time
            (mapcar #'ts-parse-org
                    (org-element-map (tsp:read-org-file org-file) 'timestamp
                      (lambda (ts) (org-element-property :raw-value ts)))))))

;;; ROADMAP
;;;
;;; from a timestamp, collect all files with type, size, last
;;; update time. if file is text, collect ordinary time strings
;;; in side, and additionally if file is org, collect its title,
;;; links, and org timestamps inside.
;;;
;;; Have a sexp based ts database! Update when an associated file
;;; has been updated since last check. Allow force update as
;;; well.
;;;
;;;   From db like this, we can make a directed graph (another db!)
;;;
;;;  and write an exporter for this db.. export to an org buffer.
;;;
;;;
;;; From a file whose name contains a timestring, let the user be
;;; able to quickly get to the exported buffer, in which all
;;; associated files are presented and linked. If no data is
;;; available, ask the user if initiate.

(defun tsp:ts-property (ts))
  ;; TODO
  ;; 1. grab file types
  ;; 2. grab file last-update time

(defun tsp:get-org-links (org-file)
  (org-element-map
      (tsp:read-org-file org-file) 'link
    ;; TODO need to resolve links for file type - this is crucial
    (lambda (link)
      (org-element-property :raw-link link))))
