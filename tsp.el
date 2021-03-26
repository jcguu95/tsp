;; tsp.el -- Timestamp Properties
;;
;; The main function should do exactly one thing: provided a
;; timestamp (e.g. "20210325-160115"), it pulls everything of
;; interest into a list.

(defvar tsp:lib nil
  "The variable that stores the list of directories to search.")

(defvar tsp:main-note-dir
  "~/data/storage/+org/wiki/fleeting"
  "The path is where the main notes are expected to be.")

(loop for dir in
      '("~/data/storage/recordings"
        "~/data/storage/memories"
        "~/data/storage/+org/wiki/fleeting"
        "~/data/storage/+org/store")
      do (add-to-list 'tsp:lib dir))

(loop for dir in
      (f-directories "~/data/storage/memories")
      do (add-to-list 'tsp:lib dir))

(defun tsp:search (ts)
  "Expect TS to be a string that represents a timestamp in the
  format YYYYmmdd-HHMMSS."

  ;; format check
  (unless (string-match
           (rx-to-string
            `(seq (repeat 8 digit)
                  "-"
                  (repeat 6 digit)))
           ts)
    (error "TS is not in the expected format."))

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
                                          :title (my/parse-org-title o)
                                          :header (my/get-org-header o)
                                          :ts (my/extract-ts-from-string (f-read o))))
                             org-files))))

;; testing -- main entry point
(tsp:search "20181229-000000")
(tsp:search "20190226-000000")
(tsp:search "20210325-093001")

(defun my/read-org-file (file)
  "Expect FILE to be an org file. Return its org data."
  (with-temp-buffer
    (goto-char (point-max))
    (insert (f-read file))
    (goto-char (point-min))
    (org-element-parse-buffer)))

(defun my/parse-org-title (file)
  "Expect FILE to be an org file with a title. Return the title
as a string."
  (let* ((data (my/read-org-file file))
         (keyword (org-element-map data 'keyword 'identity nil t)))
    (and keyword (org-element-property :value keyword))))

(defun my/get-org-header (file)
  "Expect FILE to be an org file. Return its content, as a
string, up to the first headline."
  (let ((str (f-read file)))
    (subseq str 0 (string-match "\n\\*" str))))

(defun my/ts-check (str)
  "Check if the input string is an expected time string.

Mechanism: first check if it's of length 4, 6, 8, 11, 13, or 15.
If not, return nil. If yes, complete the string canonically so
that it's of length 15.

Next, break them into tokens, and check if they are as expected."
  ;; if length 4, can be any number
  ;; if length 6, the last two should be 01~12
  ;; if length 8, after translating to a date, should be a legit date
  ;; if length 11, first 8 should give a legit date, 9th should be a -, 10+11 should be between 00~23
  ;; if length 13, ..etc + the last two should be 00~59
  ;; if length 15, ..etc + the last two should be 00~59
  ;;
  ;; ;; test cases
  ;; (mapcar #'my/ts-check '("2020" "20210731-150156")) ;; all t
  ;; (mapcar #'my/ts-check '("20200731-" "20210731-150161")) ;; all nil
  (let ((len (length str)))
    (when
        ;; First sanity check.
        (cl-case len
          (4 (setf str (concat str "0101-000000")))
          (6 (setf str (concat str "01-000000")))
          (8 (setf str (concat str "-000000")))
          (11 (setf str (concat str "0000")))
          (13 (setf str (concat str "00")))
          (15 str))
      ;; Break them into tokens, and check if as expected.
      (ignore-errors
        (let ((year (read (substring str 0 4)))
              (month (read (substring str 4 6)))
              (day (read (substring str 6 8)))
              (dash (read (substring str 8 9)))
              (hour (read (substring str 9 11)))
              (minute (read (substring str 11 13)))
              (sec (read (substring str 13 15))))
          (and (and (integerp year) (> year 0))
               (and (integerp month) (<= 1 month 12))
               (and (integerp day) (<= 1 day 31))
               (eq '- dash)
               (and (integerp hour) (<= 0 hour 23))
               (and (integerp minute) (<= 0 minute 59))
               (and (integerp sec) (<= 0 sec 59))))))))

;; A quick narrow-down timestamp search utility:
(defun my/extract-ts-from-string (str)
;; ;; test cases
;; (mapcar #'my/extract-ts-from-string
;;         (list
;;          "This is a long--message- asd -s--20210107 -12--sd ok"
;;          "This is a long--message- asd -s--20210107-12--sd ok"
;;          "This is a long--message- asd -s--20210131-085932--sd ok"
;;          "This is a long--message- asd -s--20210170--sd ok"
;;          "This is a long--message- asd -s--202101--sd ok"))
  (-uniq
   (-filter (lambda (x) (my/ts-check x))
            (-flatten
             (s-match-strings-all

              (rx word-start
                  (** 4 15 (any digit "-"))
                  word-end)

              str)))))


;;; then lemme write an exporter
;;;
;;; here is an example data

(:files ("/home/jin/data/storage/+org/wiki/fleeting/20190226-000000.org")
 :org-files
 ((:path "/home/jin/data/storage/+org/wiki/fleeting/20190226-000000.org"
   :title "20190226-000000"
   :header "#+TITLE: 20190226-000000

#+ATTR_ORG: :width 500
[[file:./img_store/20190226000000.jpg]]
[[file:../../../wiki/research-project--macdonald-polynomial.org][research project: macdonald polynomial]]
[[file:../research-project--macdonald-polynomial.org][research project: macdonald polynomial]]
"
   :ts ("20190226-000000"))))

(defun my/export-ts-property (ts)
  (let* ((data (tsp:search ts))
         (files (plist-get data :files))
         (org-files (plist-get data :org-files)))
    (apply #'concat
           (concatenate 'list
                        (list (format "* %s\n" ts))
                        (loop for f in files
                              collect (format "[[file:%s]]\n" f))
                        (loop for o in org-files
                              collect (format "** %s\n\n%s\n\n%s\n\n"
                                              (plist-get o :title)
                                              (plist-get o :ts)
                                              (plist-get o :header)))))
    ))

(my/export-ts-property "20190226-000000")
