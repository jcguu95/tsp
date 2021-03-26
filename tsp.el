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
  (let ((files (-flatten
                (loop for dir in tsp:lib
                      collect (loop for file in (-flatten (f-files dir))
                                    if (string-match ts (f-base file))
                                    collect file))))
        (main-note (let ((target (f-join tsp:main-note-dir
                                         (concat ts ".org"))))
                     (when (f-exists-p target)
                       target))))
    (list :files files
          :main-note main-note
          :title (my/parse-org-title main-note)
          :header (my/get-org-header main-note)))

  ;; return timestamp it points to
  ;; TODO Again it boils down to org parsing..
  )

;; testing
(tsp:search "20181229-000000")

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

;; working

;; A quick narrow-down timestamp search utility:
(-flatten
 (s-match-strings-all

  (rx word-start
      (** 4 15 (any digit "-"))
      word-end)

  "embedded-in-a-mess-20201017-123124--Hi"))

;; Now write my function to pin down the details
;;
;; if length 4, can be any number
;; if length 6, the last two should be 01~12
;; if length 8, after translating to a date, should be a legit date
;; if length 11, first 8 should give a legit date, 9th should be a -, 10+11 should be between 00~23
;; if length 13, ..etc + the last two should be 00~59
;; if length 15, ..etc + the last two should be 00~59

(defun my/ts-check (str)
  (let ((len (length str)))
    (when
        (cl-case len
          (4 (setf str (concat str "0101-000000")))
          (6 (setf str (concat str "01-000000")))
          (8 (setf str (concat str "-000000")))
          (11 (setf str (concat str "0000")))
          (13 (setf str (concat str "00")))
          (15 str))
      (let ((year (substring str 0 4))
            (month (substring str 4 6))
            (day (substring str 6 8))
            (dash (substring str 8 9))
            (hour (substring str 9 11))
            (minute (substring str 11 13))
            (sec (substring str 13 15)))
        (ignore-errors
          (and (read)
               (legit-dash dash)
               (legit-time time)))))))

(my/ts-check "2020")

(ignore-errors
  (and
   (> (read "2020") 3)
   (> "a" "b")))
(integerp 30)
(type-of (read "-"))
