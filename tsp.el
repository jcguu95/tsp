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
          :title (my/parse-org-title main-note)))

  ;; return timestamp it points to
  ;; TODO Again it boils down to org parsing..
  )

;; testing
(tsp:search "20181229-000000")

(defun my/parse-org-title (file)
  (let* ((data (with-temp-buffer
                 (goto-char (point-max))
                 (insert (f-read file))
                 (goto-char (point-min))
                 (org-element-parse-buffer)))
         (keyword (org-element-map data 'keyword 'identity nil t)))
    (and keyword (org-element-property :value keyword))))

(loop for file in (f-files "~/data/storage/+org/wiki")
      collect (my/parse-org-title file))
