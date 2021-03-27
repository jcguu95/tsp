;;; tsp-org-parser.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(defun tsp:parse-org-data-from-file (file)
  "Expect FILE to be an org file. Return its org data."
  (with-temp-buffer
    (goto-char (point-max))
    (insert (f-read file))
    (goto-char (point-min))
    (org-element-parse-buffer)))

(defun tsp:get-org-title (file)
  "Expect FILE to be an org file with a title. Return the title
as a string."
  (let* ((data (tsp:parse-org-data-from-file file))
         (keyword (org-element-map data 'keyword 'identity nil t)))
    (and keyword (org-element-property :value keyword))))

(defun tsp:get-org-header (file)
  "Expect FILE to be an org file. Return its content, as a
string, up to the first headline."
  (let ((str (f-read file)))
    (subseq str 0 (string-match "\n\\*" str))))

(defun tsp:get-org-links (org-file)
  (org-element-map
      (tsp:parse-org-data-from-file org-file) 'link
    ;; TODO need to resolve links for file type - this is crucial
    (lambda (link)
      (org-element-property :raw-link link))))

(defun tsp:get-ts-from-org (org-file)
  "Expect ORG-FILE to be an org file. Parse all org timestamps in
it, and translate them into our ts format."
  (cl-labels ((ts<-time (time) (ts-format tsp:ts-format time)))
    (mapcar #'ts<-time
            (mapcar #'ts-parse-org
                    (org-element-map (tsp:parse-org-data-from-file org-file) 'timestamp
                      (lambda (ts) (org-element-property :raw-value ts)))))))

(provide 'tsp-org-parser)
