;;; tsp-export.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(defvar tsp:default-buffer
  (get-buffer-create "*TSP*"))

(defun tsp:overwrite-buffer (str)
  (with-current-buffer tsp:default-buffer
    (read-only-mode -1)
    (org-mode)
    (erase-buffer)
    (insert str)
    (read-only-mode 1)))

;; (defun tsp:export-file-prop (file-prop)
;;   "TODO I don't yet know how to extract and embed org subtrees.
;; So currently I wrote a boiler plate. But ultimately, I should get
;; back and write a proper #'export-file-prop.

;;   Expect input to be the output of #'TSP:FILE-PROP.")

(defun tsp:export-ts-prop-to-string (ts)
  "
This exporter should render an org buffer:
* ts
** files
*** f1
...
...
*** f2
...
...
** related ts
+ [[tsl:20201010-010101]]
+ [[tsl:20210101-101101]]
"
  (let ((files (tsp:files<-ts ts))
        (ts-list nil))
    (apply #'concat

           (concatenate
            'list

            (list (concat "* " ts)
                  (concat "\n")
                  (concat "** " "files"))

            (loop
             for file in files
             collect (let* ((prop (tsp:file-prop file))
                            (abs-path (plist-get prop :abs-path))
                            (extension (plist-get prop :extension))
                            (size (plist-get prop :size))
                            (last-update (plist-get prop :last-update))
                            (timestamps (plist-get prop :timestamps))
                            (org-title (plist-get prop :org-title))
                            (org-header (plist-get prop :org-header))
                            (org-links (plist-get prop :org-links)))

                       (setf ts-list (concatenate 'list ts-list timestamps))
                       (format
                        "
*** %s
:PROPERTIES:
:SIZE: %s
:LAST-UPDATE: %s
:TIMESTAMPS: %s
:END:

**** path
[[file:%s]]

**** org-header
%s

**** org-links
%s
"
                        (f-filename file)
                        (file-size-human-readable size)
                        last-update
                        timestamps
                        abs-path
                        org-header
                        org-links)))

            (list (concat "\n" "** " "related ts"
                          "\n" (format "%s" ts-list) ;TODO still need to turn into tsl links
                          "\n\n"))))))


(defun tsp:export-ts-prop-to-buffer (ts-list)
  (tsp:overwrite-buffer
   (apply #'concat
          (loop for ts in ts-list
                collect (tsp:export-ts-prop-to-string ts))))
  (switch-to-buffer-other-window tsp:default-buffer))

;; TEST
;; (tsp:export-ts-prop-to-buffer
;;  '(
;;    "20111028-152106"
;;    "20111028-152419"
;;    "20210327-081236"
;;    ))

(provide 'tsp-export)
