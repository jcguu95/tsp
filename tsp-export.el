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

;;;
;; (defun tsp:export-file-prop (file-prop)
;;   "TODO I don't yet know how to extract and embed org subtrees.
;; So currently I wrote a boiler plate. But ultimately, I should get
;; back and write a proper #'export-file-prop.

;;   Expect input to be the output of #'TSP:FILE-PROP.")
;;;

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
  ;; TODO make an option for it to read from db.
  (let ((file-props (plist-get
                     (tsp:ts-prop ts)
                     :file-props))
        ts-list)
    (apply #'concat

           (concatenate
            'list

            (list (concat "* " ts)
                  (concat "\n")
                  (concat "** " "files"))

            (loop
             for file-prop in file-props
             collect (let* ((abs-path (plist-get file-prop :abs-path))
                            (extension (plist-get file-prop :extension))
                            (size (plist-get file-prop :size))
                            (last-update (plist-get file-prop :last-update))
                            (timestamps (plist-get file-prop :timestamps))
                            (org-title (plist-get file-prop :org-title))
                            (org-header (plist-get file-prop :org-header))
                            (org-links (plist-get file-prop :org-links)))

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

                        (f-filename abs-path)
                        (file-size-human-readable size)
                        last-update
                        timestamps
                        abs-path
                        org-header
                        (apply #'concat
                               (loop for l in org-links
                                     collect (format "[[%s]]\n" l)))

                        )))

            (list (eval `(concat "\n" "** " "related ts"
                                 "\n" ,@(loop for ts in ts-list
                                              collect (format "[[tsl:%s]]\n" ts))
                                 "\n\n")))))))

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
