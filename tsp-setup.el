;;; tsp-setup.el -mode -*- coding: utf-8; lexical-binding: t; -*-

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

(provide 'tsp-setup)
