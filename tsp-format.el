;;; tsp-format.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(defun tsp:check-ts-format (str)
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
  ;; (mapcar #'tsp:check-ts-format '("2020" "20210731-150156")) ;; all t
  ;; (mapcar #'tsp:check-ts-format '("20200731-" "20210731-150161")) ;; all nil
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

(defvar tsp:full-ts-format
  (rx-to-string
   `(seq (repeat 8 digit)
         "-"
         (repeat 6 digit)))
  "The expected timestring format in this program. For example,
  \"20201130-153450\" is a legit timestring.")

(defun tsp:check-full-ts-format (ts)
  "Check if TS is a string that follows the full timestamp
format."
  (string-match tsp:full-ts-format ts))

(provide 'tsp-format)
