;;; gk-habit.el --- Habit record and review in emacs. -*- lexical-binding: t; -*-b

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: habit record
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: http://github.com/Kinneyzhang/gk-habit
;; Package-Requires: ((emacs "26.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;;; Code:

(require 'cl-lib)
(require 'emacsql)
(require 'emacsql-sqlite3)

(defconst gkh-buffer "*Gk-Habit*")

(defconst gkh-weekly-report-buffer "*Habit-Weekly-Report*")

(setq gkh-db (emacsql-sqlite3 "~/.emacs.d/gk-habit/habit.db"))

(defvar gkh-file "~/.emacs.d/gk-habit/habit.org")

(setq gkh-weekdays '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defvar gkh-record-status '("DONE" "MISS"))

(defvar gkh-frequency-type '("everyday" "repeat" "by-week" "by-month"))

(defvar gkh-period '("get-up" "morning" "noon" "afternoon" "evening" "before-sleep"))

(setq gkh-table-habit-column '("create-time" "name" "frequency-type" "frequency-param" "period" "remind-time" "remind-string" "status" "archive-time"))

(setq gkh-table-record-column '("create-time" "habit" "status" "comment"))

(defun gkh-db-create-tables ()
  "Create database tables of gk-habit."  
  (interactive)
  (emacsql gkh-db [:create-table habit
				 ([(create-time string :primary-key :not-null)
				   (name string :not-null :unique)
				   (frequency-type string :not-null)
				   (frequency-param)
				   (period string :not-null)
				   (remind-time)
				   (remind-string)
				   (status string :not-null)
				   (archive-time string)])])
  
  (emacsql gkh-db [:create-table record
				 ([(create-time string :primary-key :not-null)
				   (habit string :not-null)
				   (status string :not-null)
				   (comment string)]
				  (:foreign-key [habit] :references habit [name]
						:on-delete :cascade))]))

(defun gkh-db-drop-tables ()
  "Drop database tables of gk-habit."
  (interactive)
  (let* ((tables (emacsql gkh-db [:select name
					  :from sqlite_master
					  :where (= type 'table)]))
	 (table (completing-read "Choose a table: " tables nil t)))
    (emacsql gkh-db `[:drop-table ,(intern table)])))

(defun gkh--frequency-params (frequency-type)
  "Get the habit frequency parameters"
  (let (param)
    (cond
     ((string= frequency-type "everyday")
      (setq param nil))
     ((string= frequency-type "repeat")
      (setq param (completing-read "repeated day (exam. \"135\" means habit repeat on Monday, Wensday and Friday in every week.): " nil)))
     ((string= frequency-type "by-week")
      (setq param (completing-read "how many times a week: " nil)))
     ((string= frequency-type "by-month")
      (setq param (completing-read "how many times a month: " nil))))
    param))

(defun gkh-init ()
  "Gk-habit initialize, create database and org tables."
  (interactive)
  (ignore-errors (gkh-db-create-tables))
  (gkh-org-table-draw))

(defun gkh-new ()
  "Add a new habit"
  (interactive)
  (cl-block 'return
    (let* ((create-time (format-time-string "%Y-%m-%d %T"))
	   (habit
	    (let ((temp (completing-read "name of habit: " nil))
		  (habits (mapcar 'car (emacsql gkh-db [:select name :from habit
								:where (= status "Active")]))))
	      (if (member temp habits)
		  (cl-return-from 'return
		    (message "the habit '%s' already exist!" temp))
		temp)))
	   (frequency-type (completing-read "frequency of habit: " gkh-frequency-type nil t))
	   (frequency-param (gkh--frequency-params frequency-type))
	   (period  (completing-read "period of habit: " gkh-period nil t))
	   (remind-time
	    (let ((temp (completing-read "remind this habit at: " nil)))
	      (if (string= "" temp)
		  nil temp)))
	   (remind-string
	    (let ((temp (completing-read "habit remind sentence: " nil)))
	      (if (string= "" temp)
		  nil temp))))
      (emacsql gkh-db `[:insert :into habit
				:values ([,create-time ,habit ,frequency-type ,frequency-param ,period ,remind-time ,remind-string "Active" nil])])
      (gkh-org-table-draw)
      (message "Habit '%s' is added!" habit))))

;; ------------------------------------------------------------

(defun gkh-record-today-todos ()
  "Get a list of habit need to be recorded today."
  ;; active habit and need to be recorded today and some maybe habit.
  )

(defun gkh-record-current-todos ()
  "Get a list of habit need to be recorded now."
  (let ((today-habits (mapcar 'car (emacsql gkh-db `[:select name :from habit])))
	(recorded-habits (mapcar 'car (emacsql gkh-db `[:select habit :from record :where (like create-time ,(concat (format-time-string "%Y-%m-%d") "%"))]))))
    (remove-if `(lambda (x) (member x ',recorded-habits)) today-habits)))

(defun gkh-record ()
  "Insert a habit redord in table."
  (interactive)
  (let* ((create-time (format-time-string "%Y-%m-%d %T"))
	 (habit (completing-read "Choose a habit: " (gkh-record-current-todos)))
	 (status (completing-read "Is the habit done?" gkh-record-status nil t))
	 (comment
	  (if (string= "MISS" status)
	      (completing-read "Reason why missed: " nil)
	    (completing-read "Say something: " nil))))
    (emacsql gkh-db `[:insert-into record
				   :values ([,create-time ,habit ,status ,comment])])
    (gkh-org-table-draw)
    (message "Habit '%s' is %s, record on %s, %s" habit status create-time comment)))

(defun gkh-archive ()
  "Archive a habit"
  (interactive)
  (let* ((habits (emacsql gkh-db [:select name :from habit
					  :where (= status "Active")]))
	 (habit (completing-read "Choose a habit: " habits nil t)))
    (emacsql gkh-db `[:update habit
			      :set [(= status "Archive") (= archive-time ,(format-time-string "%Y-%m-%d %T"))]
			      :where (= name ,habit)])
    (gkh-org-table-draw)
    (message "habit %s has been archived!" habit)))

(defun gkh-org-table-draw ()
  "Draw gk-habit database in org table."
  (interactive)
  (let* ((table-alist '(("habit" . gkh-table-habit-column)
			("record" . gkh-table-record-column))))
    (with-temp-file gkh-file
      (goto-char (point-min))
      (dotimes (i (length table-alist))
	(let* ((headline (car (nth i table-alist)))
	       (column-list (eval (cdr (nth i table-alist))))
	       (column-num (length column-list)))
	  (insert (concat "* " headline " table\n"))
	  (org-table-create (concat (format "%s" column-num) "x2"))
	  (dotimes (j column-num)
	    (org-table-next-field)
	    (insert (nth j column-list)))
	  (let ((items (emacsql gkh-db `[:select * :from ,(intern headline)])))
	    (dotimes (m (length items))
	      (dotimes (n column-num)
		(org-table-next-field)
		(insert (format "%s" (nth n (nth m items)))))))
	  (org-table-align)
	  (forward-line 2)
	  (end-of-line)
	  (newline 2))))))

(defun gkh-org-table-display ()
  "Display gk-habit org table in a view buffer."
  (interactive)
  (gkh-org-table-draw)
  (when (string= (buffer-name) gkh-buffer)
    (message "Already in the Gk Habit buffer."))
  (select-window
   (or (get-buffer-window gkh-buffer)
       (selected-window)))
  (ignore-errors (kill-buffer gkh-buffer))
  (with-current-buffer (get-buffer-create gkh-buffer)
    (org-mode)
    (read-only-mode -1)
    (erase-buffer)
    (insert (file-contents gkh-file))
    (valign-mode)
    (goto-char (point-min))
    (read-only-mode 1))
  (view-buffer gkh-buffer 'kill-buffer))

;; --------------------------------------------

(defun gkh-current-week-days ()
  "Get current week's days"
  (let* ((current-date (format-time-string "%Y-%m-%d"))
	 (current-second (time-convert nil 'integer))
	 (day-of-week (string-to-number (format-time-string "%u")))
	 (first-of-week-second (- current-second (* (1- day-of-week) 86400)))
	 (last-of-week-second (+ first-of-week-second (* 6 86400))))
    (cons first-of-week-second last-of-week-second)))

(defun gkh--format-frequency (habit)
  "Format habit frequency string."
  (let ((frequency-type (caar (emacsql gkh-db `[:select frequency-type :from habit :where (= name ,habit)])))
	(frequency-param (caar (emacsql gkh-db `[:select frequency-param :from habit :where (= name ,habit)]))))
    (cond
     ((string= frequency-type "everyday")
      frequency-type)
     ((string= frequency-type "repeat")
      (let* ((day-nums (mapcar 'string-to-number (split-string frequency-param "" t "^ +")))
	     (week-days (mapcar (lambda (i) (nth (1- i) gkh-weekdays)) day-nums)))
	(concat "repeat on " (string-join week-days ", "))))
     ((member frequency-type '("by-week" "by-month"))
      (concat frequency-param " times a " (cadr (split-string frequency-type "-")))))))

(defun gkh-weekly-table-report ()
  "Weekly report in org table"
  (interactive)
  (gkh-org-table-draw)
  (if (string= (buffer-name) gkh-weekly-report-buffer)
      (message "Already in the weekly report buffer."))
  (select-window
   (or (get-buffer-window gkh-weekly-report-buffer)
       (selected-window)))
  (ignore-errors (kill-buffer gkh-weekly-report-buffer))
  (with-current-buffer (get-buffer-create gkh-weekly-report-buffer)
    (org-mode)
    (let ((habits (mapcar 'car (emacsql gkh-db '[:select name :from habit :where (= status "Active")])))
	  (first-of-week-date (format-time-string "%Y-%m-%d"
						  (car (gkh-current-week-days))))
	  (last-of-week-date (format-time-string "%Y-%m-%d"
						 (cdr (gkh-current-week-days)))))
      (insert (concat "* " first-of-week-date " ~ " last-of-week-date "\n\n"))
      (org-table-create (concat "9x" (number-to-string (1+ (length habits)))))
      (goto-char (point-min))
      (skip-chars-forward "^|")
      (when (org-at-table-p)
	(org-table-next-field)
	(insert "Habit\\Week")
	(org-table-next-field)
	(insert "Frequency")
	(dotimes (i 7)
	  (org-table-next-field)
	  (insert (nth i gkh-weekdays)))
	(dotimes (i (length habits))
	  (org-table-next-field)
	  (insert (nth i habits))
	  (org-table-next-field)
	  (insert (gkh--format-frequency (nth i habits)))
	  (dotimes (j 7)
	    (let* ((nth-of-week-date (format-time-string "%Y-%m-%d"
							 (+ (* j 86400) (car (gkh-current-week-days)))))
		   (status (mapcar 'car
				   (emacsql gkh-db
					    `[:select status :from record
						      :where (and (= habit ,(nth i habits))
								  (like create-time
									,(concat nth-of-week-date "%")))]))))
	      (org-table-next-field)
	      (cond
	       ((string= (car status) "DONE")
		(insert "√"))
	       ((string= (car status) "MISS")
		(insert "×"))
	       (t (insert ""))))))))
    (valign-mode)
    (read-only-mode)
    (goto-char (point-min)))
  (view-buffer gkh-weekly-report-buffer 'kill-buffer))

(provide 'gk-habit)
;;; gk-habit.el ends here
