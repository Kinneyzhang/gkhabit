;;; gk-habit.el --- Habit record and review in emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: habit record
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: http://github.com/Kinneyzhang/gk-habit
;; Package-Requires: ((emacs "26.3") (valign) (emacsql))

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

;;; Commentary: Record and review habit in org table and graph.

;; 

;;; Code:

(require 'cl-lib)
(require 'emacsql)
(require 'valign)
(require 'emacsql-sqlite3)

(defconst gkh-buffer "*Gk-Habit*")

(defvar gkh-cjk-table-align nil)

(defconst gkh-table-report-buffer "*Habit-Table-Report*")

(setq gkh-db (emacsql-sqlite3 "~/.emacs.d/gk-habit/habit.db"))

(defvar gkh-file "~/.emacs.d/gk-habit/habit.org")

(defvar gkh-weekdays '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defvar gkh-weekdays-2 '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

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

(defun gkh-get-today-habits ()
  "Get a list of habit need to be recorded today."
  (let ((all-habits (mapcar 'car (emacsql gkh-db [:select name :from habit :where (= status "Active")])))
	today-habits)
    (dolist (habit all-habits)
      (let ((frequency-type (caar (emacsql gkh-db `[:select frequency-type :from habit :where (= name ,habit)])))
	    (frequency-param (caar (emacsql gkh-db `[:select frequency-param :from habit :where (= name ,habit)]))))
	(cond
	 ((string= "everyday" frequency-type)
	  (push habit today-habits))
	 ((string= "repeat" frequency-type)
	  (setq day-of-week-nums (split-string frequency-param "" t "^ +"))
	  (setq today-of-week-num (format-time-string "%u"))
	  (when (member today-of-week-num day-of-week-nums)
	    (push habit today-habits)))
	 ((member frequency-type '("by-week" "by-month"))
	  (setq today-habits (reverse (push habit today-habits))))))) ;; hack: how many times left.
    today-habits))

(defun gkh-get-unrecord-habits ()
  "Get a list of habit unrecorded today."
  (let ((today-habits (gkh-get-today-habits))
	(recorded-habits (mapcar 'car (emacsql gkh-db `[:select habit :from record :where (like create-time ,(concat (format-time-string "%Y-%m-%d") "%"))]))))
    (remove-if `(lambda (x) (member x ',recorded-habits)) today-habits)))

(defun gkh-record ()
  "Insert a habit redord in table."
  (interactive)
  (let* ((create-time (format-time-string "%Y-%m-%d %T"))
	 (habit (completing-read "Choose a habit: " (gkh-get-unrecord-habits)))
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
    (gk-habit-mode)
    (read-only-mode -1)
    (erase-buffer)
    (insert (file-contents gkh-file))
    (valign-mode)
    (goto-char (point-min))
    (read-only-mode 1))
  (view-buffer gkh-buffer 'kill-buffer))

;; --------------------------------------------
;; weekly table report
(defun gkh-current-week-first-day ()
  "Get the first day of current week."
  (let* ((current-second (time-convert nil 'integer))
	 (day-of-week (string-to-number (format-time-string "%u")))
	 (first-day-second (- current-second (* (1- day-of-week) 86400))))
    first-day-second))

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

(defun gkh--get-achieved-rate (habit first-date last-date)
  "Calculate achieved rate of the habit."
  (let ((frequency-type (caar (emacsql gkh-db `[:select frequency-type :from habit :where (= name ,habit)])))
	(frequency-param (caar (emacsql gkh-db `[:select frequency-param :from habit :where (= name ,habit)])))
	achieved-times goal-times)
    (setq achieved-times
	  (caar (emacsql gkh-db `[:select (funcall count *) :from record
					  :where (and (<= (like ,(concat first-date " %")) create-time ,(concat last-date " %"))
						      (= habit ,habit)
						      (= status "DONE"))])))
    (cond
     ((string= frequency-type "everyday")
      (setq goal-times 7)
      (concat (car (split-string (number-to-string (* (/ (float achieved-times) (float goal-times)) 100)) "\\.")) "%"))
     ((string= frequency-type "repeat")
      (setq goal-times (length (split-string frequency-param "" t "^ +")))
      (concat (car (split-string (number-to-string (* (/ (float achieved-times) (float goal-times)) 100)) "\\.")) "%"))
     ((member frequency-type '("by-week" "by-month"))
      (setq goal-times (string-to-number frequency-param))
      (concat (car (split-string (number-to-string (* (/ (float achieved-times) (float goal-times)) 100)) "\\.")) "%")))))

(defun gkh--get-habits-in-range (habits end-date)
  "Get current week's habits."
  (let (habit-list)
    (dolist (habit habits)
      (let* ((create-time (caar (emacsql gkh-db `[:select create-time :from habit :where (= name ,habit)])))
	     (create-time-day-num (date-to-day create-time)))
	(when (<= create-time-day-num (date-to-day end-date))
	  (push habit habit-list))))
    (reverse habit-list)))

(defun gkh-weekly-table-report (seconds)
  "Habit weekly report in org table"
  (gkh-org-table-draw)
  (select-window
   (or (get-buffer-window gkh-table-report-buffer)
       (selected-window)))
  (with-current-buffer (get-buffer-create gkh-table-report-buffer)
    (gk-habit-mode)
    (read-only-mode -1)
    (erase-buffer)
    (setq-local gkh-report-type "weekly")
    (setq-local gkh-first-day seconds)
    (let* ((first-of-week-date (format-time-string "%Y-%m-%d" gkh-first-day))
	   (last-of-week-date (format-time-string "%Y-%m-%d" (+ gkh-first-day (* 6 86400))))
	   (all-habits (mapcar 'car (emacsql gkh-db `[:select name :from habit
							      :where (= status "Active")])))
	   (habits (gkh--get-habits-in-range all-habits (concat last-of-week-date " 00:00:00"))))
      (insert (concat "* " first-of-week-date " ~ " last-of-week-date "\n\n"))
      (org-table-create (concat "10x" (number-to-string (1+ (length habits)))))
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
	(org-table-next-field)
	(insert "Achieved")
	(dotimes (i (length habits))
	  (let* ((habit-first-record-date (caar (emacsql gkh-db `[:select create-time :from record
									  :where (= habit ,(nth i habits))
									  :order-by (asc create-time)])))
		 (habit-create-date (caar (emacsql gkh-db `[:select create-time :from habit
								    :where (= name ,(nth i habits))])))
		 (habit-first-record-date (if (null habit-first-record-date)
					      habit-create-date
					    habit-first-record-date))
		 (freq-type (caar (emacsql gkh-db `[:select frequency-type :from habit
							    :where (= name ,(nth i habits))])))
		 (freq-param (caar (emacsql gkh-db `[:select frequency-param :from habit
							     :where (= name ,(nth i habits))])))
		 (first-record-date-num (date-to-day habit-first-record-date))
		 current-date-num)
	    (org-table-next-field)
	    (insert (nth i habits))
	    (org-table-next-field)
	    (insert (gkh--format-frequency (nth i habits)))
	    (dotimes (j 7)
	      (let* ((nth-of-week-date (format-time-string "%Y-%m-%d" (+ (* j 86400) gkh-first-day)))
		     (status (caar (emacsql gkh-db
					    `[:select status :from record
						      :where (and (= habit ,(nth i habits))
								  (like create-time
									,(concat nth-of-week-date "%")))]))))
		(setq current-date-num (date-to-day (concat nth-of-week-date " 00:00:00")))
		(org-table-next-field)
		(if (>= current-date-num first-record-date-num)
		    (cond
		     ((string= status "DONE")
		      (insert "√"))
		     ((string= status "MISS")
		      (insert "×"))
		     ((string= "everyday" freq-type)
		      (insert "·"))
		     ((string= "repeat" freq-type)
		      (when (member (1+ j) (mapcar 'string-to-number (split-string freq-param "" t "^ +")))
			(insert "·"))))
		  (insert "-"))))
	    (org-table-next-field)
	    (let ((last-of-week-day-num (date-to-day (concat last-of-week-date " 00:00:00"))))
	      (when (>= last-of-week-day-num first-record-date-num)
		(insert (gkh--get-achieved-rate (nth i habits) first-of-week-date last-of-week-date))))))))
    (org-table-align)
    (if gkh-cjk-table-align
	(valign-mode)
      (valign-mode -1))
    (forward-line)
    (newline)
    (insert "» [W] weekly report  » [M] monthly report  » go back today\n» [f] next  » [b] previous  » [q] quit  » [g] refresh\n")
    (read-only-mode)
    (goto-char (point-min)))
  (switch-to-buffer gkh-table-report-buffer)
  seconds)

(defun gkh--get-earliest-week-date ()
  "Get the earliest record week's first date."
  (let* ((time-spec (date-to-time (caar (emacsql gkh-db [:select create-time :from record :order-by (asc create-time)]))))
	 (day-of-week (string-to-number (format-time-string "%u" time-spec)))
	 (seconds (- (time-convert time-spec 'integer) (* (1- day-of-week) 86400))))
    (format-time-string "%Y-%m-%d" seconds)))

(defun gkh-report-current-week ()
  "Switch to current weekly table report view."
  (interactive)
  (with-current-buffer (get-buffer-create gkh-table-report-buffer)
    (setq-local gkh-first-day (gkh-weekly-table-report (gkh-current-week-first-day)))
    (message "Current weekly report.")))

(defun gkh-report-previous-week ()
  "Switch to previous weekly table report view."
  (interactive)
  (with-current-buffer (get-buffer-create gkh-table-report-buffer)
    (let ((gkh-first-day-date (format-time-string "%Y-%m-%d" gkh-first-day))
	  (current-week-first-day-date (gkh--get-earliest-week-date)))
      (if (string= gkh-first-day-date current-week-first-day-date)
	  (message "Already in the earliest week.")
	(setq-local gkh-first-day (gkh-weekly-table-report (- gkh-first-day (* 7 86400))))))))

(defun gkh-report-next-week ()
  "Switch to next weekly table report view."
  (interactive)
  (with-current-buffer (get-buffer-create gkh-table-report-buffer)
    (let ((gkh-first-day-date (format-time-string "%Y-%m-%d" gkh-first-day))
	  (current-week-first-day-date (format-time-string "%Y-%m-%d" (gkh-current-week-first-day))))
      (if (string= gkh-first-day-date current-week-first-day-date)
	  (message "Already in the latest week.")
	(setq-local gkh-first-day (gkh-weekly-table-report (+ gkh-first-day (* 7 86400))))))))

(defun gkh-report-refresh-week ()
  "Refresh current viewing report."
  (interactive)
  (with-current-buffer (get-buffer-create gkh-table-report-buffer)
    (gkh-weekly-table-report gkh-first-day)
    (message "Weekly report refreshed.")))

;;-----------------------------------------------------------
;; monthly table report
(defun gkh--get-month-day-num (year month)
  "Get the number of a month's days."
  (let ((year (if (stringp year) (string-to-number year) year))
	(month (if (stringp month) (string-to-number month) month)))
    (cond
     ((member month '(1 3 5 7 8 10 12))
      31)
     ((member month '(4 6 9 11))
      30)
     ((= month 2)
      (if (date-leap-year-p year)
	  29 28)))))

(defun gkh-draw-table (LIST)
  "Draw org table from a LIST form."
  (let ((column (catch 'break
		  (dolist (row-data LIST)
		    (when (listp row-data)
		      (throw 'break (length row-data))))))
	(beg (point)))
    (org-table-create (concat (number-to-string column) "x1"))
    (goto-char beg)
    (when (org-at-table-p)
      (org-table-next-field)
      (dotimes (i (length LIST))
	(when (nth i LIST)
	  (let ((row-data (nth i LIST)))
	    (if (listp row-data)
		(dolist (data row-data)
		  (cond
		   ((numberp data)
		    (insert (number-to-string data)))
		   ((null data)
		    (insert ""))
		   (t (insert data)))
		  (org-table-next-field))
	      (when (equal 'hl row-data)
		(org-table-insert-hline 1)))
	    (when (= i (1- (length LIST)))
	      (org-table-kill-row))
	    ))))
    (forward-line)))

(defun gkh-sequence-offset-and-group (sequence offset group)
  (let ((offset (if (stringp offset)
		    (string-to-number offset)
		  offset))
	(seq-num (length sequence))
	end-offset
	groups-len)
    (dotimes (i offset)
      (setq sequence (cons nil sequence)))
    (setq groups-len (length (seq-partition sequence group)))
    (setq end-offset (- (* group groups-len) offset seq-num))
    (dotimes (i end-offset)
      (setq sequence (append sequence (list nil))))
    (seq-partition sequence group)))

(defun gkh-month-date-group (year month offset)
  "Generate month's weekly number sequence."
  (let* ((day-num (gkh--get-month-day-num year month))
	 (number-seq (number-sequence 1 day-num)))
    (gkh-sequence-offset-and-group number-seq offset 7)))

(defun gkh-month-combine-date-and-status (year month offset status-list)
  "Combine month's date and status."
  (let ((date-groups (gkh-month-date-group year month offset))
	(status-groups (gkh-sequence-offset-and-group status-list offset 7))
	hl-date-status-list)
    (dotimes (i (length status-groups))
      (setq hl-date-status-list (append hl-date-status-list (cons 'hl (cons (nth i date-groups) (list (nth i status-groups)))))))
    hl-date-status-list))

(defun gkh-monthly-table-report (seconds)
  "Habits monthly report in org table."
  (gkh-org-table-draw)
  (select-window
   (or (get-buffer-window gkh-table-report-buffer)
       (selected-window)))
  (with-current-buffer (get-buffer-create gkh-table-report-buffer)
    (gk-habit-mode)
    (read-only-mode -1)
    (erase-buffer)
    (setq-local gkh-report-type "monthly")
    (setq-local gkh-first-day seconds)
    (let* ((year (format-time-string "%Y" gkh-first-day))
	   (month (format-time-string "%m" gkh-first-day))
	   (month-string (format-time-string "%B" gkh-first-day))
	   (day-of-week (format-time-string "%w" gkh-first-day)) 
	   (month-day-num (gkh--get-month-day-num year month))
	   (last-of-month-date (format-time-string "%Y-%m-%d" (+ gkh-first-day (* (1- (gkh--get-month-day-num year month)) 86400))))
	   (all-habits (mapcar 'car (emacsql gkh-db `[:select name :from habit
							      :where (= status "Active")])))
	   (habits (gkh--get-habits-in-range all-habits (concat last-of-month-date " 00:00:00")))
	   pos)
      (goto-char (point-min))
      (insert (concat "* " month-string ", " year "\n\n"))
      (insert "» [W] weekly report  » [M] monthly report  » go back today\n» [f] next  » [b] previous  » [q] quit  » [g] refresh\n\n")
      (dolist (habit habits)
	(let* ((habit-create-date (caar (emacsql gkh-db `[:select create-time :from habit
								  :where (= name ,habit)])))
	       (habit-create-date-num (date-to-day habit-create-date))
	       status-list
	       date-status-list)
	  (dotimes (i (gkh--get-month-day-num year month))
	    (let* ((nth-of-month-date (format-time-string "%Y-%m-%d" (+ (* i 86400) gkh-first-day)))
		   (current-date-num (date-to-day (concat nth-of-month-date " 00:00:00")))
		   (status (caar (emacsql gkh-db `[:select status :from record
							   :where (and (= habit ,habit)
								       (= (like create-time
										,(concat year "-" month
											 (if (= (length (number-to-string (1+ i))) 1)
											     (format "-0%s %%" (1+ i))
											   (format "-%s %%" (1+ i)))))))]))))
	      (if (>= current-date-num habit-create-date-num)
		  (cond
		   ((string= status "DONE")
		    (push "√" status-list))
		   ((string= status "MISS")
		    (push "×" status-list))
		   (t (push "." status-list))
		   ;; ((string= "everyday" freq-type)
		   ;;  (push "." status-list))
		   ;; ((string= "repeat" freq-type)
		   ;;  (when (member (1+ j) (mapcar 'string-to-number (split-string freq-param "" t "^ +")))
		   ;; 	(insert "·")))
		   )
		(push "-" status-list))))
	  (setq date-status-list (gkh-month-combine-date-and-status year month day-of-week (reverse status-list)))
	  (insert (concat "** " habit "\n\n"))
	  (gkh-draw-table
	   `(,gkh-weekdays-2 ,@date-status-list))
	  (insert "\n"))))
    (if gkh-cjk-table-align
	(valign-mode)
      (valign-mode -1))
    (read-only-mode)
    (goto-char (point-min)))
  (switch-to-buffer gkh-table-report-buffer)
  seconds)

(defun gkh-current-month-first-day ()
  "Get the first day of current week."
  (let* ((current-second (time-convert nil 'integer))
	 (day-of-month (string-to-number (format-time-string "%d")))
	 (first-day-second (- current-second (* (1- day-of-month) 86400))))
    first-day-second))

(defun gkh--get-earliest-month ()
  "Get the earliest record month"
  (let* ((time-spec (date-to-time (caar (emacsql gkh-db [:select create-time :from record :order-by (asc create-time)]))))
	 (month (format-time-string "%m" time-spec)))
    month))

(defun gkh-report-current-month ()
  "Switch to current monthly table report view."
  (interactive)
  (with-current-buffer (get-buffer-create gkh-table-report-buffer)
    (setq-local gkh-first-day (gkh-monthly-table-report (gkh-current-month-first-day)))
    (message "Current monthly report.")))

(defun gkh-report-previous-month ()
  "Switch to previous monthly table report view."
  (interactive)
  (with-current-buffer (get-buffer-create gkh-table-report-buffer)
    (let* ((current-month (format-time-string "%m" gkh-first-day))
	   (current-year (format-time-string "%Y" gkh-first-day))
	   (previous-month-day-num (gkh--get-month-day-num current-year (1- (string-to-number current-month))))
	   (earlist-month (gkh--get-earliest-month)))
      (if (string= current-month "01")
	  (message "Alreay the first month.")
	(if (string= current-month earlist-month)
	    (message "Already in the earliest month.")
	  (setq-local gkh-first-day (gkh-monthly-table-report (- gkh-first-day (* previous-month-day-num 86400)))))))))

(defun gkh-report-next-month ()
  "Switch to previous monthly table report view."
  (interactive)
  (with-current-buffer (get-buffer-create gkh-table-report-buffer)
    (let* ((current-month (format-time-string "%m" gkh-first-day))
	   (current-year (format-time-string "%Y" gkh-first-day))
	   (current-month-day-num (gkh--get-month-day-num current-year current-month))
	   (latest-month (format-time-string "%m")))
      (if (string= current-month "12")
	  (message "Alreay the last month.")
	(if (string= current-month latest-month)
	    (message "Already in the latest month.")
	  (setq-local gkh-first-day (gkh-monthly-table-report (+ gkh-first-day (* current-month-day-num 86400)))))))))

(defun gkh-report-refresh-month ()
  "Refresh current viewing report."
  (interactive)
  (with-current-buffer (get-buffer-create gkh-table-report-buffer)
    (gkh-monthly-table-report gkh-first-day)
    (message "Monthly report refreshed.")))

(defun gkh-backward-report-view ()
  (interactive)
  (with-temp-buffer
    (set-buffer gkh-table-report-buffer)
    (cond ((string= gkh-report-type "weekly")
	   (funcall 'gkh-report-previous-week))
	  ((string= gkh-report-type "monthly")
	   (funcall 'gkh-report-previous-month)))))

(defun gkh-forward-report-view ()
  (interactive)
  (with-temp-buffer
    (set-buffer gkh-table-report-buffer)
    (cond ((string= gkh-report-type "weekly")
	   (funcall 'gkh-report-next-week))
	  ((string= gkh-report-type "monthly")
	   (funcall 'gkh-report-next-month)))))

(defun gkh-today-report-view ()
  (interactive)
  (with-temp-buffer
    (set-buffer gkh-table-report-buffer)
    (cond ((string= gkh-report-type "weekly")
	   (funcall 'gkh-report-current-week))
	  ((string= gkh-report-type "monthly")
	   (funcall 'gkh-report-current-month)))))

(defun gkh-refresh-report-view ()
  (interactive)
  (with-temp-buffer
    (set-buffer gkh-table-report-buffer)
    (cond ((string= gkh-report-type "weekly")
	   (funcall 'gkh-report-refresh-week))
	  ((string= gkh-report-type "monthly")
	   (funcall 'gkh-report-refresh-month)))))

(defvar gk-habit-mode-map nil "Keymap for `gk-habit-mode'")

(progn
  (setq gk-habit-mode-map (make-sparse-keymap))
  (define-key gk-habit-mode-map (kbd "q") 'kill-current-buffer)
  (define-key gk-habit-mode-map (kbd "M") 'gkh-report-current-month)
  (define-key gk-habit-mode-map (kbd "W") 'gkh-report-current-week)
  (define-key gk-habit-mode-map (kbd "b") 'gkh-backward-report-view)
  (define-key gk-habit-mode-map (kbd "f") 'gkh-forward-report-view)
  (define-key gk-habit-mode-map (kbd ".") 'gkh-today-report-view)
  (define-key gk-habit-mode-map (kbd "g") 'gkh-refresh-report-view))

(define-derived-mode gk-habit-mode org-mode "gkh"
  (use-local-map gk-habit-mode-map))

(provide 'gk-habit)
;;; gk-habit.el ends here
