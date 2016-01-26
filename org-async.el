;;; org-async.el --- Parallel Processing for Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2016  William West

;; Author: William West <occidens@gmail.com>
;; Created: 23 Jan 2016

;; Keywords: outlines, hypermedia, wp
;; Homepage: https://github.com/occidens/org-async

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl-lib)
;(require 'log4e)

(defvar org-async-init-file nil
  "File used to intialize external export process.

Value must be either nil or an absolute file name. When nil, the
external process is launched like a regular Emacs session,
loading the user's initialization file and any site specific
configuration. If a file is provided, it, and only it, is loaded
at startup.

Therefore, using a specific configuration makes the process load
faster run more portably.")

(defvar org-async-debug nil)

(cl-defstruct (org-async-job
	       (:constructor nil)			; no default constructor
	       (:constructor
		org-async-create-job
		(origin fun &aux
			(coding (buffer-local-value
				 'buffer-file-coding-system
				 origin))
			(copy-fun (org-export--generate-copy-script origin))
			(timestamp (get-internal-run-time))))
	       (:copier nil))		;See `org-async-duplicate-job'
  (origin nil :read-only t)
  (coding nil :read-only t)
  (copy-fun nil :read-only t)
  (timestamp nil)
  (fun nil :read-only t)
  (file nil)
  (process nil)				;could change if the job fails and is retried.
  (callbacks nil)
  (status nil)
  (result nil))

(defun org-async-duplicate-job (job)
  "Return a duplicate of job.

The timestamp is updated to the current time."
  (let ((dup (copy-sequence job)))
    (setf (org-async-job-timestamp dup)
	  (get-internal-run-time))
    dup))

(defun org-async-create-export-job
    (buffer backend &optional subtreep visible-only body-only ext-plist)
  "Create a an export job from BUFFER using BACKEND.

The export parameters are specified with the optional arguments
SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST, which are
supplied to `org-export-as', which see."
  (org-async-create-job
   buffer
   `(lambda ()
      (require ',(intern (format "ox-%s" backend)))
      (org-export-as ',backend ,subtreep ,visible-only ,body-only ,ext-plist))))

(defun org-async-add-callback (job function &optional append)
  "Add FUNCTION to the callback slot of JOB.

If there is a single callback, it is stored directly in the slot;
otherwise the callbacks are stored in a list. If the optional
argument APPEND is non"
  (declare (indent 1))
  (let ((callbacks (org-async-job-callbacks job)))
    (when (or (not (listp callbacks)) (functionp callbacks))
      (setq callbacks (list callbacks)))
    (unless (member function callbacks)
      (setq callbacks
	    (if append
		(append callbacks (list function))
	      (cons function callbacks))))
    (setf (org-async-job-callbacks job) callbacks)))

(defun org-async-remove-callback (job function)
  "Remove FUNCTION from the callback slot of JOB."
  (let ((callbacks (org-async-job-callbacks job)))
    ;; Handle both the non-list and the list cases
    (if (or (not (listp callbacks)) (eq (car callbacks) 'lambda))
	(if (equal callbacks function)
	    (setf nil))
      (setq callbacks
	    (delete function (copy-sequence callbacks))))
    (setf (org-async-job-callbacks job) callbacks)))

(defun org-async-run-callbacks (job)
  (let ((callbacks (org-async-job-callbacks job)))
    (cond ((null callbacks) nil)
	  ((functionp callbacks)
	   (funcall callbacks job))
	  ((listp callbacks)
	   (mapcar (lambda (f) (funcall f job)) callbacks))
	  (t nil))))

;(log4e:deflogger "org-async" "%t [%l] %m" "%H:%M:%S")

(defun org-async--path-to-emacs ()
  (file-truename (expand-file-name invocation-name invocation-directory)))

(defun org-async--emacs-invocation (job)
  `(,(org-async--path-to-emacs)
    "--batch"
    "--eval=(setq debug-on-error t)"
    ,@(if org-async-init-file
	  `("-Q" "-l" ,org-async-init-file)
	`("-l" ,user-init-file))	;Because --batch implies -q
    "-l" ,(org-async-job-file job)))

;; TODO: see https://github.com/kiwanami/emacs-epc

(defun org-async--sentinel (job)
  "Return a process sentinel function for JOB

If a worker completes JOB successfully, this function will set
the slot `org-async-job-result' to the worker's output and
then call FINISH-FUN."
  (lambda (p status)
    (let ((proc-buffer (process-buffer p)))
      (when (eq 'exit
		(setf (org-async-job-status job)
		      (process-status p)))
	(unwind-protect
	    (if (zerop (process-exit-status p))
		(unwind-protect
		    (progn
		      (setf (org-async-job-result job)
			    (with-current-buffer proc-buffer
			      (goto-char (point-max))
			      (backward-sexp)
			      (read (current-buffer))))
		      (org-async-run-callbacks job))
		  (unless org-async-debug
		    (and (get-buffer proc-buffer)
			 (kill-buffer proc-buffer))))
	      (message "Process '%s' exited abnormally" p))
	  (unless org-async-debug
	    (delete-file (org-async-job-file job))))))))

(defun org-async--write-job-file (job)
  "Write out the job-file for JOB.

This file will be loaded into a subordinate Emacs process in
order to execute the job."
  (with-temp-file
      (setf (org-async-job-file job)
	    (make-temp-file "org-async-process"))
    (message "Org parallel jobfile: %s" (org-async-job-file job))
    (insert
     (format ";; -*- coding: %s; -*-\n%S"
	     (org-async-job-coding job)	;Due to potential null characters in copy-fun
	     `(with-temp-buffer
		;;,(when org-async-debug '(setq debug-on-error t))
		(setq debug-on-error t)
		;; Ignore `kill-emacs-hook' and code evaluation
		;; queries from Babel as we need a truly
		;; non-interactive process.
		(setq kill-emacs-hook nil
		      org-babel-confirm-evaluate-answer-no t)
		;; Initialize export framework
		(require 'ox)
		;; Re-create source buffer
		(prin1 "Setting up copy of source buffer...")
		(funcall ,(org-async-job-copy-fun job))
		(print "Done.")
		(restore-buffer-modified-p nil)
		;; Execute job and print result
		(print (funcall ,(org-async-job-fun job))))))))

(defun org-async--start-process (job)
  "Start a subordinate Emacs process to execute job.

Return the process object."
  (let ((process-connection-type nil))
    (setf (org-async-job-process job)
	  (apply #'start-process
		 "org-export-process"
		 (generate-new-buffer-name "*Org Parallel Process*")
		 (org-async--emacs-invocation job)))))

(defun org-async-start-worker (job)
  "Start a worker on JOB"
  (with-temp-message "Initializing asynchronous export process"
    (org-async--write-job-file job)
    (set-process-sentinel
     (org-async--start-process job)
     (org-async--sentinel job))
    job))

(defun org-async-result-to-buffer (job buffer-or-name)
  "Write the results of JOB to buffer BUFFER-OR-NAME.

If BUFFER-OR-NAME already exists, its contents are erased."
  (with-current-buffer (get-buffer-create buffer-or-name)
    (erase-buffer)
    (setq buffer-file-coding-system
	  (org-async-job-coding job))
    (insert (org-async-job-result job))
    (goto-char (point-min))))

(defun org-async-export-to-buffer
    (backend out-buffer
	     &optional subtreep visible-only body-only ext-plist
	     post-process)
  (declare (indent 2))
  (let ((job (org-async-create-export-job
	      (current-buffer)
	      backend subtreep visible-only body-only ext-plist)))
    (org-async-add-callback job
      (lambda (job)
	(org-async-result-to-buffer job out-buffer)
	(ignore-errors (funcall post-process))))
    (org-async-start-worker job)))

(provide 'org-async)
;;; org-async.el ends here
