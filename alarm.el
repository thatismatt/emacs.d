;;; alarm --- Display a message in a new frame at a specified time.

;;; Commentary:
;; Use Emacs as an alarm.

;;; Todo:
;; Allow the setting of multiple alarms.

;;; Code:

(eval-when-compile (require 'cl))

(defvar alarm-alist nil
  "An alist of alarms.")

(defvar alarm-buffer "*alarm*")

(defun alarm-action (message)
  "The actual alarm action.
Displays MESSAGE in `alarm-buffer'."
  (progn
    (switch-to-buffer-other-frame alarm-buffer)
    (goto-char (point-max))
    (insert "\n\n\n### ALARM ###\n\n")
    (insert message)))

(defun alarm (time message)
  "Set an alarm.
An alarm will occur at TIME with the MESSAGE.
The time format is the same accepted by `run-at-time'.
For example \"11:30am\"."
  (interactive "sTime: \nsMessage: ")
  (let ((alarm-timer (run-at-time time nil 'alarm-action message)))
    (add-to-list 'alarm-alist `(,time ,message ,alarm-timer))))

(defun alarm-cancel (time)
  "Cancel the alarm clock set for TIME."
  (interactive "sTime: ")
  (let ((a (alarm-get time)))
    (if a
        (progn (cancel-timer (caddr a))
               (setq alarm-alist (delq a alarm-alist))
               (message "Cancelled alarm set for %s." time))
      (message "No alarm set for %s." time))))

(defun alarm-get (time)
  "Get the alarm set for TIME."
  (assoc time alarm-alist))

(provide 'alarm)

;;; alarm.el ends here
