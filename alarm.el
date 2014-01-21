;;; alarm --- Display a message in a new frame at a specified time.

;;; Commentary:
;; Use Emacs as an alarm.

;;; Todo:
;; Allow the setting of multiple alarms.

;;; Code:

(defvar alarm-clock-timer nil
  "The timer for the current alarm, kept so that the user can cancel it.")

(defvar alarm-clock-buffer "*alarm*")

(defun alarm-clock-action (message)
  "The actual alarm action.
The alarm's MESSAGE."
  (progn
    (switch-to-buffer-other-frame alarm-clock-buffer)
    (insert "\n\n### ALARM ###\n\n")
    (insert message)))

(defun alarm-clock (time message)
  "Set an alarm.
An alarm will occur at TIME with the MESSAGE.
The time format is the same accepted by `run-at-time'.
For example \"11:30am\"."
  (interactive "sTime: \nsMessage: ")
  (setq alarm-clock-timer (run-at-time time nil 'alarm-clock-action message)))

(defun alarm-clock-cancel ()
  "Cancel the alarm clock."
  (interactive)
  (cancel-timer alarm-clock-timer))

(provide 'alarm)

;;; alarm.el ends here
