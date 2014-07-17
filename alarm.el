;;; alarm --- Display a message in a new frame at a specified time.

;;; Commentary:
;; Use Emacs as an alarm.

;;; Code:

(eval-when-compile (require 'cl))

(defvar alarm-alist nil
  "An alist of alarms.")

(defvar alarm-buffer "*alarm*")

(defun alarm-action (message time)
  "The actual alarm action.
Displays MESSAGE (and TIME) in `alarm-buffer'."
  (progn
    (switch-to-buffer-other-frame alarm-buffer)
    (goto-char (point-max))
    (insert (format "\n### ALARM (%s) ###\n\n" time))
    (insert message)
    (insert "\n\n")))

;;;###autoload
(defun alarm (time message)
  "Set an alarm.
An alarm will occur at TIME with the MESSAGE.
The time format is the same accepted by `run-at-time'.
For example \"11:30am\"."
  (interactive "sTime: \nsMessage: ")
  (let ((alarm-timer (run-at-time time nil 'alarm-action message time)))
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

(define-derived-mode alarm-mode tabulated-list-mode "Alarm Menu"
  "A major mode for viewing a list of alarms."
  (add-hook 'tabulated-list-revert-hook 'alarm-refresh nil t))

(defun alarm-refresh ()
  "Refresh the table of alarms."
  (setq tabulated-list-format
        (vector '("Alarm" 50 t)
                '("Time"  20 t)))
  (setq tabulated-list-use-header-line t)
  (let ((table-contents (mapcar
                         (lambda (x) `("" [,(cadr x) ,(car x)]))
                         alarm-alist)))
    (setq tabulated-list-entries table-contents))
  (tabulated-list-init-header))

(defun alarm-get-buffer ()
  "TODO."
  (let ((buffer (get-buffer-create "*Alarm List*")))
    (with-current-buffer buffer
      (alarm-mode)
      (alarm-refresh)
      (tabulated-list-print))
    buffer))

(defun alarm-list ()
  "View the list of alarms."
  (interactive)
  (view-buffer (alarm-get-buffer)))

(provide 'alarm)

;;; alarm.el ends here
