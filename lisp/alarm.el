;;; alarm --- Display a message in a new frame at a specified time  -*- lexical-binding: t -*-

;;; Commentary:
;; Use Emacs as an alarm.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'seq)

(defvar alarm-alist nil
  "An alist of alarms.")

;; TODO: conditionally add dbus based alarm popup
;; (require 'notifications)
;; (defun alarm-notification (message time)
;;   "Displays MESSAGE (and TIME) via `notifications-notify'."
;;   (notifications-notify
;;    :title     (format "Alarm (%s)" time)
;;    :body      message
;;    :actions   '("snooze" "Snooze (5 mins)"
;;                 "snooze-for" "Snooze for ...")
;;    :on-action (lambda (_ action-key)
;;                 (pcase action-key
;;                   ("snooze" (alarm "5 mins" message))
;;                   ("snooze-for" (alarm (completing-read "Snooze alarm for: " '("1 mins" "5 mins" "10 mins")) message))))))
;; (add-hook 'alarm-hook 'alarm-notification)
;; (remove-hook 'alarm-hook 'alarm-notification)

(defun alarm-message (message time)
  "Displays MESSAGE (and TIME) via Emacs `message'."
  (message "Alarm %s (%s)" message time))

(defvar alarm-hook '()
  "Function(s) called when an alarm goes off.
These functions are passed MESSAGE and TIME as arguments.")

(defun alarm-action (message time)
  "Invokes function(s) in `alarm-hook' when alarm \"sounds\".
Each function is passed the MESSAGE and TIME as arguments."
  (run-hook-with-args 'alarm-hook message time))

(defun alarm-cancel (a)
  "Cancel the alarm A."
  (progn (cancel-timer (caddr a))
         (setq alarm-alist (delq a alarm-alist))
         (message "Cancelled alarm '%s' set for %s." (cadr a) (car a))))

(defun alarm-cancel-by-time (time)
  "Cancel the alarm clock set for TIME."
  (interactive "sTime: ")
  (let ((a (assoc time alarm-alist)))
    (if a (alarm-cancel a)
      (message "No alarm set for %s." time))))

(defun alarm-triggered (a)
  (timer--triggered (caddr a)))

(defun alarm-seconds-till (a)
  (floor (- (float-time (timer--time (caddr a)))
            (float-time (current-time)))))

(defun alarm-format-seconds (seconds)
  (let ((format (cond
                 ((>= seconds (* 60 60)) "%hh %mm %ss")
                 ((>= seconds 60)        "%mm %ss")
                 (t                      "%ss"))))
    (format-seconds format seconds)))

(defun alarm-format-time (a)
  (format-time-string "%T" (timer--time (caddr a))))

(defun alarm-kill (pos)
  "Kill Alarm under point."
  (interactive (list (1- (line-number-at-pos))))
  (when-let ((a (nth pos alarm-alist)))
    (if (y-or-n-p (format "Are you sure you want to %s alarm '%s'?" (if (alarm-triggered a) "delete" "cancel") (cadr a)))
        (alarm-cancel a)
      (message "Alarm '%s' not %s." (cadr a) (if (alarm-triggered a) "deleted" "cancelled")))
    (tabulated-list-revert)))

(defun alarm-refresh ()
  "Refresh the table of alarms."
  (setq tabulated-list-format
        '[("Triggered" 10 t)
          ("Alarm"     20 t) ;; TODO: dynamically calculate width
          ("Time"      10 t)
          ("Time till" 10 t)
          ("Time set"  10 t)])
  (setq tabulated-list-use-header-line t)
  (let ((table-contents (mapcar
                         (lambda (a) `("" [,(if (alarm-triggered a) "✔" "✗") ;; triggered
                                           ,(cadr a) ;; name
                                           ,(alarm-format-time a) ;; time
                                           ,(if (alarm-triggered a) "--" (alarm-format-seconds (alarm-seconds-till a))) ;; time till
                                           ,(car a) ;; time set
                                           ]))
                         alarm-alist)))
    (setq tabulated-list-entries table-contents))
  (tabulated-list-init-header))

(defun alarm-get-buffer ()
  "Return the alarm list buffer, creating it if necessary."
  (let ((buffer (get-buffer-create "*Alarm List*")))
    (with-current-buffer buffer
      (alarm-mode)
      (tabulated-list-revert)
      (tabulated-list-print))
    buffer))

(defun alarm-list ()
  "View the list of alarms."
  (interactive)
  (switch-to-buffer (alarm-get-buffer)))

(defun alarm-next ()
  (interactive)
  (let ((untriggered (seq-remove #'alarm-triggered alarm-alist)))
    (if (eq untriggered '()) (message "No alarms set.")
      (let ((a (car (seq-sort-by #'alarm-seconds-till #'< untriggered))))
        (message "Next alarm: %s in %s" (cadr a) (alarm-format-seconds (alarm-seconds-till a)))))))

;;;###autoload
(defun alarm (time message)
  "Set an alarm.
An alarm will occur at TIME with the MESSAGE.
The time format is the same accepted by `run-at-time'.
For example \"11:30am\" or \"5 mins\"."
  (interactive "sTime: \nsMessage: ")
  (let* ((alarm-timer (run-at-time time nil 'alarm-action message time))
         (a `(,time ,message ,alarm-timer)))
    (add-to-list 'alarm-alist a)
    (message "Alarm will go off in %s" (alarm-format-seconds (alarm-seconds-till a)))
    (alarm-get-buffer)))

(define-derived-mode alarm-mode tabulated-list-mode "Alarm List"
  "A major mode for viewing the list of alarms.

\\{alarm-mode-map}"
  (add-hook 'tabulated-list-revert-hook 'alarm-refresh nil t))

(define-key alarm-mode-map (kbd "k") 'alarm-kill)
(define-key alarm-mode-map (kbd "x") 'alarm-kill)
(define-key alarm-mode-map (kbd "a") 'alarm)

(provide 'alarm)

;;; alarm.el ends here
