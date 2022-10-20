;;; alarm --- Display a message in a new frame at a specified time  -*- lexical-binding: t -*-

;;; Commentary:
;; Use Emacs as an alarm.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'dash)

(defvar alarm-alist nil
  "An alist of alarms.")

(defvar alarm-popup-buffer "*Alarm*")

(defun alarm-play-sound (sound)
  (when (and sound
             (file-exists-p (expand-file-name sound))
             (executable-find "paplay"))
	(start-process "alarm-paplay" nil "paplay" sound)))

(defun alarm-action (message time)
  "The actual alarm action.
Displays MESSAGE (and TIME) in `alarm-popup-buffer'."
  (progn
    (switch-to-buffer-other-frame alarm-popup-buffer)
    (goto-char (point-max))
    (insert (format "\n### ALARM (%s) ###\n\n" time))
    (insert message)
    (insert "\n\n")
    (alarm-popup-mode)
    (setq-local alarm-message message)
    (alarm-play-sound "/usr/share/sounds/sound-icons/trumpet-12.wav")))

(defun alarm-popup-kill ()
  (interactive)
  (kill-buffer (current-buffer))
  (delete-frame))

(defun alarm-popup-snooze (mins)
  (interactive "sSnooze (mins): ")
  (alarm (format "%s mins" mins) alarm-message)
  (alarm-popup-kill))

(define-derived-mode alarm-popup-mode fundamental-mode "Alarm Popup"
  "A mode for the Alarm Popup
\\{alarm-popup-mode-map}"
  (read-only-mode))

(define-key alarm-popup-mode-map (kbd "q") 'alarm-popup-kill)
(define-key alarm-popup-mode-map (kbd "s") 'alarm-popup-snooze)

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
  (let ((untriggered (--filter (not (alarm-triggered it)) alarm-alist)))
    (if (eq untriggered '()) (message "No alarms set.")
      (let ((a (car (--sort (< (alarm-seconds-till it) (alarm-seconds-till other)) untriggered))))
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
