;;; alarm --- Display a message in a new frame at a specified time.

;;; Commentary:
;; Use Emacs as an alarm.

;;; Code:

(eval-when-compile (require 'cl))

(defvar alarm-alist nil
  "An alist of alarms.")

(defvar alarm-popup-buffer "*Alarm*")

(defun alarm-action (message time)
  "The actual alarm action.
Displays MESSAGE (and TIME) in `alarm-popup-buffer'."
  (progn
    (switch-to-buffer-other-frame alarm-popup-buffer)
    (goto-char (point-max))
    (insert (format "\n### ALARM (%s) ###\n\n" time))
    (insert message)
    (insert "\n\n")
    (alarm-popup-mode)))

(defun alarm-popup-kill ()
  (interactive)
  (kill-buffer (current-buffer))
  (delete-frame))

(defvar alarm-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map  (kbd "q") 'alarm-popup-kill)
    map)
  "Keymap for `alarm-popup-mode'.")

(define-derived-mode alarm-popup-mode fundamental-mode
  "Alarm Popup"
  "A mode for the Alarm Popup"
  (use-local-map alarm-popup-mode-map))

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

(define-derived-mode alarm-mode tabulated-list-mode "Alarm List"
  "A major mode for viewing the list of alarms."
  (add-hook 'tabulated-list-revert-hook 'alarm-refresh nil t))

(defun alarm-refresh ()
  "Refresh the table of alarms."
  (setq tabulated-list-format
        (vector '("Triggered" 20 t)
                '("Alarm"     50 t)
                '("Time"      20 t)
                '("Time till" 20 t)))
  (setq tabulated-list-use-header-line t)
  (let ((table-contents (mapcar
                         (lambda (x) `("" [,(if (alarm-triggered x) "✔" "✗") ;; triggered
                                           ,(cadr x) ;; name
                                           ,(car x) ;; time
                                           ,(if (alarm-triggered x) "--" (alarm-format-seconds (alarm-seconds-till x))) ;; time till
                                           ]))
                         alarm-alist)))
    (setq tabulated-list-entries table-contents))
  (tabulated-list-init-header))

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

(defun alarm-get-buffer ()
  "Return the alarm list buffer, creating it if necessary."
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
