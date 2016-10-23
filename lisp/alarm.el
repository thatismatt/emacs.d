;;; alarm --- Display a message in a new frame at a specified time.

;;; Commentary:
;; Use Emacs as an alarm.

;;; Code:

(eval-when-compile (require 'cl))

(require 'dash)

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
For example \"11:30am\" or \"5 mins\"."
  (interactive "sTime: \nsMessage: ")
  (let* ((alarm-timer (run-at-time time nil 'alarm-action message time))
         (a `(,time ,message ,alarm-timer)))
    (add-to-list 'alarm-alist a)
    (message "Alarm will go off in %s" (alarm-format-seconds (alarm-seconds-till a)))))

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

(define-derived-mode alarm-mode tabulated-list-mode "Alarm List"
  "A major mode for viewing the list of alarms."
  (add-hook 'tabulated-list-revert-hook 'alarm-refresh nil t))

(defun alarm-kill (pos)
  "Kill Alarm under point."
  (interactive (list (1- (line-number-at-pos))))
  (let* ((a (nth pos alarm-alist))
         (triggered (alarm-triggered a)))
    (if (y-or-n-p (format "Are you sure you want to %s alarm '%s'?" (if triggered "delete" "cancel") (cadr a)))
        (alarm-cancel a)
      (message "Alarm '%s' not %s." (cadr a) (if triggered "deleted" "cancelled"))))
  (tabulated-list-revert))
(define-key alarm-mode-map (kbd "k") 'alarm-kill)

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

(defun alarm-next ()
  (interactive)
  (let ((untriggered (--filter (not (alarm-triggered it)) alarm-alist)))
    (if (eq untriggered '()) (message "No alarms set.")
      (let ((a (car (--sort (< (alarm-seconds-till it) (alarm-seconds-till other)) untriggered))))
          (message "Next alarm: %s in %s" (cadr a) (alarm-format-seconds (alarm-seconds-till a)))))))

(provide 'alarm)

;;; alarm.el ends here
