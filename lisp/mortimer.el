;;; mortimer --- Countdown timer, displayed in the mode line  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; A countdown timer that displays in the mode line

;;; Usage:
;; (mortimer-start "1 sec")
;; (mortimer-start "5 secs")
;; (mortimer-start "10 mins")
;; (mortimer-start "25 mins")
;; (mortimer-stop)

;;; TODO:
;; - change faces to inherit from standard ones, e.g. success, error etc

;;; Code:

(require 'view)
(require 'seq)
(require 'map)

(defvar mortimer-timer nil)
(defvar mortimer-timer-duration nil)

(defvar mortimer-mode-line nil)
(put 'mortimer-mode-line 'risky-local-variable t)

(defvar mortimer-mode-line-timer nil)

(defvar mortimer-pause-time-remaining nil)

(defvar mortimer-log '()
  "The log of timer events, used to display a summary of the recent timers.
Stored in reverse chronological order.")

(defun mortimer-on-complete-message ()
  "Displays completion message via Emacs `message'."
  (message "Mortimer timer complete"))

(defvar mortimer-complete-hook '(mortimer-on-complete-message)
  "Function(s) called when a Mortimer timer completes.")

(defface mortimer-mode-line-complete-face
  '((t (:background "#050")))
  "Face used for complete portion of Mortimer timer in the mode line.")

(defface mortimer-mode-line-remaining-face
  '((t ()))
  "Face used for remaining portion of Mortimer timer in the mode line.")

(defface mortimer-mode-line-paused-face
  '((t (:foreground "#500")))
  "Face used for Mortimer timer in the mode line when paused.")

(defun mortimer-time-remaining ()
  "Return the amount of time remaining for the current timer, in seconds."
  (when mortimer-timer
    (floor (- (float-time (timer--time mortimer-timer))
              (float-time (current-time))))))

(defun mortimer-time-complete ()
  "Return the amount of time completed for the current timer, in seconds."
  (when-let ((remaining (when mortimer-timer-duration (mortimer-time-remaining))))
    (- mortimer-timer-duration remaining)))

(defun mortimer-fraction-complete ()
  "Return the fraction of time completed for the current timer."
  (/ (float (mortimer-time-complete)) mortimer-timer-duration))

(defun mortimer-running-p ()
  "Check whether there is a timer currently running and not paused."
  (and mortimer-timer
       (> (mortimer-time-remaining) 0)))

(defun mortimer-duration-friendly-string (seconds)
  "Format SECONDS as a human readable string.
e.g. 60 => \"1 min\", 125 => 2 mins 5 secs"
  (let* ((s (mod seconds 60))
         (m (mod (floor (/ seconds 60)) 60))
         (h (floor (/ seconds (* 60 60)))))
    (string-join (seq-remove 'null
                             (list (when (< 0 h)
                                     (format "%s %s" h (if (eq 1 h) "hr" "hrs")))
                                   (when (or (< 0 m) (and (< 0 h) (< 0 s)))
                                     (format "%s %s" m (if (eq 1 m) "min" "mins")))
                                   (when (< 0 s)
                                     (format "%s %s" s (if (eq 1 s) "sec" "secs")))))
                 " ")))

(defun mortimer-time-remaining-string ()
  "Return the time remaining for the current timer as a formatted string."
  (when mortimer-timer
    (format-seconds "%02h:%02m:%02s"
                    (mortimer-time-remaining))))

(defun mortimer-propertize-for-mode-line ()
  `(:eval
    (let* ((timer-string (string-join (list "[" (mortimer-time-remaining-string) "]")))
           (timer-string-length (length timer-string))
           (num-complete (floor (* timer-string-length (mortimer-fraction-complete)))))
      (put-text-property 0 num-complete 'face 'mortimer-mode-line-complete-face timer-string)
      (put-text-property num-complete timer-string-length 'face 'mortimer-mode-line-remaining-face timer-string)
      timer-string)))

(defun mortimer-refresh-mode-line ()
  "Used to force a mode line refresh, redisplaying the time remaining."
  (force-mode-line-update t))

(defun mortimer-update-mode-line ()
  "Update the mode line based on the timer state."
  (setq mortimer-mode-line
        (cond ((mortimer-running-p)
               (mortimer-propertize-for-mode-line))
              (mortimer-pause-time-remaining
               `(:propertize (">" (:eval (format-seconds "%02h:%02m:%02s" mortimer-pause-time-remaining)) "<")
                             face mortimer-mode-line-paused-face))))
  (mortimer-refresh-mode-line))

(defun mortimer-timer-start (seconds)
  "Start a timer for SECONDS, a numeric duration in seconds.
Does not reset state, used to start or resume timer."
  (setq mortimer-timer
        (run-with-timer seconds nil
                        'mortimer-on-complete))
  (mortimer-update-mode-line)
  (setq mortimer-mode-line-timer
	    (run-with-timer 0 1
                        'mortimer-refresh-mode-line)))

(defun mortimer-timer-init-and-start (seconds)
  "Start a countdown timer starting at SECONDS.
This will delete the current timer if there is one running or paused."
  (add-to-list 'mode-line-misc-info '(mortimer-mode-line ("" mortimer-mode-line " ")) t)
  (mortimer-stop)
  (setq mortimer-timer-duration seconds)
  (mortimer-timer-start seconds))

(defun mortimer-timer-stop ()
  "Stop current timer without resetting state, used to stop or pause timer."
  (when mortimer-mode-line-timer
    (cancel-timer mortimer-mode-line-timer))
  (setq mortimer-mode-line-timer nil)
  (when mortimer-timer
    (cancel-timer mortimer-timer))
  (setq mortimer-timer nil))

(defun mortimer-stop ()
  "Stop the current countdown timer."
  (interactive)
  (mortimer-timer-stop)
  (setq mortimer-timer-duration nil)
  (setq mortimer-pause-time-remaining nil)
  (mortimer-update-mode-line))

(defun mortimer-on-complete ()
  "Callback for timer completion."
  (run-hooks 'mortimer-complete-hook)
  (mortimer-stop))

;;;###autoload
(defun mortimer-start (time)
  "Start a countdown timer starting at TIME, e.g. \"25 mins\".
This will delete the current timer if there is one running or paused."
  (interactive "sTime: ")
  (if-let ((seconds (if (numberp time) time (timer-duration time))))
      (mortimer-timer-init-and-start seconds)
    (message "I don't understand %s, try something like \"25 mins\"." time)))

(defvar mortimer-quick-toggle-default-time "25 mins")

;;;###autoload
(defun mortimer-quick-toggle ()
  "Convenience function to start or stop a timer with the default duration.
Ideal for binding to a convenient key."
  (interactive)
  (if (or (mortimer-running-p)
          mortimer-pause-time-remaining)
      (when (yes-or-no-p "Stop the timer?")
        (mortimer-stop)
        (message "Timer stopped."))
    (mortimer-start mortimer-quick-toggle-default-time)
    (message "Timer started for %s." mortimer-quick-toggle-default-time)))

(defun mortimer-pause ()
  "Pause the current timer."
  (when-let ((remaining (mortimer-time-remaining)))
    (mortimer-timer-stop)
    (setq mortimer-pause-time-remaining remaining)
    (mortimer-update-mode-line)))

(defun mortimer-resume ()
  "Resume a paused timer."
  (when mortimer-pause-time-remaining
    (mortimer-timer-start mortimer-pause-time-remaining)
    (setq mortimer-pause-time-remaining nil)))

(defun mortimer-pause-resume ()
  "Convenience function to pause or resume the current timer.
Ideal for binding to a convenient key."
  (interactive)
  (cond ((mortimer-running-p)
         (mortimer-pause)
         (message "Timer paused."))
        (mortimer-pause-time-remaining
         (mortimer-resume)
         (message "Timer resumed."))
        (:otherwise
         (message "No timer to pause/resume."))))

(defun mortimer-log-append (id &optional f args)
  "Append an event, with ID and ARGS, to the log.
Optionally invoke F, excluding any internal logs."
  (let ((mortimer-log-old mortimer-log)) ;; don't log inner calls, e.g. advised fns
    (prog1 (when f (apply f args))
      (setq mortimer-log (cons (list :time (current-time) :id id :args args) mortimer-log-old)))))

(defun mortimer-mark-last-as-fail ()
  "Append a fail event to the log."
  (interactive)
  (mortimer-log-append :fail))

(defun mortimer-mark-last-as-success ()
  "Append a success event to the log."
  (interactive)
  (mortimer-log-append :success))

(defun mortimer-log-advice (id)
  "Create an advice function for log events with ID."
  (lambda (f &rest args)
    (mortimer-log-append id f args)))

(fset 'mortimer-start-advice    (mortimer-log-advice :start))
(fset 'mortimer-stop-advice     (mortimer-log-advice :stop))
(fset 'mortimer-pause-advice    (mortimer-log-advice :pause))
(fset 'mortimer-resume-advice   (mortimer-log-advice :resume))
(fset 'mortimer-complete-advice (mortimer-log-advice :complete))

(advice-add 'mortimer-timer-init-and-start ;; can't add advice to mortimer-start, as that function fails on invalid user input
            :around #'mortimer-start-advice)
(advice-add 'mortimer-stop
            :around #'mortimer-stop-advice)
(advice-add 'mortimer-pause
            :around #'mortimer-pause-advice)
(advice-add 'mortimer-resume
            :around #'mortimer-resume-advice)
(advice-add 'mortimer-on-complete
            :around #'mortimer-complete-advice)

(defface mortimer-view-log-complete-face
  '((t (:foreground "#bb0" :box t)))
  "Face used for Mortimer completed timers in the log.")

(defface mortimer-view-log-success-face
  '((t (:foreground "#090" :box t)))
  "Face used for Mortimer success timers in the log.")

(defface mortimer-view-log-incomplete-face
  '((t (:foreground "#999" :box t)))
  "Face used for Mortimer incomplete timers in the log.")

(defface mortimer-view-log-fail-face
  '((t (:foreground "#b00" :box t)))
  "Face used for Mortimer failed timers in the log.")

(defun mortimer-partition-at (pred sequence)
  "Partition SEQUENCE at points when PRED is true.
Will return partitions, each partition is a list of one item for
which PRED is true and then 0 or more items for which PRED is false."
  (when (not (seq-empty-p sequence))
    (cons (cons (seq-first sequence)
                (seq-take-while (lambda (x) (not (funcall pred x))) (seq-rest sequence)))
          (mortimer-partition-at pred (seq-drop-while (lambda (x) (not (funcall pred x))) (seq-rest sequence))))))

(defun mortimer-group-log (log)
  "Group the events in LOG in to timers."
  (thread-last
    log
    (seq-reverse)
    (mortimer-partition-at (lambda (l) (eq (plist-get l :id) :start)))))

(defun mortimer-reduce-log (log)
  "Summarise LOG, a list of Mortimer events, by collating details for each timer."
  (let* ((sorted-log (sort log (lambda (x y) (time-less-p (plist-get x :time)
                                                          (plist-get y :time)))))
         ;; `aggregated' is a hashmap keyed by the start times, enabling the lookup for events with :time-ref
         ;; and the timer being aggregated is stored in :active
         (aggregated (seq-reduce
                      (lambda (agg e)
                        (let* ((e-id   (plist-get e :id))
                               (start? (eq e-id :start))
                               (ref    (plist-get e :time-ref))
                               (active (gethash :active agg))
                               (status (when active (gethash :status active)))
                               (curr   (cond
                                        (ref    (gethash ref agg)) ;; updating a previous timer by ref
                                        (start? nil)               ;; starting a new timer
                                        (t      active)))          ;; updating the active timer
                               (result (map-merge 'hash-table curr (list e-id e :status e-id)))
                               (start  (map-nested-elt result '(:start :time))))
                          (map-merge 'hash-table
                                      agg
                                      (list start result) ;; FIX: this breaks when time is "real" elisp time
                                      (when (not ref)
                                        (list :active result))
                                      ;; starting new timer with stopped (but not complete) previous timer
                                      ;; so mark previous timer as incomplete
                                      (when (and start?
                                                 active
                                                 (eq status :stop))
                                        (list (map-nested-elt active '(:start :time))
                                              (map-merge 'hash-table active (list :status :incomplete)))))))
                      sorted-log
                      (make-hash-table)))
         (reduced (map-values (map-delete aggregated :active))))
    (sort reduced
          (lambda (x y) (time-less-p (map-nested-elt x '(:start :time))
                                     (map-nested-elt y '(:start :time)))))))

(defun mortimer-log-tag (id)
  (string-pad
   (cl-case id
     (:fail       (propertize "failed" 'face 'mortimer-view-log-fail-face))
     (:success    (propertize "success" 'face 'mortimer-view-log-success-face))
     (:complete   (propertize "completed" 'face 'mortimer-view-log-complete-face))
     (:paused     (propertize "paused" 'face 'mortimer-view-log-incomplete-face))
     (:incomplete (propertize "incomplete" 'face 'mortimer-view-log-incomplete-face))
     (:ongoing    (propertize "ongoing" 'face 'mortimer-view-log-incomplete-face)))
   10)) ;; (length "incomplete")

(defun mortimer-get-buffer ()
  "Prepare and open the Mortimer buffer, displaying the timers."
  (let ((buffer (get-buffer-create "*Mortimer*")))
    (with-current-buffer buffer
      (view-mode -1)
      (erase-buffer)
      (insert (propertize "Mortimer Log" 'face 'header-line))
      (insert "\n\n")
      (thread-last
        mortimer-log
        mortimer-group-log
        ;; TODO: Split the "collapsing" of multiple events from the string formatting
        ;;  - fix: `mortimer-mark-last-as-success' doesn't work after `mortimer-mark-last-as-fail'
        (seq-map (lambda (p)
                   (let* ((start         (car p)) ;; first entry is always the :start event - see `mortimer-group-log'
                          (start-time    (format-time-string "%F %R" (plist-get start :time)))
                          (duration      (car (plist-get start :args)))
                          (duration-time (mortimer-duration-friendly-string duration))
                          (complete      (seq-find (lambda (l) (eq (plist-get l :id) :complete)) p))
                          (complete-time (format-time-string "%F %R" (plist-get complete :time)))
                          (paused        (seq-find (lambda (l) (eq (plist-get l :id) :pause)) p))
                          ;; (resumed       (seq-find (lambda (l) (eq (plist-get l :id) :resume)) p)) ;; TODO: handled resumed timers
                          (success       (seq-find (lambda (l) (eq (plist-get l :id) :success)) p))
                          (fail          (seq-find (lambda (l) (eq (plist-get l :id) :fail)) p))
                          (end-time      (time-add (plist-get start :time) duration))
                          (stop          (seq-find (lambda (l) (eq (plist-get l :id) :stop)) p))
                          (ongoing       (time-less-p (current-time) end-time)))
                     (cond (fail       (format (concat (mortimer-log-tag :fail)       " %s (%s)\n")      start-time duration-time))
                           (success    (format (concat (mortimer-log-tag :success)    " %s - %s (%s)\n") start-time complete-time duration-time))
                           (complete   (format (concat (mortimer-log-tag :complete)   " %s - %s (%s)\n") start-time complete-time duration-time))
                           (paused     (format (concat (mortimer-log-tag :paused)     " %s - %s (%s)\n") start-time complete-time duration-time))
                           (stop       (format (concat (mortimer-log-tag :incomplete) " %s (%s)\n")      start-time duration-time))
                           (ongoing    (format (concat (mortimer-log-tag :ongoing)    " %s (%s)\n")      start-time duration-time))
                           ;; TODO: rename, this is really an unknown state
                           (:otherwise (format (concat (mortimer-log-tag :incomplete) " %s (%s)\n")      start-time duration-time))))))
        (seq-do #'insert)))
    buffer))

(defun mortimer-view-log ()
  (interactive)
  (view-buffer (mortimer-get-buffer)))

(defun mortimer-clear-log ()
  (interactive)
  (when (yes-or-no-p "Are you sure you want to clear the Mortimer log?")
    (setq mortimer-log '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; experiment to use tabulated list mode for the mortimer log

(when
nil

(defun mortimer-log-refresh ()
  "Refresh the table of Mortimer log entries."
  (setq tabulated-list-format
        '[("A" 10 t)
          ("B" 10 t)
          ("C" 10 t)])
  (setq tabulated-list-use-header-line t)
  (let ((table-contents (mapcar
                         (lambda (x) `("" [,(symbol-name (plist-get x :id))
                                           ""
                                           ""]))
                         mortimer-log)))
    (setq tabulated-list-entries table-contents))
  (tabulated-list-init-header))

(define-derived-mode mortimer-log-mode tabulated-list-mode "Mortimer Log"
  "A major mode for viewing the Mortimer log.

\\{mortimer-log-mode-map}"
  (add-hook 'tabulated-list-revert-hook 'mortimer-log-refresh nil t))

(let ((buffer (get-buffer-create "*Mortimer Log*")))
  (with-current-buffer buffer
    (mortimer-log-mode)
    (tabulated-list-revert)
    ;; (tabulated-list-print)
    )
  buffer)

)
;; end experiment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when
nil

(defun mortimer-time-at-midnight (time)
  (let ((dt (decode-time time)))
    (encode-time
     (make-decoded-time
      :second 0
      :minute 0
      :hour   0
      :day    (decoded-time-day   dt)
      :month  (decoded-time-month dt)
      :year   (decoded-time-year  dt)
      :dst    (decoded-time-dst   dt)
      :zone   (decoded-time-zone  dt)))))

(list (format-time-string "%F %R" (current-time))
      (format-time-string "%F %r" (current-time))
      (format-time-string "%F %R" (time-subtract nil (* 60 60)))
      (format-time-string "%F %r" (time-subtract nil (* 60 60))))

(thread-last mortimer-log
             (seq-filter (lambda (x)
                           (time-less-p
                            (mortimer-time-at-midnight (current-time))
                            (plist-get x :time))))
             (seq-map (lambda (x)
                        (message "%s %s %s"
                                 (format-time-string "%F %r" (plist-get x :time))
                                 (plist-get x :id)
                                 (plist-get x :args))))
             (mapc #'message))

(timer-duration mortimer-quick-toggle-default-time)

;; investigating a "set timer until" mode of operation
(require 'diary-lib)
(diary-entry-time "14:07") ;; 1407
(diary-entry-time "2:07pm") ;; 1407
(let* ((time "14:07")
      (hhmm (diary-entry-time time)))
	(when (>= hhmm 0)
	  (list (/ hhmm 100) (% hhmm 100))))
;; => (14 7)

(let* ((start (car mortimer-log))
       (duration (car (plist-get start :args))))
  (time-add (plist-get start :time) duration))

(let ((success1 (car mortimer-log))
      (start2 (cadr mortimer-log))
      (rest (cddr mortimer-log)))
  (setq mortimer-log
        (cons start2 (cons success1 rest))))

(equal
 (thread-first mortimer-log car (plist-get :time))
 '(25487 6029 664998 970000))

(setq mortimer-log-old mortimer-log)

(setq mortimer-log
      '((:time (25487 6029 664998 970000) :id :start :args (1500))
        (:time (25487 6043 190203 201000) :id :success :args nil)
        (:time (25487 6027 256551 522000) :id :complete :args nil)
        (:time (25487 4527 246490 116000) :id :start :args (1500))))

)

(provide 'mortimer)

;;; mortimer.el ends here
