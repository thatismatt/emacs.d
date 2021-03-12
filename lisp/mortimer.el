;;; mortimer --- Countdown timer, displayed in the mode line  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; A countdown timer that displays in the mode line

;;; Usage:
;; (setq mortimer-sound "/usr/share/sounds/sound-icons/trumpet-12.wav")
;; (setq mortimer-sound "/usr/share/sounds/sound-icons/canary-long.wav")
;; (setq mortimer-sound "/usr/share/sounds/sound-icons/prompt.wav")
;;
;; (mortimer-play-sound "/usr/share/sounds/sound-icons/trumpet-12.wav")
;; (mortimer-play-sound "/usr/share/sounds/sound-icons/canary-long.wav")
;; (mortimer-play-sound "/usr/share/sounds/sound-icons/cembalo-2.wav")
;; (mortimer-play-sound "/usr/share/sounds/sound-icons/cembalo-6.wav")
;; (mortimer-play-sound "/usr/share/sounds/sound-icons/cembalo-12.wav")
;; (mortimer-play-sound "/usr/share/sounds/sound-icons/gummy-cat-2.wav")
;; (mortimer-play-sound "/usr/share/sounds/sound-icons/piano-3.wav")
;; (mortimer-play-sound "/usr/share/sounds/sound-icons/prompt.wav")
;; (mortimer-play-sound "/usr/share/sounds/sound-icons/trumpet-12.wav")
;; (mortimer-play-sound "/usr/share/sounds/sound-icons/xylofon.wav")
;; (mortimer-play-sound "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga")
;; (mortimer-play-sound "/usr/share/sounds/freedesktop/stereo/complete.oga")
;; (mortimer-play-sound "/usr/share/sounds/freedesktop/stereo/message.oga")
;; (mortimer-play-sound "/usr/share/sounds/gnome/default/alerts/glass.ogg")
;; (mortimer-play-sound "/usr/share/sounds/gnome/default/alerts/drip.ogg")
;;
;; (mortimer-start "1 sec")
;; (mortimer-start "5 secs")
;; (mortimer-start "10 mins")
;; (mortimer-start "25 mins")
;; (mortimer-stop)

;;; Code:

(setq mortimer-timer nil)
(setq mortimer-timer-duration nil)

(setq mortimer-mode-line nil)
(put 'mortimer-mode-line 'risky-local-variable t)

(setq mortimer-mode-line-timer nil)

(setq mortimer-sound nil)

(setq mortimer-sound-command
      (if (executable-find "ffplay")
          (list "ffplay" "-nodisp" "-autoexit")
        (list "paplay")))

(setq mortimer-pause-time-remaining nil)

(setq mortimer-log '())

(defface mortimer-mode-line-complete-face
  '((t (:background "#050")))
   "Face used for complete portion of Mortimer timer in the mode line")

(defface mortimer-mode-line-remaining-face
  '((t ()))
   "Face used for remaining portion of Mortimer timer in the mode line")

(defface mortimer-mode-line-paused-face
  '((t (:foreground "#500")))
  "Face used for Mortimer timer in the mode line when paused")

(defun mortimer-play-sound (sound)
  (when (and sound
             (file-exists-p (expand-file-name sound))
             (stringp (car mortimer-sound-command))
             (executable-find (car mortimer-sound-command)))
    (apply 'start-process "mortimer-sound-command" nil
           (seq-concatenate 'list mortimer-sound-command (list (expand-file-name sound))))))

(defun mortimer-time-remaining ()
  (when mortimer-timer
    (floor (- (float-time (timer--time mortimer-timer))
              (float-time (current-time))))))

(defun mortimer-time-complete ()
  (when-let ((remaining (when mortimer-timer-duration (mortimer-time-remaining))))
    (- mortimer-timer-duration remaining)))

(defun mortimer-fraction-complete ()
  (/ (float (mortimer-time-complete)) mortimer-timer-duration))

(defun mortimer-running-p ()
  (and mortimer-timer
       (> (mortimer-time-remaining) 0)))

(defun mortimer-to-string ()
  (when mortimer-timer
    (format-seconds "%02h:%02m:%02s"
                    (mortimer-time-remaining))))

(defun mortimer-propertize-for-mode-line ()
  `(:eval
    (let* ((timer-string (string-join (list "[" (mortimer-to-string) "]")))
           (timer-string-length (length timer-string))
           (num-complete (floor (* timer-string-length (mortimer-fraction-complete)))))
      (put-text-property 0 num-complete 'face 'mortimer-mode-line-complete-face timer-string)
      (put-text-property num-complete timer-string-length 'face 'mortimer-mode-line-remaining-face timer-string)
      timer-string)))

(defun mortimer-refresh-mode-line ()
  (force-mode-line-update t))

(defun mortimer-update-mode-line ()
  (setq mortimer-mode-line
        (cond ((mortimer-running-p)
               (mortimer-propertize-for-mode-line))
              (mortimer-pause-time-remaining
               `(:propertize (">" (:eval (format-seconds "%02h:%02m:%02s" mortimer-pause-time-remaining)) "<")
                             face mortimer-mode-line-paused-face))))
  (mortimer-refresh-mode-line))

(defun mortimer-timer-start (seconds)
  (setq mortimer-timer
        (run-with-timer seconds nil
                        'mortimer-on-complete))
  (mortimer-update-mode-line)
  (setq mortimer-mode-line-timer
	    (run-with-timer 0 1
                        'mortimer-refresh-mode-line)))

(defun mortimer-timer-stop ()
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
  (mortimer-play-sound mortimer-sound)
  (mortimer-stop))

;;;###autoload
(defun mortimer-start (time)
  "Start a countdown timer starting at `time', e.g. \"25
  mins\". This will delete the current timer if there is one
  running or paused."
  (interactive "sTime: ")
  (if-let ((seconds (if (numberp time) time (timer-duration time))))
      (progn
        (add-to-list 'mode-line-misc-info '(mortimer-mode-line ("" mortimer-mode-line " ")) t)
        (mortimer-stop)
        (setq mortimer-timer-duration seconds)
        (mortimer-timer-start seconds))
    (message "%s isn't a time, try something like \"25 mins\"." time)))

(setq mortimer-quick-toggle-default-time "25 mins")

;;;###autoload
(defun mortimer-quick-toggle ()
  (interactive)
  (if (or (mortimer-running-p)
          mortimer-pause-time-remaining)
      (when (yes-or-no-p "Stop the timer?")
        (mortimer-stop)
        (message "Timer stopped."))
    (mortimer-start mortimer-quick-toggle-default-time)
    (message "Timer started for %s." mortimer-quick-toggle-default-time)))

(defun mortimer-pause ()
  (when-let ((remaining (mortimer-time-remaining)))
    (mortimer-timer-stop)
    (setq mortimer-pause-time-remaining remaining)
    (mortimer-update-mode-line)))

(defun mortimer-resume ()
  (when mortimer-pause-time-remaining
    (mortimer-timer-start mortimer-pause-time-remaining)
    (setq mortimer-pause-time-remaining nil)))

(defun mortimer-pause-resume ()
  (interactive)
  (cond ((mortimer-running-p)
         (mortimer-pause)
         (message "Timer paused."))
        (mortimer-pause-time-remaining
         (mortimer-resume)
         (message "Timer resumed."))
        (:otherwise
         (message "No timer to pause/resume."))))

(defun mortimer-mark-last-as-fail ()
  (interactive)
  (setq mortimer-log (cons (list :time (current-time) :id :fail :args nil) mortimer-log)))

(defun mortimer-log-advice (id)
  (lambda (f &rest args)
    (let ((mortimer-log-old mortimer-log)) ;; don't log inner advised fns
      (prog1 (apply f args)
        (setq mortimer-log (cons (list :time (current-time) :id id :args args) mortimer-log-old))))))

(fset 'mortimer-start-advice    (mortimer-log-advice :start))
(fset 'mortimer-stop-advice     (mortimer-log-advice :stop))
(fset 'mortimer-pause-advice    (mortimer-log-advice :pause))
(fset 'mortimer-resume-advice   (mortimer-log-advice :resume))
(fset 'mortimer-complete-advice (mortimer-log-advice :complete))

(advice-add 'mortimer-start       :around #'mortimer-start-advice)
(advice-add 'mortimer-stop        :around #'mortimer-stop-advice)
(advice-add 'mortimer-pause       :around #'mortimer-pause-advice)
(advice-add 'mortimer-resume      :around #'mortimer-resume-advice)
(advice-add 'mortimer-on-complete :around #'mortimer-complete-advice)

(defun mortimer-partition-by (pred sequence)
  (when (not (seq-empty-p sequence))
    (cons (cons (seq-first sequence)
                (seq-take-while (lambda (x) (not (funcall pred x))) (seq-rest sequence)))
          (mortimer-partition-by pred (seq-drop-while (lambda (x) (not (funcall pred x))) (seq-rest sequence))))))

(defface mortimer-view-log-completed-face
  '((t (:foreground "#090" :box t)))
  "Face used for Mortimer completed timers in the log")

(defface mortimer-view-log-unfinished-face
  '((t (:foreground "#b00" :box t)))
  "Face used for Mortimer unfinished timers in the log")

(defun mortimer-get-buffer ()
  (let ((buffer (get-buffer-create "*Mortimer*")))
    (with-current-buffer buffer
      (view-mode-disable)
      (erase-buffer)
      (insert (propertize "Mortimer Log" 'face 'header-line))
      (insert "\n\n")
      (thread-last mortimer-log
        (seq-reverse)
        (mortimer-partition-by (lambda (l) (eq (plist-get l :id) :start)))
        (seq-map (lambda (p)
                   (let* ((start (car p))
                          (start-time (format-time-string "%F %R" (plist-get start :time)))
                          (duration (car (plist-get start :args)))
                          (complete (seq-find (lambda (l) (eq (plist-get l :id) :complete)) p))
                          (fail (seq-find (lambda (l) (eq (plist-get l :id) :fail)) p))
                          ;; (end-time (time-add (plist-get start :time) (timer-duration duration)))
                          ;; (stop (seq-find (lambda (l) (eq (plist-get l :id) :stop)) p))
                          ;; (ongoing  (time-less-p (current-time) end-time))
                          )
                     (cond (fail       (format (concat (propertize "failed" 'face 'mortimer-view-log-unfinished-face) "     %s (%s)\n")
                                               start-time duration))
                           (complete   (format (concat (propertize "completed" 'face 'mortimer-view-log-completed-face) "  %s - %s (%s)\n")
                                               start-time (format-time-string "%F %R" (plist-get complete :time)) duration))
                           ;; TODO:
                           ;; (stop       (format (concat (propertize "unfinished" 'face '(:foreground "#a00" :box t)) " %s (%s)\n")
                           ;;                     start-time duration))
                           ;; (ongoing    (format (concat (propertize "ongoing" 'face '(:foreground "#d80" :box t)) "    %s (%s)\n")
                           ;;                     start-time duration))
                           (:otherwise (format (concat (propertize "unfinished" 'face 'mortimer-view-log-unfinished-face) " %s (%s)\n")
                                               start-time duration))))))
        (seq-do #'insert)))
    buffer))

(defun mortimer-view-log ()
  (interactive)
  (view-buffer (mortimer-get-buffer)))

(defun mortimer-clear-log ()
  (interactive)
  (setq mortimer-log '()))

(provide 'mortimer)
