;;; mortimer --- Countdown timer, displayed in the mode line

;;; Commentary:
;;
;; A countdown timer that displays in the mode line

;;; Usage:
;; (setq mortimer-sound "/usr/share/sounds/sound-icons/trumpet-12.wav")
;; (setq mortimer-sound "/usr/share/sounds/sound-icons/canary-long.wav")
;; (setq mortimer-sound "/usr/share/sounds/sound-icons/prompt.wav")
;;
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
;; (mortimer-play-sound "/usr/share/sounds/freedesktop/stereo/bell.oga")
;; (mortimer-play-sound "/usr/share/sounds/gnome/default/alerts/glass.ogg")
;; (mortimer-play-sound "/usr/share/sounds/gnome/default/alerts/drip.ogg")
;;
;; (mortimer-start "1 sec" t)
;; (mortimer-start "5 secs" t)
;; (mortimer-start "10 mins" t)
;; (mortimer-start "25 mins" t)
;; (mortimer-stop)

;;; Code:

(setq mortiner-log '())

(setq mortimer-timer nil)

(setq mortimer-mode-line nil)

(setq mortimer-mode-line-timer nil)

(setq mortimer-sound nil)

(defun mortimer-play-sound (sound)
  (when (and sound
             (file-exists-p (expand-file-name sound))
             (executable-find "paplay"))
	(start-process "mortimer-paplay" nil
			       "paplay" sound)))

(defun mortimer-time-remaining ()
  (when mortimer-timer
    (abs (floor (- (float-time (current-time))
                   (float-time (timer--time mortimer-timer)))))))

(defun mortimer-running-p ()
  (and mortimer-timer
       (> (mortimer-time-remaining) 0)))

(defun mortimer-to-string ()
  (when mortimer-timer
    (format-seconds "%02h:%02m:%02s"
                    (mortimer-time-remaining))))

(defun mortimer-update-mode-line ()
  (setq mortimer-mode-line
        (when (mortimer-running-p)
          (concat "[" (mortimer-to-string) "]")))
  (force-mode-line-update))

(defun mortimer-stop ()
  "Stop the current countdown timer."
  (interactive)
  (when mortimer-mode-line-timer
    (cancel-timer mortimer-mode-line-timer))
  (setq mortimer-mode-line-timer nil)
  (when mortimer-timer
    (cancel-timer mortimer-timer))
  (setq mortimer-timer nil)
  (mortimer-update-mode-line))

(defun mortimer-on-complete ()
  (mortimer-play-sound mortimer-sound)
  (mortimer-stop))

;;;###autoload
(defun mortimer-start (time &optional force)
  "Start a countdown timer starting at `time', e.g. \"25
  mins\". Non-nil `force' will delete the current timer if there
  is one running."
  (interactive "sTime: ")
  (if-let ((seconds (timer-duration time)))
      (progn
        (add-to-list 'mode-line-misc-info '(mortimer-mode-line ("" mortimer-mode-line " ")) t)
        (when (and (mortimer-running-p)
                   (or force
                       (yes-or-no-p "Stop the current timer?")))
          (mortimer-stop))
        (setq mortimer-timer
              (run-with-timer seconds nil
                              'mortimer-on-complete))
        (setq mortimer-mode-line-timer
	          (run-with-timer 0 1 'mortimer-update-mode-line)))
    (message "%s isn't a time, try something like \"25 mins\"." time)))

(message "Mortimer loaded")

(provide 'mortimer)
