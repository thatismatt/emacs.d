;;; init --- Matt Lee's init file  -*- lexical-binding: t -*-

;;; Commentary:
;; Lots of tweaks to my Emacs.

;;; Code:

(defvar matt-init-start-time (current-time))
(defvar matt-init-stop-time)

(when (getenv "emacs_perf")
  (defun matt-require-perf-wrapper (orig feature &rest args)
    (let* ((require-start-time (current-time))
           (loaded (apply orig feature args)))
      (message "%06dμs | %s"
               (* 1000 1000 (float-time (time-subtract (current-time) require-start-time)))
               feature)
      loaded))
  (advice-add 'require :around 'matt-require-perf-wrapper))

;; gc tweaks - see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq garbage-collection-messages t)

;; time a gc collection
;; (let ((before gc-elapsed)) (garbage-collect) (- gc-elapsed before))

(defun matt-gc-inhibit ()
  "Increase the gc threshold to prevent garbage collection."
  (setq gc-cons-threshold (* 512 1024 1024)))
;; (add-hook 'minibuffer-setup-hook #'matt-gc-inhibit)
;; (remove-hook 'minibuffer-setup-hook #'matt-gc-inhibit)

(defun matt-gc-uninhibit ()
  "Return the gc threshold to a normal level."
  (setq gc-cons-threshold (* 8 1024 1024)))
;; (add-hook 'minibuffer-exit-hook #'matt-gc-uninhibit)
;; (remove-hook 'minibuffer-exit-hook #'matt-gc-uninhibit)

(defvar matt-gc-on-blur-timer nil)

(defun matt-gc-on-blur ()
  "Run gc when Emacs window loses focus AKA blurs.
Focus change event is debounced so we don't gc on focus."
  (if (frame-focus-state)
      (when matt-gc-on-blur-timer
        (cancel-timer matt-gc-on-blur-timer))
    (setq matt-gc-on-blur-timer (run-with-timer 5 nil #'garbage-collect))))

(add-function :after after-focus-change-function
              #'matt-gc-on-blur)

;; (remove-function after-focus-change-function #'matt-gc-on-blur)

(matt-gc-inhibit) ;; speedup startup

(require 'package)
(require 'seq)
(require 'cl-lib)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory)) ;; HACK: ensure matt-make-theme is on the load-path

(defun matt-recompile-packages ()
  "Recompile all packages in `package-user-dir'."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(defun dissoc (key alist)
  "Delete elements of ALIST where KEY is equal to the element's car."
  (delq (assoc key alist) alist))

;; prevent ctrl mouse scroll changing the font size
(setq mouse-wheel-scroll-amount (dissoc '(control) mouse-wheel-scroll-amount))
(setq mouse-wheel-scroll-amount (dissoc '(control meta) mouse-wheel-scroll-amount))

(mouse-wheel-mode 1)

;; horizontal mouse scroll
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t) ;; osx style

;; hide tool bar & menu bar & tab bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(tab-bar-mode -1)

;; scroll at same rate as the cursor - this stops the scroll "jumping" when you move off the top/bottom
(setq scroll-conservatively 10)

;; display keystrokes almost immediately
(setq echo-keystrokes 0.3)

(setq ring-bell-function 'ignore)

;; keys
(defvar matt-keymap (make-sparse-keymap))
(global-set-key (kbd "M-m") matt-keymap)
(defun matt-define-key (key def)
  "Define key sequence KEY as DEF in personal keymap."
  (define-key matt-keymap (kbd key) def))

;; themes
(setq custom-theme-load-path
      (cons (expand-file-name "themes/" user-emacs-directory) custom-theme-load-path))

(defun matt-disable-current-theme ()
  "Disable the current theme, assumes only one theme is enabled."
  (interactive)
  (disable-theme (car custom-enabled-themes)))

(defun matt-disable-all-themes ()
  "Disable all loaded themes."
  (interactive)
  (when (not (eql nil custom-enabled-themes))
    (matt-disable-current-theme)
    (matt-disable-all-themes)))
(matt-define-key "t ESC" 'matt-disable-all-themes)

(defun matt-load-theme (theme)
  "Load THEME after disabling all loaded themes."
  (interactive
   (list
    (intern
     (completing-read "Load custom theme: "
                      (mapcar 'symbol-name (custom-available-themes))))))
  (matt-disable-all-themes)
  (load-theme theme t))
(matt-define-key "t l" 'matt-load-theme)

(defun matt-reload-theme ()
  "Reload the current theme."
  (interactive)
  (matt-load-theme (car custom-enabled-themes)))
(matt-define-key "t r" 'matt-reload-theme)

(defvar matt-default-theme 'xorbit)

(defvar matt-secondary-theme 'gruuvem)

(defun matt-load-default-theme ()
  "Load the default theme, specified by `matt-default-theme'."
  (interactive)
  (matt-load-theme matt-default-theme))
(matt-define-key "t d" 'matt-load-default-theme)

(defun matt-toggle-theme ()
  "Toggle between default and secondary themes."
  (interactive)
  (matt-load-theme
   (if (eq matt-secondary-theme (car custom-enabled-themes))
       matt-default-theme
     matt-secondary-theme)))
(matt-define-key "t t" 'matt-toggle-theme)

;; font
(defun matt-font-size (&optional sz)
  "Set the default font size to SZ."
  (interactive "NFont size: ")
  (if (numberp sz)
      (set-face-attribute 'default nil :height (truncate (* sz 10)))
    (round (/ (face-attribute 'default :height) 10.0))))
(matt-define-key "f f" 'matt-font-size)

(defun matt-font-face-wide ()
  "Change the default font family to a wide font."
  (interactive)
  (set-face-attribute 'default nil :width 'normal :weight 'medium :slant 'normal :family "Inconsolata"))
(matt-define-key "f w" 'matt-font-face-wide)

(defun matt-font-face-narrow ()
  "Change the default font family to a narrow font."
  (interactive)
  (set-face-attribute 'default nil :width 'normal :weight 'medium :slant 'normal :family "Victor Mono"))
(matt-define-key "f n" 'matt-font-face-narrow)

;; (set-face-attribute 'default nil :family "Victor Mono")     ;; https://rubjo.github.io/victor-mono/
;; (set-face-attribute 'default nil :family "Iosevka")         ;; https://be5invis.github.io/Iosevka/
;; (set-face-attribute 'default nil :family "Inconsolata")     ;; apt: fonts-inconsolata
;; (set-face-attribute 'default nil :family "M+ 1mn")          ;; apt: fonts-mplus
;; (set-face-attribute 'default nil :family "M+ 1m")
;; (set-face-attribute 'default nil :family "VL Gothic")       ;; apt: fonts-vlgothic
;; (set-face-attribute 'default nil :family "Nimbus Mono L")
;; (set-face-attribute 'default nil :family "Liberation Mono") ;; apt: fonts-liberation
;; (set-face-attribute 'default nil :family "DejaVu Sans Mono")
;; (set-face-attribute 'default nil :family "Droid Sans Mono")
;; (set-face-attribute 'default nil :family "Mononoki")        ;; apt: fonts-mononoki
;; (set-face-attribute 'default nil :family "Space Mono")
;; (set-face-attribute 'default nil :family "Courier 10 Pitch")
;; (set-face-attribute 'default nil :family "Ubuntu Mono")

(defvar matt-font-size-default 14)

(defun matt-font-size-increase ()
  "Increase the font size by 1 point."
  (interactive)
  (matt-font-size (1+ (matt-font-size)))
  (message "Font size: %s" (matt-font-size)))
(global-set-key (kbd "C-=") 'matt-font-size-increase)

(defun matt-font-size-decrease ()
  "Decrease the font size by 1 point."
  (interactive)
  (matt-font-size (1- (matt-font-size)))
  (message "Font size: %s" (matt-font-size)))
(global-set-key (kbd "C--") 'matt-font-size-decrease)

(defun matt-font-size-default ()
  "Set the font to the default size, specified by `matt-font-size-default'."
  (interactive)
  (matt-font-size matt-font-size-default)
  (message "Font size: %s" (matt-font-size)))
(matt-define-key "f d" 'matt-font-size-default)

(matt-font-size-default)
(matt-font-face-narrow)

;; cursor - bar instead of a block
(set-default 'cursor-type '(bar . 2))

;; window title - include file's full path
(setq frame-title-format
      '("" invocation-name
        (:eval (when (and (boundp 'server-name)
                          (not (equal server-name "server")))
                 (concat " - " server-name)))
        " - "
        (:eval (if (buffer-file-name)
                   (replace-regexp-in-string "%" "%%" (abbreviate-file-name (buffer-file-name)))
                 "%b"))))

(defun matt-find-files (filenames)
  "Useful as eshell alias, FILENAMES can be multiple args or globs."
  (mapc 'find-file
        (if (consp (car filenames)) (car filenames) filenames)))

(defvar matt-mode-line-format-default)

(when (not (boundp 'matt-mode-line-format-default)) ;; make safe for re-evaluation
  (setq matt-mode-line-format-default mode-line-format))

(defvar matt-mode-line-format-minimal
  '("%e "
    mode-line-modified
    " "
    mode-line-buffer-identification
    mode-line-frame-identification
    mode-name
    ;; centre
    (:propertize " " display ((space :align-to (- right 10)))) ;; (length "[00:00:00]") => 10
    ;; right
    mode-line-misc-info))

(defun matt-toggle-minimal-mode-line ()
  "Toggle between the normal mode line and a minimal version."
  (interactive)
  (setq-default mode-line-format
                (if (eq mode-line-format matt-mode-line-format-minimal)
                    matt-mode-line-format-default
                  matt-mode-line-format-minimal)))
(matt-toggle-minimal-mode-line)

(defun matt-number-modes-cycle ()
  "Cycle through the column and line number modes."
  (interactive)
  (cond ((and line-number-mode column-number-mode)
         (line-number-mode -1)
         (column-number-mode -1))
        (line-number-mode
         (line-number-mode -1)
         (column-number-mode 1))
        (column-number-mode
         (line-number-mode 1)
         (column-number-mode 1))
        (:else
         (line-number-mode 1)
         (column-number-mode -1))))

(defun matt-wrap-mode ()
  "Interactively adjust the fill column for wrapping."
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map [left]  (lambda () (interactive) (setq fill-column (1- fill-column))))
    (define-key map [right] (lambda () (interactive) (setq fill-column (1+ fill-column))))
    (define-key map "?" (lambda () (interactive) (message "Current fill column: %s" fill-column)))
    (define-key map "v" 'display-fill-column-indicator-mode)
    (define-key map (kbd "M-q") 'prog-fill-reindent-defun)
    (set-transient-map map t nil "Adjust wrapping %k")))

(column-number-mode -1)
(line-number-mode -1)
(size-indication-mode -1)
(setq mode-line-percent-position nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (remove-hook 'before-save-hook 'delete-trailing-whitespace)
(setq kill-whole-line t)
(setq mouse-yank-at-point t) ;; middle click paste at point (not mouse pointer)
(setq require-final-newline nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 115)
(setq-default display-fill-column-indicator-character ?\u2506)
(global-auto-revert-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t) ;; delete the selection with a keypress
(save-place-mode 1)
(savehist-mode)
(setq history-delete-duplicates t)
(setq set-mark-command-repeat-pop t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; remove newlines with cycling whitespace
(setq cycle-spacing-actions '((just-one-space -) (delete-all-space -) restore))
;; (setq enable-recursive-minibuffers nil)
(setq read-process-output-max (* 1024 1024))

;; window splitting - always horizontal
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;; default to no wrap
(setq-default truncate-lines t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; enable stuff
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'scroll-left      'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "C-d")           'backward-delete-char)
(global-set-key (kbd "<M-backspace>") 'backward-kill-word)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)

(global-set-key (kbd "M-DEL")         'kill-word)
(global-set-key (kbd "<M-delete>")    'kill-word)
(global-set-key (kbd "M-SPC")         'cycle-spacing)

(matt-define-key "b b"                'bury-buffer)
(matt-define-key "r a"                'align-regexp)
(matt-define-key "s r"                'replace-string)
(matt-define-key "s l"                'sort-lines)
(matt-define-key "w b"                'balance-windows)
(matt-define-key "w l"                'toggle-truncate-lines)              ;; mnemonic "wrap lines"
(matt-define-key "w w"                'toggle-word-wrap)
(matt-define-key "w v"                'display-fill-column-indicator-mode) ;; mnemonic "wrap view"
(matt-define-key "w f"                'auto-fill-mode)                     ;; mnemonic "wrap fill"
(matt-define-key "w c"                'set-fill-column)                    ;; mnemonic "wrap column"
(matt-define-key "w m"                'matt-wrap-mode)
(matt-define-key "l n"                'display-line-numbers-mode)

(defvar matt-sound-command
  (cond ((executable-find "ffplay") (list "ffplay" "-nodisp" "-autoexit"))
        ((executable-find "paplay") (list "paplay"))
        ((executable-find "afplay") (list "afplay"))
        (t                          nil)))

(defvar matt-sound-default-file
  (cond ((file-exists-p "/usr/share/sounds/sound-icons/trumpet-12.wav") "/usr/share/sounds/sound-icons/trumpet-12.wav") ;; linux
        ((file-exists-p "/System/Library/Sounds/Funk.aiff") "/System/Library/Sounds/Funk.aiff") ;; osx
        (t nil))
  "The default sound file to be played by `matt-sound-play'.")

(defun matt-sound-play (&optional sound)
  "Play SOUND, an audio file, by shelling out to `matt-sound-command'."
  (let ((sound (or sound matt-sound-default-file)))
    (when (and sound
               (file-exists-p (expand-file-name sound))
               (stringp (car matt-sound-command))
               (executable-find (car matt-sound-command)))
      (apply 'start-process "matt-sound-command" nil
             (seq-concatenate 'list matt-sound-command (list (expand-file-name sound)))))))

;; (matt-sound-play "/System/Library/Sounds/Basso.aiff")
;; (matt-sound-play "/System/Library/Sounds/Blow.aiff")
;; (matt-sound-play "/System/Library/Sounds/Bottle.aiff")
;; (matt-sound-play "/System/Library/Sounds/Frog.aiff")
;; (matt-sound-play "/System/Library/Sounds/Funk.aiff")
;; (matt-sound-play "/System/Library/Sounds/Glass.aiff")
;; (matt-sound-play "/System/Library/Sounds/Hero.aiff")
;; (matt-sound-play "/System/Library/Sounds/Morse.aiff")
;; (matt-sound-play "/System/Library/Sounds/Ping.aiff")
;; (matt-sound-play "/System/Library/Sounds/Pop.aiff")
;; (matt-sound-play "/System/Library/Sounds/Purr.aiff")
;; (matt-sound-play "/System/Library/Sounds/Sosumi.aiff")
;; (matt-sound-play "/System/Library/Sounds/Submarine.aiff")
;; (matt-sound-play "/System/Library/Sounds/Tink.aiff")

;; (matt-sound-play "/usr/share/sounds/sound-icons/trumpet-12.wav")
;; (matt-sound-play "/usr/share/sounds/sound-icons/canary-long.wav")
;; (matt-sound-play "/usr/share/sounds/sound-icons/cembalo-2.wav")
;; (matt-sound-play "/usr/share/sounds/sound-icons/cembalo-6.wav")
;; (matt-sound-play "/usr/share/sounds/sound-icons/cembalo-12.wav")
;; (matt-sound-play "/usr/share/sounds/sound-icons/gummy-cat-2.wav")
;; (matt-sound-play "/usr/share/sounds/sound-icons/piano-3.wav")
;; (matt-sound-play "/usr/share/sounds/sound-icons/prompt.wav")
;; (matt-sound-play "/usr/share/sounds/sound-icons/xylofon.wav")
;; (matt-sound-play "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga")
;; (matt-sound-play "/usr/share/sounds/freedesktop/stereo/complete.oga")
;; (matt-sound-play "/usr/share/sounds/freedesktop/stereo/message.oga")
;; (matt-sound-play "/usr/share/sounds/gnome/default/alerts/glass.ogg")
;; (matt-sound-play "/usr/share/sounds/gnome/default/alerts/drip.ogg")

(defun matt-beep ()
  "Play the default audio file.  Beep!"
  (interactive)
  (matt-sound-play))

(use-package dabbrev
  :config
  (setq dabbrev-ignored-buffer-regexps (list (rx bos " "))))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (setq recentf-max-saved-items 500)
  (setq recentf-max-menu-items 15)
  (setq recentf-auto-cleanup 'never) ;; disable - can cause problems with remote files
  (recentf-mode 1))

(use-package dired
  :config
  (setq dired-listing-switches "-al --group-directories-first"))

(use-package comint
  :init
  (setq comint-input-ignoredups t))

(use-package isearch
  :bind (:map isearch-mode-map
              ("C-." . 'isearch-forward-symbol-at-point)))

(use-package buffer-naming
  :config
  (buffer-naming-load)
  (buffer-naming-set-fn 'project-el-buffer-naming-fn))

(use-package ffap
  :bind ("C-x C-f" . find-file-at-point))

(defvar matt-scratch-file-locations
  (list "dev/scratch*" "dev/*/scratch*" "dev/*/*/scratch*"
        "src/scratch*" "src/*/scratch*" "src/*/*/scratch*"))

(use-package project
  :config
  (defun matt-project-find-scratch ()
    (interactive)
    (let* ((pr                (project-current t))
           (dir               (project-root pr))
           (scratch-files     (seq-remove #'file-directory-p
                                          (mapcan (lambda (pattern)
                                                    (file-expand-wildcards (concat dir pattern) t))
                                                  matt-scratch-file-locations)))
           (current-file-name (buffer-file-name))
           (current-extension (when current-file-name (file-name-extension current-file-name)))
           (scratch-file      (if (= (length scratch-files) 1)
                                  (car scratch-files)
                                (car (seq-filter (lambda (file) (equal (file-name-extension file) current-extension))
                                                 scratch-files)))))
      (cond
       ((and scratch-files
             (or (null scratch-file)
                 (equal current-file-name scratch-file)))
        (find-file (completing-read "Open scratch file: " scratch-files)))
       ((and scratch-file (file-exists-p scratch-file))
        (find-file scratch-file))
       (t
        (message "No scratch file found")))))
  (defun matt-test-file-p (file)
    ;; (rx bol (* any) "/test/" (* any) "_test.clj" (optional (any "sc")) eol)
    (string-match-p "^.*/test/.*_test\\.clj[sc]?$" file))
  (defun matt-guess-test-file (src-file)
    (thread-last src-file
                 (replace-regexp-in-string "/src/" "/test/")
                 (replace-regexp-in-string "\\(\\.clj[sc]?\\)" "_test\\1")))
  (defun matt-try-guess-test-file (file-name)
    (when (and file-name (not (matt-test-file-p file-name)))
      (matt-guess-test-file file-name)))
  (defun matt-guess-src-file (test-file)
    (thread-last test-file
                 (replace-regexp-in-string "/test/" "/src/")
                 (replace-regexp-in-string "_test\\(\\.clj[sc]?\\)" "\\1")))
  (defun matt-guess-other-file (file)
    (when file
      (let ((other-file (if (matt-test-file-p file)
                            (matt-guess-src-file file)
                          (matt-guess-test-file file))))
        (when (and other-file
                   (not (equal file other-file))
                   (file-exists-p other-file))
          other-file))))
  (defun matt-toggle-between-src-and-test ()
    (interactive)
    (let* ((file-name (buffer-file-name))
           (other-file (matt-guess-other-file file-name))
           (test-file-name (matt-try-guess-test-file file-name)))
      (cond (other-file
             (find-file other-file))
            ((and test-file-name
                  (yes-or-no-p (format "Test file %s doesn't exist, create it?" test-file-name)))
             (find-file test-file-name))
            (t
             (message "No matching src or test file found")))))
  (defun matt-guess-related-file (file)
    (let* ((sibling-files (seq-remove (lambda (f) (or (file-directory-p f)
                                                      (file-equal-p f file)
                                                      (string-prefix-p ".#" f))) ;; lock files
                                      (directory-files (file-name-parent-directory file))))
           (ext-files     (seq-filter (lambda (f) (equal (url-file-extension f)
                                                         (url-file-extension file)))
                                      sibling-files))
           ;; NOTE: could improve this prefix calculation, for now this works with numeric prefixes
           (prefix-fn     (lambda (f) (when (string-match (rx (group (1+ num))) f) (match-string 0 f))))
           (file-prefix   (funcall prefix-fn (file-name-nondirectory file)))
           (prefix-files  (when file-prefix
                            (seq-filter (lambda (f) (equal file-prefix (funcall prefix-fn f)))
                                        ext-files))))
      (cond ((eq 1 (length sibling-files)) (car sibling-files))
            ((eq 1 (length ext-files)) (car ext-files))
            ((eq 1 (length prefix-files)) (car prefix-files)))))
  (defun matt-toggle-related-file ()
    (interactive)
    (let* ((file-name (buffer-file-name))
           (related-file (matt-guess-related-file file-name)))
      (if related-file
          (find-file related-file)
        (message "No related file found"))))
  :bind (:map matt-keymap
              ("M-p" . project-switch-project)
              ("p b" . project-switch-to-buffer)
              ("p f" . project-find-file)
              ("p t" . matt-toggle-between-src-and-test)
              ("p r" . matt-toggle-related-file)
              ("p s" . matt-project-find-scratch)))

(use-package ibuffer
  :init
  (setq ibuffer-default-sorting-mode 'filename/process) ;; groups file buffers together
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 52 52 :left :elide) " "
                filename-and-process)
          (mark modified read-only " "
                (name 25 25 :left :elide) " "
                (size 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                filename-and-process)))
  (use-package ibuffer-vc
    :ensure t
    :config
    (defun matt-ibuffer-vc-ignore-some-buffers (f buf)
      (when (or (buffer-file-name buf)
                (eq 'cider-repl-mode (with-current-buffer buf major-mode))
                (and (get-buffer-process buf)
                     (string-match-p "^nrepl" (process-name (get-buffer-process buf)))))
        (funcall f buf)))
    (advice-add 'ibuffer-vc-root :around 'matt-ibuffer-vc-ignore-some-buffers)
    :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))
  :bind ("C-x C-b" . (lambda () (interactive) (matt-clean-buffers) (ibuffer))))

(use-package vundo
  :ensure t
  :bind ("M-_" . vundo))

(use-package eshell
  :init
  (add-hook 'eshell-first-time-mode-hook ;; can't use `:bind' because `eshell-mode-map' is buffer local
            (lambda ()
              (define-key eshell-hist-mode-map [up] 'previous-line)
              (define-key eshell-hist-mode-map [down] 'next-line)
              (define-key eshell-hist-mode-map [C-up] 'eshell-previous-matching-input-from-input)
              (define-key eshell-hist-mode-map [C-down] 'eshell-next-matching-input-from-input)
              (define-key eshell-hist-mode-map [M-up] 'eshell-previous-input)
              (define-key eshell-hist-mode-map [M-down] 'eshell-next-input)
              (define-key eshell-mode-map [home] 'eshell-bol)
              (kill-local-variable 'mode-line-format))) ;; ensure matt-toggle-minimal-mode-line works
  (defun matt-eshell-current-directory ()
    (interactive)
    (let ((path (file-name-directory (or (buffer-file-name) default-directory))))
      (eshell)
      (cd path)
      (eshell-reset)))
  (defun matt-eshell-other-buffers-directory ()
    (interactive)
    (when-let* ((filename (buffer-file-name (other-buffer)))
                (path (file-name-directory filename)))
      (cd path)
      (eshell-reset)))
  (defun matt-eshell-dwim-directory ()
    (interactive)
    (if (equal major-mode 'eshell-mode)
        (matt-eshell-other-buffers-directory)
      (matt-eshell-current-directory)))
  :config
  ;; FIX: currently breaks the input ring
  ;; (setq eshell-hist-ignoredups 'erase)
  (setq eshell-history-size (* 1024 1024))
  :bind (:map matt-keymap
              ("o e" . eshell)
              ("o d" . matt-eshell-dwim-directory)))

(use-package tramp
  :defer t
  :init
  (autoload 'matt-file-reopen-as-root "tramp" nil t)
  :config
  ;; open as root - from emacs prelude
  (defun matt-file-owner-uid (filename)
    "Return the UID of the FILENAME as an integer. See `file-attributes'."
    (nth 2 (file-attributes filename 'integer)))
  (defun matt-file-owned-by-user-p (filename)
    "Return t if file FILENAME is owned by the currently logged in user."
    (equal (matt-file-owner-uid filename)
           (user-uid)))
  (defun matt-find-alternate-file-as-root (filename)
    "Wraps `find-alternate-file' with opening a file as root."
    (find-alternate-file (concat "/sudo:root@localhost:" filename)))
  (defun matt-file-reopen-as-root ()
    "Find file as root if necessary."
    (interactive)
    (unless (or (tramp-tramp-file-p buffer-file-name)
                (equal major-mode 'dired-mode)
                (not (file-exists-p (file-name-directory buffer-file-name)))
                (file-writable-p buffer-file-name)
                (matt-file-owned-by-user-p buffer-file-name))
      (matt-find-alternate-file-as-root buffer-file-name))))

(use-package bookmark
  :bind (:map matt-keymap
              ("b m" . bookmark-set)
              ("b j" . bookmark-jump)
              ("b l" . bookmark-bmenu-list)))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-width-start t)
  (setq display-line-numbers-widen t))

(use-package abbrev-mode
  :hook (sql-mode
         sql-interactive-mode
         clojure-mode
         org-mode))

(use-package smartparens
  :vc (:url "https://github.com/Fuco1/smartparens")
  :init
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  ;; (add-to-list 'sp-ignore-modes-list 'org-mode)
  :config
  (use-package smartparens-config) ;; fixes various issues, including single quote issue in lisps
  (sp-use-smartparens-bindings)
  (setq sp-highlight-pair-overlay nil))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq orderless-match-faces [match])
  (setq completion-styles '(orderless basic)) ;; reversing this order breaks cider's completion
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion))))
  (setq orderless-matching-styles '(orderless-literal orderless-initialism)))

(use-package corfu
  :ensure t
  :config
  (defun matt-corfu-on ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  :hook (minibuffer-setup . matt-corfu-on)
  :init
  (global-corfu-mode))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package consult
  :ensure t
  :bind (("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)
         ("C-x f" . consult-recent-file)
         ("S-<down>" . consult-buffer)
         (:map matt-keymap
               ("g g" . consult-ripgrep)
               ("M-b" . consult-bookmark)
               ("c l" . consult-line)
               ("c m" . consult-mark)
               ("c g" . consult-global-mark)
               ("c p" . consult-project-buffer)
               ("c k" . consult-locate))))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ;; "M-." . embark-dwim
         ("C-h B" . embark-bindings)
         )
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :init
  (setq rainbow-html-colors nil)
  (setq rainbow-x-colors nil)
  :hook (emacs-lisp-mode
         css-mode))

(use-package grep
  :config
  (setq grep-find-ignored-directories
        (append grep-find-ignored-directories '("target" "out" "node_modules" "build" "dist" "bower")))
  (add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1))))

(use-package idle-highlight-mode
  :ensure t
  :hook (prog-mode))

(use-package multiple-cursors
  :ensure t
  :bind (:map matt-keymap
              ("<down>" . mc/mark-more-like-this-extended)
              ("m r"    . mc/edit-lines)                            ;; mnemonic "multiple-cursors rectangle"
              ("r r"    . mc/mark-all-symbols-like-this-in-defun))) ;; mnemonic "refactor rename"

(use-package move-text
  :ensure t
  :bind (("C-S-<up>"   . move-text-up)
         ("C-S-<down>" . move-text-down)))

(use-package string-inflection
  :ensure t
  :config
  (defun string-inflection-snake-kebab-function (str)
    "foo_bar => foo-bar"
    (if (string-inflection-kebab-case-p str)
        (string-inflection-underscore-function str)
      (string-inflection-kebab-case-function str)))
  (defun string-inflection-snake-kebab ()
    "foo_bar => foo-bar"
    (interactive)
    (string-inflection--single-or-region #'string-inflection-snake-kebab-function))
  :bind (:map matt-keymap
              ("-"   . string-inflection-snake-kebab)
              ("M--" . string-inflection-all-cycle)))

(use-package csv-mode
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :init
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  :bind (("C-x g" . magit)
         (:map matt-keymap
               ("g l" . magit-log-buffer-file)
               ("g b" . magit-blame-addition)
               ("g c" . (lambda () (interactive) (find-file (concat (magit-gitdir) "config"))))
               ("g f" . magit-find-file)
               ("g d f" . magit-diff-buffer-file)))) ;; mnemonic "git diff file"

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode 1)
  :bind ("C-c g" . diff-hl-command-map))

(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain) ;; don't start another frame
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package calendar
  :init
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  (setq calendar-week-start-day 1)
  :bind (:map matt-keymap
              ("o c" . calendar)))

(use-package org
  :defer t
  :init
  (defface org-doing-face '((t (:inherit org-todo))) "org mode face for DOING items")
  (defface org-postponed-face '((t (:inherit org-done))) "org mode face for POSTPONED items")
  (defface org-query-face '((t (:inherit org-todo))) "org mode face for ??? (query) items")
  (setq org-fontify-done-headline nil)
  (setq org-todo-keywords '((sequence "TODO" "DOING" "DONE")
                            (sequence "BLOCKED")
                            (sequence "POSTPONED")
                            (sequence "???" "DONE")))
  (setq org-todo-keyword-faces
        '(("TODO" org-todo)
          ("DONE" org-done)
          ("DOING" org-doing-face)
          ("POSTPONED" org-postponed-face)
          ("BLOCKED" org-postponed-face)
          ("???" org-query-face)))
  (setq org-adapt-indentation t) ;; fixes indentation of drawers (e.g. logbook/clock)
  (setq org-startup-folded t)
  (setq org-cycle-hide-drawer-startup nil)
  :config
  (use-package org-clock)
  (setq org-duration-format 'h:mm) ;; display days as hours
  (defun matt-org-insert-timestamp ()
    (interactive)
    (org-insert-time-stamp (org-current-time 0) 'with-hm 'inactive))
  (defun matt-org-clock-in ()
    "A stripped down version of `org-clock-in' that just inserts
    the current time in the correct position & format."
    (interactive)
    (save-excursion
      (org-clock-find-position nil)
      (insert-before-markers "\n")
      (backward-char 1)
      (org-indent-line)
      (insert org-clock-string " ")
      (matt-org-insert-timestamp)))
  (defun matt-org-clock-out ()
    "A stripped down version of `org-clock-out' that just inserts
    the current time in the correct position & format."
    (interactive)
    (save-excursion
      (org-clock-find-position nil)
      ;; TODO: verify that we haven't already clocked out
      (goto-char (line-end-position))
      (insert "--")
      (matt-org-insert-timestamp)
      (org-clock-update-time-maybe)))
  (defun matt-org-clock-report ()
    (interactive)
    (save-excursion
      (org-clock-report t))
    ;; scroll to the clock report at the top of the page
    (scroll-down))
  (defun matt-org-clock-fortnight ()
    (interactive)
    (goto-char (line-beginning-position))
    (org-insert-heading nil nil :top)
    (insert "F?")
    (org-insert-subheading nil)
    (insert "Forecast")
    (let* (;; TODO: find the previous Monday, if not today
           (t1  (decode-time (current-time)))
           (d1  (encode-time (make-decoded-time :second 0
                                                :minute 0
                                                :hour   0
                                                :day    (decoded-time-day   t1)
                                                :month  (decoded-time-month t1)
                                                :year   (decoded-time-year  t1)
                                                :dst    (decoded-time-dst   t1)
                                                :zone   (decoded-time-zone  t1)))))
      (mapc (lambda (i)
              (org-clock-find-position nil)
              (insert-before-markers "\n")
              (backward-char 1)
              (org-indent-line)
              (insert org-clock-string " ")
              (org-insert-time-stamp (time-add d1 (+ (* i 24 60 60)
                                                     (* 8 60 60)))
                                     'with-hm 'inactive)
              (insert "--")
              (org-insert-time-stamp (time-add d1 (+ (* i 24 60 60)
                                                     (* 16 60 60)))
                                     'with-hm 'inactive)
              (org-clock-update-time-maybe))
            ;; MTWTF    MTWT
            '(0 1 2 3 4 7 8 9 10)))
    (org-insert-heading nil)
    (insert "W?")
    (matt-org-clock-in))
  :bind ((:map org-mode-map
               ("C-<up>"      . org-backward-element)
               ("C-<down>"    . org-forward-element)
               ("C-S-<up>"    . org-metaup)
               ("C-S-<down>"  . org-metadown)
               ("S-<up>"      . nil)
               ("S-<down>"    . nil)
               ("S-<left>"    . nil)
               ("S-<right>"   . nil)
               ("M-<left>"    . nil)
               ("M-<right>"   . nil)
               ("C-<left>"    . nil)
               ("C-<right>"   . nil)
               ("C-c <left>"  . org-shiftleft)
               ("C-c <right>" . org-shiftright)
               ("C-c <up>"    . org-shiftup)
               ("C-c <down>"  . org-shiftdown)
               ("M-."         . org-open-at-point))
         (:map matt-keymap
               ("c i" . matt-org-clock-in)
               ("c o" . matt-org-clock-out)
               ("c r" . matt-org-clock-report)
               ("c f" . matt-org-clock-fortnight)
               ("l l" . org-store-link)
               ("l t" . org-toggle-link-display))))

(use-package org-bullets
  :ensure t
  :init (setq org-bullets-bullet-list '("●" "○"))
  :hook (org-mode . org-bullets-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode
          lisp-mode
          clojure-mode
          cider-repl-mode
          scheme-mode)
         . rainbow-delimiters-mode-enable))

(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c C-k" . eval-buffer)))

(use-package elisp-slime-nav
  :ensure t
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

(use-package markdown-mode
  :ensure t
  :defer t
  :bind (:map markdown-mode-map
              ("M-<left>"  . nil)
              ("M-<right>" . nil)))

(use-package js
  :init
  (setq js-indent-level tab-width)
  :mode (("\\.json\\'" . js-mode)
         ("\\.cfn\\'"  . js-mode))) ;; cloud formation

(use-package lua-mode
  :ensure t
  :defer t)

(use-package css-mode
  :ensure t
  :config
  (setq css-indent-offset tab-width)
  :defer t)

(use-package cc-mode
  :init
  (add-hook 'java-mode-hook (lambda () (c-set-offset 'arglist-intro '+)))
  :defer t)

(use-package web-mode
  :ensure t
  :mode (("\\.jsx\\'"      . web-mode)
         ("\\.mustache\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset tab-width)
  (setq web-mode-code-indent-offset tab-width)
  (setq web-mode-enable-comment-interpolation nil))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (defun matt-flycheck-navigate ()
    (interactive)
    (let ((map (make-sparse-keymap)))
      (define-key map "n" 'flycheck-next-error)
      (define-key map "p" 'flycheck-previous-error)
      (define-key map [down] 'flycheck-next-error)
      (define-key map [up] 'flycheck-previous-error)
      (set-transient-map map t nil "Flycheck Navigate %k")))
  :bind (:map matt-keymap
              ("e e" . matt-flycheck-navigate)
              ("e l" . flycheck-list-errors)
              ("e n" . flycheck-next-error)
              ("e p" . flycheck-previous-error)
              ("e w" . flycheck-copy-errors-as-kill)))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (setq clojure-toplevel-inside-comment-form t)
  (put-clojure-indent 'do-at 1)
  (put-clojure-indent 'do-at* 1)
  (put-clojure-indent 'fact 1)
  (put-clojure-indent 'time! 2))

(use-package cider
  :ensure t
  :defer t
  :config
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-history-size 1000)
  (setq cider-repl-history-file (expand-file-name ".cider-repl-history" user-emacs-directory))
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-use-tooltips nil)
  (setq cider-test-fail-fast nil)
  (setq cider-inspector-max-coll-size 3) ;; reduce likelihood of wrapping in inspector
  (setq cider-enrich-classpath t)
  (setq cider-comment-prefix           ";; ")
  (setq cider-comment-continued-prefix ";; ")
  (setq cider-comment-postfix          "") ;; for completeness, but this is the default
  (defun matt-cider-repl-clear-buffer ()
    "Version of `cider-repl-clear-buffer' that can be used from another buffer."
    (interactive)
    (with-current-buffer (cider-current-repl nil 'ensure)
      (let ((inhibit-read-only t))
        (cider-repl--clear-region (point-min) cider-repl-prompt-start-mark)
        (cider-repl--clear-region cider-repl-output-start cider-repl-output-end)
        (when (< (point) cider-repl-input-start-mark)
          ;; goto-char doesn't work for visible windows - see https://emacs.stackexchange.com/q/21464
          (set-window-point (get-buffer-window (current-buffer)) (point-max))))
      (run-hooks 'cider-repl-clear-buffer-hook)))
  (defun matt-cider-repl-display-buffer ()
    (interactive)
    (display-buffer (cider-current-repl nil 'ensure)))
  ;; stolen from https://github.com/corgi-emacs/corgi-packages/blob/main/corgi-clojure/corgi-clojure.el
  (defun matt-cider-jack-in-babashka (&optional project-dir)
    (interactive)
    (let ((project-dir (or project-dir user-emacs-directory)))
      (nrepl-start-server-process
       project-dir
       "bb --nrepl-server 0"
       (lambda (server-buf)
         (set-process-query-on-exit-flag
          (get-buffer-process server-buf) nil)
         (cider-nrepl-connect
          (list :repl-buffer server-buf
                :repl-type 'clj
                :host (plist-get nrepl-endpoint :host)
                :port (plist-get nrepl-endpoint :port)
                :project-dir project-dir
                :session-name "babashka"
                :repl-init-function (lambda ()
                                      (setq-local cljr-suppress-no-project-warning t
                                                  cljr-suppress-middleware-warnings t
                                                  process-query-on-exit-flag nil)
                                      (set-process-query-on-exit-flag
                                       (get-buffer-process (current-buffer)) nil)
                                      (rename-buffer "*babashka-repl*"))))))))
  :bind ((:map cider-repl-mode-map
               ("C-<up>" . cider-repl-previous-input)
               ("C-<down>" . cider-repl-next-input))
         (:map cider-inspector-mode-map
               ("w" . toggle-truncate-lines))
         (:map matt-keymap
               ("M-c M-o" . matt-cider-repl-clear-buffer)
               ("M-c M-z" . matt-cider-repl-display-buffer))))

(defun matt-get-defun-name-at-point ()
  "Return the name of the defun at point."
  (save-excursion
    (beginning-of-defun)
    (let ((defun-first-line (thing-at-point 'line)))
      ;; TODO: handle metadata keys, e.g. ^:dev/before-load
      (string-match "^(def[-a-z]* \\([-*'_<>+=A-Za-z0-9?!]*\\)" defun-first-line)
      (set-text-properties 0 (length defun-first-line) nil defun-first-line)
      (match-string 1 defun-first-line))))

(defun matt-defun-name-at-point (&optional arg)
  "Grab the name of the defun at point, if ARG is given then insert."
  (interactive "P")
  (when-let ((defun-name (matt-get-defun-name-at-point)))
    (if arg
        (insert defun-name)
      (kill-new defun-name))))
(matt-define-key "i k" 'matt-defun-name-at-point)

(defun matt-insert-capture ()
  "Insert an environment capture form, named according to the containing function."
  (interactive)
  (insert "(matt.capture/capture :" (or (matt-get-defun-name-at-point) "capture") ")"))
(matt-define-key "i c" 'matt-insert-capture)

(defun matt-inspect-capture ()
  "Inspect the captured environment for capture-id at point."
  (interactive)
  (let ((capture-id (thing-at-point 'symbol)))
    ;; TODO: test that capture-id is a keyword: (string-prefix-p ":" capture-id)
    (cider-inspect-expr (concat "(matt.capture/captured-bindings " capture-id ")") nil)))
(matt-define-key "M-i" 'matt-inspect-capture) ;; like cider-inspect "C-c M-i"

(defun matt-cider-beep (&optional arg)
  "Beep! Continuous when prefix ARG is set."
  (interactive "P")
  (let ((form (cons 'matt.beep/beep
                    (when arg '(:continuous? true)))))
    (cider-interactive-eval (format "%S" form)))
  (message "Beep!"))
(matt-define-key "c b" 'matt-cider-beep)

(defun matt-cider-kill-ring-save-qualified-defun-name ()
  "Save the qualified name of the function at point to the kill ring."
  (interactive)
  (cider-interactive-eval (concat "`" (matt-get-defun-name-at-point))
                          (nrepl-make-response-handler (current-buffer)
                                                       (lambda (_buffer value)
                                                         (message "Saved: %s" value)
                                                         (kill-new value))
                                                       nil
                                                       nil
                                                       nil)))

(defun matt-cider-kill-ring-save-qualified-symbol-name ()
  "Save the qualified name of the symbol at point to the kill ring."
  (interactive)
  (when-let ((s (thing-at-point 'symbol)))
    (cider-interactive-eval (concat "`" s)
                            (nrepl-make-response-handler (current-buffer)
                                                         (lambda (_buffer value)
                                                           (message "Saved: %s" value)
                                                           (kill-new value))
                                                         nil
                                                         nil
                                                         nil))))

(defun matt-cider-unmap-ns-vars ()
  "Unmap all of the current namespaces public symbols."
  (interactive)
  (cider-interactive-eval
   (format "%S" '(->> *ns* ns-interns keys sort (mapv (fn [s] (ns-unmap *ns* s)))))))

(use-package sql
  :defer t
  :config
  (defun matt-sql-init ()
    (message "matt-sql-init")
    ;; remove buffer local completion, so global cape completion is used
    (remove-hook 'completion-at-point-functions 'comint-completion-at-point t))
  :init
  (setq sql-input-ring-file-name (expand-file-name ".sql-history" user-emacs-directory))
  (add-hook 'sql-login-hook 'matt-sql-init))

(use-package fennel-mode
  :mode "\\.fnl\\'"
  :load-path (lambda () (expand-file-name "~/code/fennel-mode"))
  :config
  (defun matt-fennel-init ()
    (fennel-proto-repl-minor-mode)
    (setq fennel-program
          (expand-file-name "~/code/fennel/fennel --repl")))
  :hook (fennel-mode . matt-fennel-init))

(use-package ruby-mode
  :ensure t
  :mode (("\\.pp\\'"          . ruby-mode)
         ("\\^Vagrantfile\\'" . ruby-mode)))

(use-package go-mode
  :ensure t
  :config
  (defun matt-go-init ()
    (add-hook 'before-save-hook 'gofmt nil t))
  :init
  (setq gofmt-command "goimports") ;; go install golang.org/x/tools/cmd/goimports@latest
  :hook (go-mode . matt-go-init))

(use-package image
  :bind (:map image-map
              ("=" . image-increase-size)))

(use-package alarm
  :config
  (defun alarm-sound (&rest _)
    (matt-sound-play))
  (add-hook 'alarm-hook 'alarm-sound)
  (add-hook 'alarm-hook 'alarm-message)
  :bind (:map matt-keymap
              ("a a" . alarm)
              ("a l" . alarm-list)
              ("a n" . alarm-next)))

(use-package mortimer
  :config
  (defun mortimer-sound (&rest _)
    (matt-sound-play))
  (add-hook 'mortimer-complete-hook 'mortimer-sound)
  :bind (:map matt-keymap
              ("m s" . mortimer-start)
              ("m p" . mortimer-pause-resume)
              ("m m" . mortimer-quick-toggle)
              ("m l" . mortimer-view-log)
              ("m k" . mortimer-clear-log)))

(defun matt-font-lock-comment-annotations ()
  "Highlight well known comment annotations."
  (font-lock-add-keywords
   nil '((" \\<\\(\\(FIX\\|TODO\\|NOTE\\|HACK\\|DEBUG\\|XXX\\|\\?\\?\\?\\)\\)[: ]"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'matt-font-lock-comment-annotations)
(add-hook 'web-mode-hook 'matt-font-lock-comment-annotations)
(add-hook 'css-mode-hook 'matt-font-lock-comment-annotations)
(add-hook 'conf-mode-hook 'matt-font-lock-comment-annotations)
(add-hook 'sql-mode-hook 'matt-font-lock-comment-annotations)

(defun matt-kill-this-buffer ()
  "Kill buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'matt-kill-this-buffer)

(defun matt-mru-buffer ()
  "Switch to the most recently used buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun matt-other-window-or-split ()
  "Switch to another window, creating one if needed.
New window's buffer is selected according to `matt-mru-buffer'."
  (interactive)
  (if (> (count-windows) 1)
      (other-window 1)
    (matt-mru-buffer)
    (split-window-right)
    (matt-mru-buffer)))
(defun matt-delete-other-windows-or-split ()
  (interactive)
  (if (> (count-windows) 1)
      (delete-other-windows)
    (split-window-right)
    (matt-mru-buffer)
    (other-window 1)))
(global-set-key (kbd "S-<up>") 'matt-mru-buffer)
(global-set-key (kbd "S-<right>") 'matt-other-window-or-split)
(global-set-key (kbd "S-<left>") 'matt-delete-other-windows-or-split)

(defun matt-disposable-buffer-p (buf)
  "The buffer BUF is disposable, i.e. can be killed safely."
  (and (null (get-buffer-process buf)) ;; ref. cider repl buffer
       (string-match-p
        (rx (and bos
                 (or "*Messages*" "*Completions*" "*info*" "*Help*" "*Backtrace*" "*Warnings*"
                     "*Compile-Log*" "*Async-native-compile-log*" "*Shell Command Output*"
                     "*Occur*" "*vc*" "*vc-" "*Man " "*WoMan" "*Calendar*" "*Ediff" "*Bookmark List*"
                     "*Packages*" "magit-" "*ag search" "*eldoc" "*Alarm*" "*Alarm List*" "*cider-"
                     "*sesman " "*Mortimer*")))
        (buffer-name buf))))

(defun matt-disposable-major-mode-p (buf)
  (eq 'dired-mode
      (with-current-buffer buf
        major-mode)))

(defun matt-persistent-buffer-p (buf)
  (eq 'org-mode
      (with-current-buffer buf
        major-mode)))

(defun matt-clean-buffers (&optional arg)
  "Kill all \"disposable\" buffers.
With prefix ARG also kill all unmodified file buffers."
  (interactive "P")
  (thread-last (buffer-list)
    (seq-filter (lambda (buf)
                  (or (matt-disposable-buffer-p buf)
                      (matt-disposable-major-mode-p buf)
                      (and arg
                           (not (matt-persistent-buffer-p buf))
                           (buffer-file-name buf)
                           (not (buffer-modified-p buf))))))
    (seq-do 'kill-buffer)))
(matt-define-key "k k" 'matt-clean-buffers)

(defun matt-swap-windows ()
  "Swap the buffers in `selected-window' and `next-window'."
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))
(matt-define-key "w s" 'matt-swap-windows)

(defvar matt-window-delta 4)

(defun matt-window-adjust ()
  "Interactively adjust window size."
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map "-" (lambda () (interactive)
                          (shrink-window matt-window-delta (not (window--resizable-p nil (- matt-window-delta))))))
    (define-key map "=" (lambda () (interactive)
                          (enlarge-window matt-window-delta (not (window--resizable-p nil matt-window-delta)))))
    (define-key map "_" (lambda () (interactive)
                          (shrink-window matt-window-delta 'horizontal)))
    (define-key map "+" (lambda () (interactive)
                          (enlarge-window matt-window-delta 'horizontal)))
    (define-key map "[" (lambda () (interactive)
                          (shrink-window matt-window-delta 'horizontal)))
    (define-key map "]" (lambda () (interactive)
                          (enlarge-window matt-window-delta 'horizontal)))
    (define-key map "o" 'other-window)
    (define-key map "s" 'matt-swap-windows)
    (define-key map "b" 'balance-windows)
    (define-key map "0" 'delete-window)
    (define-key map "1" 'delete-other-windows)
    (define-key map "2" 'split-window-below)
    (define-key map "3" 'split-window-right)
    (define-key map "n" 'next-buffer)
    (define-key map "p" 'previous-buffer)
    (set-transient-map map t nil "Adjust windows %k")))
(matt-define-key "w a" 'matt-window-adjust)
(matt-define-key "M-w" 'matt-window-adjust)

(defun matt-toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
  (interactive)
  (let ((window (get-buffer-window (current-buffer))))
    (message
     (if (set-window-dedicated-p window
                                 (not (window-dedicated-p window)))
         "Window '%s' is dedicated"
       "Window '%s' is normal")
     window)))

(defun matt-scroll-mode ()
  "Enter a mode where arrow keys scroll and the cursor stays centered."
  (interactive)
  (let ((map (make-sparse-keymap))
        (go-up   (lambda () (interactive) (forward-line -1) (recenter nil t)))
        (go-down (lambda () (interactive) (forward-line 1) (recenter nil t)))
        (fwd-p   (lambda () (interactive) (forward-paragraph 1) (recenter nil t)))
        (bkwd-p  (lambda () (interactive) (forward-paragraph -1) (recenter nil t)))
        (quit-scroll-mode))
    (define-key map [up] go-up)
    (define-key map [down] go-down)
    (define-key map [C-up] bkwd-p)
    (define-key map [C-down] fwd-p)
    (define-key map "q" (lambda () (interactive) (funcall quit-scroll-mode)))
    (move-to-window-line nil)
    (hl-line-mode 1)
    (setq quit-scroll-mode
          (set-transient-map map t (lambda () (hl-line-mode -1)) "Scroll! %k"))))
(matt-define-key "<up>" 'matt-scroll-mode)

(defun matt-time-string-to-numeric (time-string)
  "Convert human readable TIME-STRING to a numeric value (minutes or hours).
e.g. \"30:30\" becomes 30.5."
  (cl-destructuring-bind
      (minutes seconds) (mapcar 'string-to-number (split-string time-string ":" t))
    (+ minutes (/ seconds 60.0))))

(defun matt-time-numeric-to-string (time)
  "Convert a numeric TIME value (minutes or hours) to human readable times.
e.g. 30.5 becomes \"30:30\"."
  (let* ((tm (abs time))
         (ms (floor tm))
         (ss (round (* 60 (- tm ms)))))
    (format "%s%02d:%02d" (if (< 0 time) "" "-") ms ss)))

(defun matt-time-numeric-to-symbol (time)
  "Convert a numeric TIME form to a symbol.
e.g. 30.5 becomes '30:30."
  (intern (matt-time-numeric-to-string time)))

(defun matt-symbol-or-string-to-string (s)
  "Convert S to a string."
  (cond ((stringp s) s)
        ((null s) nil) ;; (symbolp nil) => true!
        ((symbolp s) (symbol-name s))))

(defun matt-time-p (x)
  "Return t if X is a time symbol or string.
e.g. '11:00 or \"10:30\"."
  (when-let ((s (matt-symbol-or-string-to-string x)))
    (when (string-match-p "^[0-9]+:[0-9][0-9]$" s)
      t)))

(defun matt-time-prepare (x)
  "Prepare the form X, a time maths expression, to Lisp."
  (cond
   ((matt-time-p x) `(matt-time-string-to-numeric (matt-symbol-or-string-to-string ',x)))
   ((consp x) (mapcar 'matt-time-prepare x))
   (t x)))

(defmacro matt-time-eval (form)
  "Evalute FORM using time maths."
  `(matt-time-numeric-to-symbol ,(matt-time-prepare form)))

(defvar matt-journal-file "~/Documents/journal.org")

(defun matt-journal ()
  "Open my journal."
  (interactive)
  (find-file matt-journal-file))

(defvar matt-log-file "~/Documents/log.org")

(defun matt-log ()
  "Open my log."
  (interactive)
  (find-file matt-log-file))

(defun matt-journal+log-window-configuration-p ()
  "Return t if my journal & log are open."
  (and (< 1 (length (window-list)))
       (or (equal (expand-file-name matt-log-file)
                  (buffer-file-name))
           (equal (expand-file-name matt-journal-file)
                  (buffer-file-name)))))

(defvar matt-journal+log-window-configuration-stash nil)

(defun matt-journal+log ()
  "Open my journal & log."
  (interactive)
  (setq matt-journal+log-window-configuration-stash (current-window-configuration))
  (delete-other-windows)
  (matt-log)
  (split-window nil nil t)
  (matt-journal))

(defun matt-journal+log-back ()
  "Close my journal & log."
  (interactive)
  (when matt-journal+log-window-configuration-stash
    (set-window-configuration matt-journal+log-window-configuration-stash)
    ;; ensure the journal and log don't just "reappear" on next matt-mru-buffer call
    (bury-buffer (find-file-noselect matt-journal-file))
    (bury-buffer (find-file-noselect matt-log-file))))

(defun matt-journal+log-toggle ()
  "Open/close my journal & log."
  (interactive)
  (if (matt-journal+log-window-configuration-p)
      (matt-journal+log-back)
    (matt-journal+log)))
(matt-define-key "M-m" 'matt-journal+log-toggle)

(defun matt-runs ()
  "Open the file used to track my running."
  (interactive)
  (find-file "~/notes/runs.org"))
(matt-define-key "o r" 'matt-runs)

(matt-define-key "M-a" (lambda () (interactive) (jump-to-register ?a)))
(matt-define-key "M-s" (lambda () (interactive) (jump-to-register ?s)))
(matt-define-key "M-d" (lambda () (interactive) (jump-to-register ?d)))
(matt-define-key "M-f" (lambda () (interactive) (jump-to-register ?f)))

;; A         B        C                            D        E   F          G    H   I  J   K  L  M  N         O  P  Q  R              S            T        U  V  W  X        Y    Z
;; Wednesday February 20                           02/07/18 %E  2018-02-07 2018 15  03 %J  %K %L 28 312138752 %O pm %Q 15:28          35           15:28:35 05 06 06 15:28:35 2018 GMT
;; a         b        c                            d        e   f          g    h   i  j   k  l  m  n         o  p  q  r              s            t        u  v  w  x        y    z
;; Wed       Feb      Wed 07 Feb 2018 15:28:35 GMT 07       7   %f         18   Feb %i 038 15  3 02 \n        %o pm 1  3:28:35 pm GMT 1518017315   \t       3  %v 3  07/02/18 18   +0000
(defun matt-insert-date (arg)
  "Insert the current date, use prefix ARG to select format.
e.g 2018-02-08, 08-Feb-2018 or Thu, 08 Feb 2018."
  (interactive "p")
  (insert (format-time-string
           (cond
            ((eq arg 16) "%a, %d %b %Y") ;; Thu, 08 Feb 2018
            ((eq arg 4)  "%d-%b-%Y")     ;; 08-Feb-2018
            (t           "%F"))          ;; 2018-02-08
           (current-time)))) ;;
(matt-define-key "i d" 'matt-insert-date)

(defun matt-insert-time (arg)
  "Insert the current time, use prefix ARG to select format.
e.g. 16:34:42, 16:34, 2018-02-08T16:34:42+0000 or 2018-02-08_16-34-42."
  (interactive "p")
  (insert (format-time-string
           (cond
            ((eq arg 64) "%Y-%m-%d_%H-%M-%S") ;; 2018-02-08_16-34-42
            ((eq arg 16) "%FT%T%z")           ;; 2018-02-08T16:34:42+0000
            ((eq arg 4)  "%R")                ;; 16:34
            (t           "%T"))               ;; 16:34:42
           (current-time))))
(matt-define-key "i t" 'matt-insert-time)

(defun matt-insert-timestamp (arg)
  "Insert a current timestamp, use prefix ARG to select format.
e.g. 2020012016131337, 2020-01-20_16-13-13, 1621854380123 or 1621854380."
  (interactive "p")
  (insert (format-time-string
           (cond
            ((eq arg 64) "%s")                ;; 1621854575
            ((eq arg 16) "%s%3N")             ;; 1621854572121
            ((eq arg 4)  "%Y-%m-%d_%H-%M-%S") ;; 2021-05-24_12-03-45
            (t           "%Y%m%d%H%M%S%2N"))  ;; 2021052412035569
           (current-time))))
(matt-define-key "i s" 'matt-insert-timestamp)

(defun matt-insert-underline (c)
  "Underline the line above with the character C."
  (interactive "cCharacter:")
  (forward-line -1)
  (let ((b (point))
        (e (point-at-eol)))
    (forward-line)
    (insert (make-string (- e b) c))))
(matt-define-key "i u" 'matt-insert-underline)

(defun matt-open-init ()
  "Open my Emacs init file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
(matt-define-key "o i" 'matt-open-init)

(defun matt-open-theme ()
  "Open my Emacs theme file."
  (interactive)
  (if-let ((theme (car custom-enabled-themes)))
      (find-file (expand-file-name (format "themes/%s-theme.el" theme) user-emacs-directory))
    (message "No theme currently active")))
(matt-define-key "o t" 'matt-open-theme)

(defun matt-open-todo ()
  "Open my todo file."
  (interactive)
  (find-file "~/notes/todo.org"))

(defun matt-open-awesome-rc ()
  "Open my awesome rc.lua file."
  (interactive)
  (find-file "~/.config/awesome/rc.lua"))
(matt-define-key "o a" 'matt-open-awesome-rc)

(defun matt-open-keymap ()
  "Open my keyboard layout file."
  (interactive)
  (find-file "~/code/qmk_firmware/keyboards/crkbd/keymaps/thatismatt/keymap.c"))
(matt-define-key "o k" 'matt-open-keymap)

(defun matt-open-temp-file ()
  "Open a temporary file named with a timestamp."
  (interactive)
  (let* ((timestamp (format-time-string "%Y-%m-%d_%H-%M-%S" (current-time)))
         (filename  (format "~/tmp/emacs-temp-%s" timestamp)))
    (find-file filename)))

(defun matt-open-temp-buffer ()
  "Open a temporary buffer named with a timestamp."
  (interactive)
  (let* ((timestamp (format-time-string "%Y-%m-%d_%H-%M-%S" (current-time)))
         (buffer    (format "*temp-%s*" timestamp)))
    (switch-to-buffer buffer)))
(matt-define-key "o p" 'matt-open-temp-buffer)

(defun matt-org-title ()
  "Insert an org formatted title, based on the filename."
  (interactive)
  (let* ((date (format-time-string "%a, %d %B %Y" (current-time)))
         (title (capitalize (file-name-base (buffer-file-name)))))
    (insert (format "#+TITLE: %s - %s\n" title date))))

(defun matt-open-org-file (filename)
  "Open an org file named FILENAME with timestamp appended."
  (interactive "F")
  (let* ((timestamp (format-time-string "%Y-%m-%d" (current-time)))
         (filename-full (format "%s-%s.%s" filename timestamp "org")))
    (find-file filename-full)
    (matt-org-title)))
(matt-define-key "o o" 'matt-open-org-file)

(defun matt-create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*scratch*")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    (insert initial-scratch-message)))

(defun matt-open-scratch-buffer ()
  "Switch to the scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))
(matt-define-key "o s" 'matt-open-scratch-buffer)

(defun matt-open-messages-buffer ()
  "Switch to the messages buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))
(matt-define-key "o m" 'matt-open-messages-buffer)

(defun matt-delete-file-and-buffer ()
  "Kill the buffer and delete the associated file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (cond ((not filename)
           (message "This buffer is not associated with a file."))
          ((y-or-n-p (format "Are you sure you want to delete %s?" filename))
           (delete-file filename)
           (kill-buffer (current-buffer))
           (message "%s deleted." filename))
          (:else
           (message "%s not deleted." filename)))))
(matt-define-key "C-k" 'matt-delete-file-and-buffer)

(defun matt-indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(matt-define-key "<tab>" 'matt-indent-buffer)

(defun matt-insert-filename (&optional arg)
  "Insert filename of current buffer, with prefix ARG insert just filename base."
  (interactive "P")
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "This buffer is not associated with a file.")
      (insert (if arg (file-name-base filename) filename)))))
(matt-define-key "i f" 'matt-insert-filename)

(defun matt-insert-full-filename ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename) (message "This buffer is not associated with a file.")
      (insert filename))))
(matt-define-key "i C-f" 'matt-insert-full-filename)

(defun matt-wget (url)
  "Retrieve and return the content of URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (let ((content (buffer-substring (marker-position url-http-end-of-headers)
                                     (buffer-size))))
      (kill-buffer)
      content)))

(defun matt-insert-wget (url)
  "Retrieve and Insert the content of URL."
  (interactive "sURL: ")
  (let ((content (matt-wget url)))
    (insert content)))

(use-package json
  :defer t
  :config
  (defun matt-whats-my-ip ()
    (let ((json (json-read-from-string (matt-wget "http://ip.jsontest.com/"))))
      (cdr (assoc 'ip json))))
  (defun matt-insert-my-ip ()
    (interactive)
    (insert (matt-whats-my-ip)))
  :bind (:map matt-keymap
              ("i i" . matt-insert-my-ip)))

(defun matt-change-at-point (f)
  "Adjust the thing at point by applying F."
  (skip-chars-backward "0-9")
  (if (looking-at "[0-9]+")
      (let* ((raw (match-string 0))
             (format-string (format "%%0%dd" (length raw)))
             (new-value (max 0 (funcall f (string-to-number raw)))))
        (replace-match (format format-string new-value)))
    (error "Nothing matched at point")))

(defun matt-increment-at-point ()
  "Increment the thing at point."
  (interactive)
  (matt-change-at-point '1+))
(matt-define-key "p +" 'matt-increment-at-point)
(matt-define-key "p =" 'matt-increment-at-point)

(defun matt-decrement-at-point ()
  "Decrement the thing at point."
  (interactive)
  (matt-change-at-point '1-))
(matt-define-key "p -" 'matt-decrement-at-point)

(defun matt-adjust-at-point ()
  "Start interactive adjustment of the thing at point."
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map "-" 'matt-decrement-at-point)
    (define-key map "+" 'matt-increment-at-point)
    (define-key map "_" 'matt-decrement-at-point)
    (define-key map "=" 'matt-increment-at-point)
    (set-transient-map map t nil "Adjust at point %k")))
(matt-define-key "p p" 'matt-adjust-at-point)

(defun matt-kill-emacs ()
  "Like `save-buffers-kill-emacs' but with confirmation."
  (interactive)
  (when (yes-or-no-p "Kill Emacs?")
    (save-buffers-kill-emacs)))
(matt-define-key "q" 'matt-kill-emacs)

(defun matt-chmod+x ()
  "Make the current file executable."
  (interactive)
  (let ((filename (buffer-file-name)))
    (chmod filename
           (file-modes-symbolic-to-number "+x" (file-modes filename)))))

(defun matt-execute-buffer ()
  "Run this buffer as an async shell command."
  (interactive)
  (let ((filename (expand-file-name (buffer-file-name))))
    (if (file-executable-p filename)
        (async-shell-command filename (concat "*Execute " (abbreviate-file-name filename) "*"))
      (message "This buffer isn't associated with an executable file"))))

(defun matt-clear-text-properties (begin end)
  "Remove text properties between BEGIN and END, defaults to the region."
  (interactive "r")
  (set-text-properties begin end nil))

(defun matt-remove-read-only-region (begin end)
  "Remove read-only text property between BEGIN and END, defaults to the region."
  (interactive "r")
  (let ((inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))))

(defun matt-recenter-region (begin end)
  "Center the display on the region, i.e. between BEGIN and END."
  (interactive "r")
  (save-excursion
    (goto-char (point-min))
    (forward-line (/ (+ (line-number-at-pos begin)
                        (line-number-at-pos end))
                     2))
    (recenter nil t)))
(defun matt-recenter-region-top-bottom (&optional begin end)
  "Center on the region, or behave as `recenter-top-bottom'.
If the region is active BEGIN and END default to the region."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if (use-region-p)
      (matt-recenter-region begin end)
    (call-interactively 'recenter-top-bottom)))
;; matt-recenter-region-top-bottom hangs when called repeatedly in the minibuffer
(bind-key "C-l" 'matt-recenter-region-top-bottom nil (not (minibufferp)))

(defun matt-dwim-upcase (&optional begin end)
  "Upcase region or word, depending on whether the region is active.
If the region is active BEGIN and END default to the region."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if (use-region-p)
      (upcase-region begin end)
    (upcase-word 1)))
(global-set-key (kbd "M-u") 'matt-dwim-upcase)

(defun matt-dwim-downcase (&optional begin end)
  "Downcase region or word, depending on whether the region is active.
If the region is active BEGIN and END default to the region."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if (use-region-p)
      (downcase-region begin end)
    (downcase-word 1)))
(global-set-key (kbd "M-l") 'matt-dwim-downcase)

(defun matt-region-size ()
  "Report details about the region."
  (interactive)
  (let ((mk (or (mark) (point))) ;; initially mark is nil
        (pt (point)))
    (message "Size:  %s\nLines: %s\nPoint: %s\nMark:  %s"
             (abs (- mk pt))
             (1+ (abs (- (line-number-at-pos mk) (line-number-at-pos pt))))
             pt (mark))))
(matt-define-key "r s" 'matt-region-size)

(defun matt-random-name (&optional name syllables)
  "Generate a random name, with prefix NAME and SYLLABLES long."
  (let* ((common-closed '("b" "c" "d" "f" "g" "h" "k" "l" "m" "n" "p"
                          "r" "s" "t" "w"))
         (uncommon-closed '("x" "z" "q" "v" "v" "j" "j" "gg" "ll" "mm" "nn" "ss" "tt"))
         (enders '("b" "d" "g" "m" "n" "s" "r" "t"))
         ;; weight towards common letters
         (closed (append common-closed common-closed enders uncommon-closed))
         (vowels '("a" "e" "i" "o" "u" "ai" "au" "ie" "oa" "oi" "oo" "ou" "ue"))
         (syllables (or syllables (+ 2 (random 4))))
         (name (or name (concat (upcase (seq-random-elt common-closed))
                                (seq-random-elt vowels)
                                (seq-random-elt closed)))))
    (if (< syllables 3)
        (concat name (seq-random-elt vowels) (seq-random-elt enders))
      (matt-random-name (concat name (seq-random-elt vowels) (seq-random-elt closed))
                        (- syllables 1)))))

(defun matt-insert-random-name ()
  "Insert a random name, as generated by `matt-random-name'."
  (interactive)
  (insert (matt-random-name)))
(matt-define-key "i n" 'matt-insert-random-name)

(cond
 ((daemonp) (add-hook 'after-make-frame-functions
                      (lambda (frame)
                        (select-frame frame)
                        (matt-load-default-theme))))
 ((display-graphic-p) (matt-load-default-theme)))

;; require an init-local if present
(require 'init-local nil t)

(matt-gc-uninhibit)

(setq matt-init-stop-time (current-time))

(message "init time: %.2fms"
         (* 1000.0 (float-time (time-subtract matt-init-stop-time matt-init-start-time))))

(provide 'init)
;;; init.el ends here
