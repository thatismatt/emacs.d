;;; init --- Matt Lee's init file  -*- lexical-binding: t -*-

;;; Commentary:
;; Lots of tweaks to my Emacs.

;;; Code:

(defvar matt-init-start-time (current-time))

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

(defun matt-gc-inhibit ()
  "Increase the gc threshold to prevent garbage collection."
  (setq gc-cons-threshold (* 512 1024 1024)))
;; (add-hook 'minibuffer-setup-hook #'matt-gc-inhibit)
;; (remove-hook 'minibuffer-setup-hook #'matt-gc-inhibit)

(defun matt-gc-uninhibit ()
  "Return the gc threshold to a normal level."
  (setq gc-cons-threshold (* 8 1024 1024)))
;; 16 = long pauses on undo
;; (add-hook 'minibuffer-exit-hook #'matt-gc-uninhibit)
;; (remove-hook 'minibuffer-exit-hook #'matt-gc-uninhibit)

(add-hook 'focus-out-hook 'garbage-collect)

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

;; hide tool bar & menu bar & tab bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(tab-bar-mode -1)

;; scroll at same rate as the cursor - this stops the scroll "jumping" when you move off the top/bottom
(setq scroll-conservatively 1)

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

(defvar matt-default-theme 'witness)

(defun matt-load-default-theme ()
  "Load the default theme, specified by `matt-default-theme'."
  (interactive)
  (matt-load-theme matt-default-theme))
(matt-define-key "t d" 'matt-load-default-theme)

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
  (set-face-attribute 'default nil :family "Inconsolata"))
(matt-define-key "f w" 'matt-font-face-wide)

(defun matt-font-face-narrow ()
  "Change the default font family to a narrow font."
  (interactive)
  (set-face-attribute 'default nil :family "Iosevka"))
(matt-define-key "f n" 'matt-font-face-narrow)

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

(defvar matt-font-size-default 11)

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

(when (not (boundp 'matt-mode-line-format-default)) ;; make safe for re-evaluation
  (setq matt-mode-line-format-default mode-line-format))

(setq matt-mode-line-format-minimal
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
  (interactive)
  (setq-default mode-line-format
                (if (eq mode-line-format matt-mode-line-format-minimal)
                    matt-mode-line-format-default
                  matt-mode-line-format-minimal)))
(matt-toggle-minimal-mode-line)

(defun matt-number-modes-cycle ()
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
(setq-default tab-width 4)
(setq-default fill-column 100)
(setq-default display-fill-column-indicator-character ?\u2506)
(global-auto-revert-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t) ;; delete the selection with a keypress
(save-place-mode 1)
(savehist-mode)
(setq history-delete-duplicates t)
(setq set-mark-command-repeat-pop t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(global-set-key (kbd "C-d")           'backward-delete-char)
(global-set-key (kbd "<M-backspace>") 'backward-kill-word)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)

(global-set-key (kbd "M-DEL")         'kill-word)
(global-set-key (kbd "<M-delete>")    'kill-word)

(matt-define-key "b b"                'bury-buffer)
(matt-define-key "a r"                'align-regexp)
(matt-define-key "s r"                'replace-string)
(matt-define-key "s l"                'sort-lines)
(matt-define-key "w b"                'balance-windows)
(matt-define-key "w l"                'toggle-truncate-lines) ;; mnemonic "wrap lines"
(matt-define-key "w w"                'toggle-word-wrap)
(matt-define-key "w v"                'display-fill-column-indicator-mode) ;; mnemonic "wrap view"

(define-key isearch-mode-map (kbd "C-.") 'isearch-forward-symbol-at-point)

;; window splitting - always horizontal
(setq split-height-threshold 0)
(setq split-width-threshold nil)

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

(use-package buffer-naming
  :config
  (buffer-naming-load)
  (buffer-naming-set-fn 'projectile-buffer-naming-fn))

(use-package browse-url
  :init
  (setq browse-url-browser-function 'browse-url-chrome))

(use-package ffap
  :bind ("C-x C-f" . find-file-at-point))

(use-package selectrum
  :ensure t
  :hook
  (after-init . selectrum-mode)
  :config
  (selectrum-mode +1)
  (setq selectrum-max-window-height 25)
  (use-package selectrum-prescient
    :ensure t
    :config
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1))
  (defun selectrum-recentf-open-files ()
    (interactive)
    (let ((files (mapcar 'abbreviate-file-name recentf-list)))
      (find-file (completing-read "Find recent file: " files nil t))))
  :bind (("C-x f" . selectrum-recentf-open-files)
         (:map selectrum-minibuffer-map
               ("S-<down>" . abort-recursive-edit))))

(setq matt-scratch-file-locations
      (list "dev/scratch.*" "dev/*/scratch.*"
            "src/scratch.*" "src/*/scratch.*"))

(use-package projectile
  :ensure t
  :init
  (projectile-mode)
  :config
  (setq projectile-svn-command "find . -type f -not -iwholename '*.svn/*' -print0") ;; see https://github.com/bbatsov/projectile/issues/520
  (defun matt-projectile-guess-scratch-filename (&optional project-root)
    (let ((scratch-file (thread-last matt-scratch-file-locations
                          (mapcar (lambda (pattern) (file-expand-wildcards (concat (projectile-project-root project-root) pattern) t)))
                          (apply 'append)
                          car)))
      (when (and scratch-file (file-exists-p scratch-file))
        scratch-file)))
  (defun matt-projectile-find-scratch ()
    (interactive)
    (if-let ((scratch-file (or (matt-projectile-guess-scratch-filename)
                               (when-let* ((other-filename (buffer-file-name (other-buffer)))
                                           (other-directory (file-name-directory other-filename)))
                                 (matt-projectile-guess-scratch-filename other-directory)))))
        (find-file scratch-file)
      (message "No scratch file found")))
  :bind (("C-x p" . projectile-find-file)
         (:map matt-keymap
               ("p b" . projectile-switch-to-buffer)
               ("p t" . projectile-toggle-between-implementation-and-test)
               ("p s" . matt-projectile-find-scratch))))

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
  (setq eshell-hist-ignoredups t)
  (setq eshell-history-size 1024)
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
    "Return the UID of the FILENAME as an integer. See `file-attributes' for more info."
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

(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup 'never) ;; disable - can cause problems with remote files
(recentf-mode 1)

(use-package bookmark
  :bind (:map matt-keymap
              ("b m" . bookmark-set)
              ("b j" . bookmark-jump)
              ("b l" . bookmark-bmenu-list)))

(use-package display-line-numbers
  :init
  (global-display-line-numbers-mode)
  :config
  (setq display-line-numbers-width-start t)
  (setq display-line-numbers-widen t))

(use-package abbrev-mode
  :hook (sql-mode
         sql-interactive-mode
         clojure-mode
         org-mode))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (add-to-list 'sp-ignore-modes-list 'org-mode)
  :config
  (use-package smartparens-config)
  (sp-use-smartparens-bindings)
  (setq sp-highlight-pair-overlay nil))

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.1)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-flip-when-above t)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case 't)
  (global-company-mode 1))

(use-package rainbow-mode
  :ensure t
  :init
  (setq rainbow-html-colors nil)
  (setq rainbow-x-colors nil)
  :hook (emacs-lisp-mode css-mode))

(use-package grep
  :config
  (setq grep-find-ignored-directories (append grep-find-ignored-directories '("target" "out" "node_modules" "build" "dist" "bower")))
  (add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1))))

(use-package ag
  :ensure t
  :defer t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (add-hook 'ag-mode-hook
            '(lambda () (switch-to-buffer-other-window (current-buffer))))
  :bind (:map matt-keymap
              ("g d" . ag) ;; mnemonic "grep directory"
              ("g g" . ag-project)))

(use-package idle-highlight-mode
  :ensure t
  :hook (prog-mode))

(use-package multiple-cursors
  :ensure t
  :bind (:map matt-keymap
              ("<down>" . mc/mark-more-like-this-extended)
              ("m r"    . mc/edit-lines)))

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
    (string-inflection-insert
     (string-inflection-snake-kebab-function (string-inflection-get-current-word))))
  :bind (:map matt-keymap
              ("-"   . string-inflection-snake-kebab)
              ("M--" . string-inflection-all-cycle)))

(use-package magit
  :ensure t
  :init
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  :config
  (use-package magit-log
    :config
    (setf (nth 1 magit-log-margin) "%a %d %b %R"))
  :bind (("C-x g" . magit)
         (:map matt-keymap
               ("g l" . magit-log-buffer-file)
               ("g b" . magit-blame-addition)
               ("g f" . magit-find-file))))

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
  (remove-hook 'org-cycle-hook 'org-cycle-hide-drawers)
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
               ("l l" . org-store-link)
               ("l t" . org-toggle-link-display))))

(use-package org-bullets
  :ensure t
  :init (setq org-bullets-bullet-list '("●" "○" "✸" "✿"))
  :hook (org-mode . org-bullets-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode clojure-mode cider-repl-mode) . rainbow-delimiters-mode-enable))

(use-package elisp-slime-nav
  :ensure t
  :hook ((emacs-lisp-mode) . turn-on-elisp-slime-nav-mode))

(use-package markdown-mode
  :ensure t
  :defer t
  :bind (:map markdown-mode-map
              ("M-<left>"  . nil)
              ("M-<right>" . nil)))

(use-package js
  :init
  (setq js-indent-level 2)
  :mode (("\\.json\\'" . js-mode)
         ("\\.cfn\\'"  . js-mode))) ;; cloud formation

(use-package lua-mode
  :ensure t
  :defer t)

(use-package css-mode
  :ensure t
  :config
  (setq css-indent-offset 2)
  :defer t)

(use-package cc-mode
  :init
  (add-hook 'java-mode-hook '(lambda () (c-set-offset 'arglist-intro '+)))
  :defer t)

(use-package web-mode
  :ensure t
  :mode (("\\.jsx\\'"      . web-mode)
         ("\\.mustache\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-comment-interpolation nil))

(use-package clojure-mode
  :ensure t
  :defer t)

(use-package cider
  :ensure t
  :defer t
  :config
  (setq cider-repl-history-size 1000)
  (setq cider-repl-history-file (expand-file-name ".cider-repl-history" user-emacs-directory))

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
  :bind (:map matt-keymap
              ("M-c M-o" . matt-cider-repl-clear-buffer)
              ("M-c M-z" . matt-cider-repl-display-buffer)))

(use-package sql
  :defer t
  :init
  (setq sql-input-ring-file-name (expand-file-name ".sql-history" user-emacs-directory)))

(use-package fennel-mode
  :mode "\\.fnl\\'"
  :load-path (lambda () (expand-file-name "~/code/fennel-mode"))
  :config
  (defun matt-fennel-init ()
    (setq inferior-lisp-program
          (expand-file-name "~/code/fennel/fennel --repl")))
  :hook (fennel-mode . matt-fennel-init))

(use-package ruby-mode
  :ensure t
  :mode (("\\.pp\\'"          . ruby-mode)
         ("\\^Vagrantfile\\'" . ruby-mode)))

(use-package image
  :bind (:map image-map
              ("=" . image-increase-size)))

(use-package alarm
  :bind (:map matt-keymap
              ("a a" . alarm)
              ("a l" . alarm-list)
              ("a n" . alarm-next)))

(use-package mortimer
  :config
  (setq mortimer-sound "/usr/share/sounds/sound-icons/trumpet-12.wav")
  ;; (setq mortimer-sound "~/personal/dnb-loop.mp3")
  ;; (setq mortimer-sound "/usr/share/sounds/sound-icons/prompt.wav")
  ;; (mortimer-play-sound mortimer-sound)
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
(global-set-key (kbd "S-<down>") 'switch-to-buffer)
(global-set-key (kbd "S-<right>") 'matt-other-window-or-split)
(global-set-key (kbd "S-<left>") 'matt-delete-other-windows-or-split)

(defun matt-disposable-buffer-p (buf)
  (and (null (get-buffer-process buf)) ;; ref. cider repl buffer
       (string-match-p
        (rx (and bos
                 (or "*Messages*" "*Completions*" "*info*" "*Help*" "*Backtrace*" "*Warnings*"
                     "*Compile-Log*" "*Shell Command Output*" "*Occur*" "*vc*" "*vc-" "*Man "
                     "*WoMan" "*Calendar*" "*Ediff" "*Bookmark List*" "*Packages*"
                     "magit" "*ag search" "*eldoc" "*Alarm*" "*Alarm List*" "*cider-" "*sesman "
                     "*Mortimer*")))
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
  "Kill all \"disposable\" buffers. With prefix arg also kill all
  unmodified file buffers."
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
(matt-define-key "k" 'matt-clean-buffers)

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

(setq matt-window-delta 4)
(defun matt-window-adjust ()
  "Interactively adjust window size."
  (interactive)
  (message "Adjust windows.")
  (let ((map (make-sparse-keymap)))
    (define-key map "-" '(lambda () (interactive)
                           (shrink-window matt-window-delta (not (window--resizable-p nil (- matt-window-delta))))))
    (define-key map "=" '(lambda () (interactive)
                           (enlarge-window matt-window-delta (not (window--resizable-p nil matt-window-delta)))))
    (define-key map "_" '(lambda () (interactive)
                           (shrink-window matt-window-delta t)))
    (define-key map "+" '(lambda () (interactive)
                           (enlarge-window matt-window-delta t)))
    (define-key map "o" 'other-window)
    (define-key map "s" 'matt-swap-windows)
    (define-key map "b" 'balance-windows)
    (define-key map "0" 'delete-window)
    (define-key map "1" 'delete-other-windows)
    (define-key map "2" 'split-window-below)
    (define-key map "3" 'split-window-right)
    (define-key map "n" 'next-buffer)
    (define-key map "p" 'previous-buffer)
    (set-transient-map map t)))
(matt-define-key "w a" 'matt-window-adjust)
(matt-define-key "M-w" 'matt-window-adjust)

(defun matt-toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (let ((window (get-buffer-window (current-buffer))))
    (message
     (if (set-window-dedicated-p window
                                 (not (window-dedicated-p window)))
         "Window '%s' is dedicated"
       "Window '%s' is normal")
     window)))

(defun matt-just-one-space-multiline ()
  (interactive)
  (just-one-space -1))
(global-set-key (kbd "M-SPC") 'matt-just-one-space-multiline)

(defun matt-kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive)
  (save-excursion
    (progn
      (re-search-backward "[^ \t\r\n]" nil t)
      (re-search-forward "[ \t\r\n]+" nil t)
      (replace-match "" nil nil))))
(matt-define-key "j k" 'matt-kill-whitespace)

(defun matt-time-string-to-numeric (time-string)
  "Converts human readable times to a numeric value (minutes or hours), e.g. \"30:30\" becomes 30.5."
  (cl-destructuring-bind
      (minutes seconds) (mapcar 'string-to-number (split-string time-string ":" t))
    (+ minutes (/ seconds 60.0))))

(defun matt-time-numeric-to-string (time)
  "Converts a numeric value (minutes or hours) to human readable times, e.g. 30.5 becomes \"30:30\"."
  (let* ((ms (floor time))
         (ss (round (* 60 (- time ms)))))
    (format "%02d:%02d" ms ss)))

(defvar matt-journal-file "~/Documents/journal.org")

(defun matt-journal ()
  (interactive)
  (find-file matt-journal-file))
(matt-define-key "o j" 'matt-journal)

(defvar matt-log-file "~/Documents/log.org")

(defun matt-log ()
  (interactive)
  (find-file matt-log-file))
(matt-define-key "o l" 'matt-log)

(defun matt-journal+log-window-configuration-p ()
  (interactive)
  (let* ((window-child-count (frame-root-window))
         (left-window (window-child (frame-root-window)))
         (right-window (window-right left-window)))
    (and (equal (expand-file-name matt-journal-file)
                (buffer-file-name (window-buffer left-window)))
         (equal (expand-file-name matt-log-file)
                (buffer-file-name (window-buffer right-window))))))

(defvar matt-journal+log-window-configuration-stash nil)

(defun matt-journal+log ()
  (interactive)
  (setq matt-journal+log-window-configuration-stash (current-window-configuration))
  (delete-other-windows)
  (matt-log)
  (split-window nil nil t)
  (matt-journal))

(defun matt-log+journal ()
  (interactive)
  (matt-journal+log)
  (other-window 1))

(defun matt-journal+log-back ()
  (interactive)
  (when matt-journal+log-window-configuration-stash
    (set-window-configuration matt-journal+log-window-configuration-stash)))

(defun matt-journal+log-toggle ()
  (interactive)
  (if (matt-journal+log-window-configuration-p) ;; TODO: try testing non-nil stash as trigger for jumping back
      (matt-journal+log-back)
    (matt-journal+log)))
(matt-define-key "j j" 'matt-journal+log-toggle)

(defun matt-runs ()
  (interactive)
  (find-file "~/notes/runs.org"))
(matt-define-key "o r" 'matt-runs)

;; A         B        C                            D        E   F          G    H   I  J   K  L  M  N         O  P  Q  R              S            T        U  V  W  X        Y    Z
;; Wednesday February 20                           02/07/18 %E  2018-02-07 2018 15  03 %J  %K %L 28 312138752 %O pm %Q 15:28          35           15:28:35 05 06 06 15:28:35 2018 GMT
;; a         b        c                            d        e   f          g    h   i  j   k  l  m  n         o  p  q  r              s            t        u  v  w  x        y    z
;; Wed       Feb      Wed 07 Feb 2018 15:28:35 GMT 07       7   %f         18   Feb %i 038 15  3 02 \n        %o pm 1  3:28:35 pm GMT 1518017315   \t       3  %v 3  07/02/18 18   +0000
(defun matt-insert-date (arg)
  "Insert the current date. e.g 2018-02-08, 08-Feb-2018 or Thu, 08 Feb 2018."
  (interactive "p")
  (insert (format-time-string
           (cond
            ((eq arg 16) "%a, %d %b %Y") ;; Thu, 08 Feb 2018
            ((eq arg 4)  "%d-%b-%Y")     ;; 08-Feb-2018
            (t           "%F"))          ;; 2018-02-08
           (current-time)))) ;;
(matt-define-key "i d" 'matt-insert-date)

(defun matt-insert-time (arg)
  "Insert the current time. e.g. 16:34:42, 16:34, 2018-02-08T16:34:42+0000 or 2018-02-08_16-34-42."
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
  "Insert the current timestamp, e.g. 2020012016131337, 2020-01-20_16-13-13, 1621854380123 or 1621854380."
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
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
(matt-define-key "o i" 'matt-open-init)

(defun matt-open-theme ()
  (interactive)
  (if-let ((theme (car custom-enabled-themes)))
      (find-file (expand-file-name (format "themes/%s-theme.el" theme) user-emacs-directory))
    (message "No theme currently active")))
(matt-define-key "o t" 'matt-open-theme)

(defun matt-open-todo ()
  (interactive)
  (find-file "~/notes/todo.org"))

(defun matt-open-awesome-rc ()
  (interactive)
  (find-file "~/.config/awesome/rc.lua"))
(matt-define-key "o a" 'matt-open-awesome-rc)

(defun matt-open-temp-file ()
  (interactive)
  (let* ((timestamp (format-time-string "%Y-%m-%d_%H-%M-%S" (current-time)))
         (filename  (format "~/tmp/emacs-temp-%s" timestamp)))
    (find-file filename)))

(defun matt-open-temp-buffer ()
  (interactive)
  (let* ((timestamp (format-time-string "%Y-%m-%d_%H-%M-%S" (current-time)))
         (buffer    (format "*temp-%s*" timestamp)))
    (switch-to-buffer buffer)))
(matt-define-key "o p" 'matt-open-temp-buffer)

(defun matt-org-title ()
  (interactive)
  (let* ((date (format-time-string "%a, %d %B %Y" (current-time)))
         (title (file-name-base (buffer-file-name))))
    (insert (format "#+TITLE: %s - %s\n" title date))))

(defun matt-open-org-file (filename)
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
  (interactive)
  (indent-region (point-min) (point-max)))
(matt-define-key "<tab>" 'matt-indent-buffer)

(defun matt-insert-filename ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "This buffer is not associated with a file.")
      (insert (file-name-base filename)))))
(matt-define-key "i f" 'matt-insert-filename)

(defun matt-insert-full-filename ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename) (message "This buffer is not associated with a file.")
      (insert filename))))
(matt-define-key "i C-f" 'matt-insert-full-filename)

(defun matt-wget (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (let ((content (buffer-substring (marker-position url-http-end-of-headers)
                                     (buffer-size))))
      (kill-buffer)
      content)))

(defun matt-insert-wget (url)
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
  (skip-chars-backward "0-9")
  (if (looking-at "[0-9]+")
      (let* ((raw (match-string 0))
             (format-string (format "%%0%dd" (length raw)))
             (new-value (max 0 (funcall f (string-to-number raw)))))
        (replace-match (format format-string new-value)))
    (error "Nothing matched at point")))

(defun matt-increment-at-point ()
  (interactive)
  (matt-change-at-point '1+))
(matt-define-key "p +" 'matt-increment-at-point)
(matt-define-key "p =" 'matt-increment-at-point)

(defun matt-decrement-at-point ()
  (interactive)
  (matt-change-at-point '1-))
(matt-define-key "p -" 'matt-decrement-at-point)

(defun matt-adjust-at-point ()
  (interactive)
  (message "Adjust at point, + or -.")
  (let ((map (make-sparse-keymap)))
    (define-key map "-" 'matt-decrement-at-point)
    (define-key map "=" 'matt-increment-at-point)
    (define-key map "+" 'matt-increment-at-point)
    (set-transient-map map t)))
(matt-define-key "p p" 'matt-adjust-at-point)

(defun matt-kill-emacs ()
  "Like `save-buffers-kill-emacs' but with confirmation."
  (interactive)
  (when (y-or-n-p "Kill Emacs?")
    (save-buffers-kill-emacs)))
(matt-define-key "q" 'matt-kill-emacs)

(defun matt-chmod+x ()
  "Make the current file executable."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (chmod filename
           (file-modes-symbolic-to-number "+x" (file-modes filename)))))

(defun matt-clear-text-properties (begin end)
  (interactive "r")
  (set-text-properties begin end nil))

(defun matt-recenter-region (begin end)
  "Center region in the display."
  (interactive "r")
  (save-excursion
    (goto-char (/ (+ begin end) 2))
    (recenter nil t)))
(defun matt-recenter-region-top-bottom (&optional begin end)
  "If the region's active center on that, otherwise behave like `recenter-top-bottom'."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if (use-region-p)
      (matt-recenter-region begin end)
    (call-interactively 'recenter-top-bottom)))
;; matt-recenter-region-top-bottom hangs when called repeatedly in the minibuffer
(bind-key "C-l" 'matt-recenter-region-top-bottom nil (not (minibufferp)))

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

(defun matt-beep ()
  "Beep!"
  (interactive)
  (start-process "matt-beep" nil "paplay" "/usr/share/sounds/sound-icons/prompt.wav"))

(when (or (display-graphic-p)
          (daemonp))
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (matt-load-default-theme))))

;; require an init-local if present
(require 'init-local nil t)

(matt-gc-uninhibit)

(setq matt-init-stop-time (current-time))

(message "init time: %.2fms"
         (* 1000.0 (float-time (time-subtract matt-init-stop-time matt-init-start-time))))

(provide 'init)
;;; init.el ends here
