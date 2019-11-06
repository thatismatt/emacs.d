;;; init --- Matt Lee's init file

;;; Commentary:
;; Lots of tweaks to my Emacs.

;;; Code:

(setq matt-init-start-time (current-time))

;; gc tweaks - see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq garbage-collection-messages t)

(defun matt-gc-inhibit ()
  (setq gc-cons-threshold (* 512 1024 1024)))
(add-hook 'minibuffer-setup-hook #'matt-gc-inhibit)

(defun matt-gc-uninhibit ()
  (setq gc-cons-threshold (* 16 1024 1024)))
(add-hook 'minibuffer-exit-hook #'matt-gc-uninhibit)

(matt-gc-inhibit) ;; speedup startup

(require 'package)

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
(add-to-list 'load-path (expand-file-name "~/code/fennel-mode"))

;; hide tool bar & menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; scroll at same rate as the cursor - this stops the scroll "jumping" when you move off the top/bottom
(setq scroll-conservatively 1)

;; keys
(setq matt-keymap (make-sparse-keymap))
(global-set-key (kbd "M-m") matt-keymap)
(defun matt-define-key (key def)
  (define-key matt-keymap (kbd key) def))

;; themes
(setq custom-theme-load-path
      (cons "~/.emacs.d/themes/" custom-theme-load-path))

(defun matt-disable-current-theme ()
  (interactive)
  (disable-theme (car custom-enabled-themes)))

(defun matt-disable-all-themes ()
  (interactive)
  (when (not (eql nil custom-enabled-themes))
    (matt-disable-current-theme)
    (matt-disable-all-themes)))
(matt-define-key "t ESC" 'matt-disable-all-themes)

(defun matt-load-theme (theme)
  (interactive
   (list
    (intern
     (completing-read "Load custom theme: "
                      (mapcar 'symbol-name (custom-available-themes))))))
  (matt-disable-all-themes)
  (load-theme theme t))

(defun matt-reload-theme ()
  (interactive)
  (matt-load-theme (car custom-enabled-themes)))
(matt-define-key "t r" 'matt-reload-theme)

(defun matt-load-dark-theme ()
  (interactive)
  (matt-load-theme 'witness))
(matt-define-key "t d" 'matt-load-dark-theme)

(defun matt-load-light-theme ()
  (interactive)
  (matt-load-theme 'footlamp))
(matt-define-key "t l" 'matt-load-light-theme)

(defun matt-toggle-theme ()
  (interactive)
  (matt-load-theme
   (if (custom-theme-enabled-p 'witness) 'footlamp 'witness)))
(matt-define-key "t t" 'matt-toggle-theme)

;; font
(defun matt-font-size (sz)
  (interactive "NFont size: ")
  (set-face-attribute 'default nil :height (truncate (* sz 10))))
(matt-define-key "f f" 'matt-font-size)

(defun matt-font-face-wide ()
  (interactive)
  (set-face-attribute 'default nil :family "Inconsolata"))
(matt-define-key "f w" 'matt-font-face-wide)

(defun matt-font-face-narrow ()
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

(defun matt-font-size-small ()
  (interactive)
  (matt-font-size 11))
(matt-define-key "f s" 'matt-font-size-small)
(global-set-key (kbd "C--") 'matt-font-size-small)

(defun matt-font-size-large ()
  (interactive)
  (matt-font-size 18))
(matt-define-key "f l" 'matt-font-size-large)
(global-set-key (kbd "C-=") 'matt-font-size-large)

(matt-font-size-small)
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

(column-number-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq kill-whole-line t)
(setq mouse-yank-at-point t) ;; middle click paste at point (not mouse pointer)
(setq require-final-newline nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 100)
(global-auto-revert-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t) ;; delete the selection with a keypress
(savehist-mode)
(save-place-mode 1)
(global-set-key (kbd "C-d") 'backward-delete-char)
(setq set-mark-command-repeat-pop t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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

(use-package helm
  :ensure t
  :init
  (use-package helm-buffers
    :config
    (setq helm-buffer-max-length nil)
    (setq helm-boring-buffer-regexp-list
          (append helm-boring-buffer-regexp-list
                  '("\\`\\*Help\\*\\'" "\\`\\*Backtrace\\*\\'" "\\`\\*info\\*\\'" "\\`\\*Ibuffer\\*\\'"
                    "\\`\\*nrepl-server" "\\`\\*Alarm List\\*\\'" "\\`magit-"))))
  :config
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         ("S-<down>" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         (:map helm-map
               ("C-t" . helm-toggle-full-frame))))

(use-package projectile
  :ensure t
  :init
  (projectile-mode)
  :config
  (setq projectile-svn-command "find . -type f -not -iwholename '*.svn/*' -print0") ;; see https://github.com/bbatsov/projectile/issues/520
  :bind (:map matt-keymap
              ("p b" . projectile-switch-to-buffer)
              ("p t" . projectile-toggle-between-implementation-and-test)))

(use-package helm-ag
  :ensure t
  :init
  (setq helm-ag-insert-at-point 'symbol))

(use-package helm-projectile
  :ensure t
  :bind (("C-x p" . helm-projectile)
         (:map matt-keymap
               ("g p" . helm-projectile-ag))))

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
    :init
    (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root))
  :bind ("C-x C-b" . ibuffer))

(use-package eshell
  :init
  (add-hook 'eshell-first-time-mode-hook ;; can't use `:bind' because `eshell-mode-map' is buffer local
            (lambda ()
              (define-key eshell-mode-map [up] 'previous-line)
              (define-key eshell-mode-map [down] 'next-line)
              (define-key eshell-mode-map [C-up] 'eshell-previous-matching-input-from-input)
              (define-key eshell-mode-map [C-down] 'eshell-next-matching-input-from-input)
              (define-key eshell-mode-map [M-up] 'eshell-previous-input)
              (define-key eshell-mode-map [M-down] 'eshell-next-input)
              (define-key eshell-mode-map [home] 'eshell-bol)))
  :config
  (setq eshell-hist-ignoredups t)
  (setq eshell-history-size 1024)
  :bind (:map matt-keymap
              ("o e" . eshell)))

(require 'tramp)
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
    (matt-find-alternate-file-as-root buffer-file-name)))

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

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

(use-package smartparens
  :ensure t
  :init
  (use-package smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (add-to-list 'sp-ignore-modes-list 'org-mode)
  :config
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
  :config
  (setq ag-highlight-search t)
  :bind (:map matt-keymap
              ("g g" . ag)))

(use-package idle-highlight-mode
  :ensure t
  :hook (prog-mode))

(use-package multiple-cursors
  :ensure t
  :bind (:map matt-keymap
              ("m m" . mc/mark-more-like-this-extended)
              ("m r" . mc/edit-lines)))

(use-package move-text
  :ensure t
  :bind (("C-S-<up>" . move-text-up)
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
              ("-" . string-inflection-snake-kebab)
              ("M--" . string-inflection-all-cycle)))

(use-package magit
  :ensure t
  :init
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (require 'magit-log)
  (setf (nth 1 magit-log-margin) "%a %d %b %R")
  :bind ("C-x g" . magit))

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode 1)
  :bind ("C-c g" . diff-hl-command-map))

(use-package ediff
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain) ;; don't start another frame
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package calendar
  :init
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  :bind (:map matt-keymap
              ("o c" . calendar)))

(require 'org)
(require 'org-clock)
(defface org-todo-face '((t (:inherit org-todo))) "org mode face for TODO items")
(defface org-done-face '((t (:inherit org-done))) "org mode face for DONE items")
(defface org-doing-face '((t (:inherit org-todo))) "org mode face for DOING items")
(defface org-postponed-face '((t (:inherit org-done))) "org mode face for POSTPONED items")
(defface org-query-face '((t (:inherit org-todo))) "org mode face for ??? (query) items")
(setq org-todo-keywords '((sequence "TODO" "DOING" "DONE") (sequence "BLOCKED") (sequence "POSTPONED") (sequence "???" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" org-todo)
        ("DONE" org-done)
        ("DOING" org-doing-face)
        ("POSTPONED" org-postponed-face)
        ("BLOCKED" org-postponed-face)
        ("???" org-query-face)))
(setq org-duration-format 'h:mm) ;; display days as hours
(define-key org-mode-map (kbd "C-<up>") 'org-backward-element)
(define-key org-mode-map (kbd "C-<down>") 'org-forward-element)
(define-key org-mode-map (kbd "C-S-<up>") 'org-metaup)
(define-key org-mode-map (kbd "C-S-<down>") 'org-metadown)
(define-key org-mode-map (kbd "S-<up>") nil)
(define-key org-mode-map (kbd "S-<down>") nil)
(define-key org-mode-map (kbd "S-<left>") nil)
(define-key org-mode-map (kbd "S-<right>") nil)
(define-key org-mode-map (kbd "M-<left>") nil)
(define-key org-mode-map (kbd "M-<right>") nil)
(define-key org-mode-map (kbd "C-<left>") nil)
(define-key org-mode-map (kbd "C-<right>") nil)
(define-key org-mode-map (kbd "C-,") nil)
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
    (goto-char (point-min))
    (search-forward "#+BEGIN: clocktable")
    (org-show-entry)
    (org-clock-report))
  (scroll-down))
(matt-define-key "c i" 'matt-org-clock-in)
(matt-define-key "c o" 'matt-org-clock-out)
(matt-define-key "c r" 'matt-org-clock-report)
(matt-define-key "l l" 'org-store-link)
(matt-define-key "l t" 'org-toggle-link-display)

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode clojure-mode cider-repl-mode) . rainbow-delimiters-mode-enable))

(use-package elisp-slime-nav
  :ensure t
  :hook ((emacs-lisp-mode) . turn-on-elisp-slime-nav-mode))

(use-package markdown-mode
  :ensure t
  :bind (:map markdown-mode-map
              ("M-<left>" . nil)
              ("M-<right>" . nil)))

(use-package js
  :init
  (setq js-indent-level 2)
  :mode (("\\.json\\'" . js-mode)
         ("\\.cfn\\'" . js-mode))) ;; cloud formation

(use-package lua-mode
  :ensure t)

(use-package css-mode
  :ensure t
  :config
  (setq css-indent-offset 2))

(require 'cc-mode)
(add-hook 'java-mode-hook '(lambda () (c-set-offset 'arglist-intro '+)))

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-comment-interpolation nil)
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode)))

(use-package clojure-mode
  :ensure t
  :defer t)

(use-package cider
  :ensure t
  :defer t
  ;; :pin melpa-stable
  :config
  (setq cider-repl-history-size 1000)
  (setq cider-repl-history-file (expand-file-name ".cider-repl-history" user-emacs-directory)))

(use-package sql
  :init
  (setq sql-input-ring-file-name (expand-file-name ".sql-history" user-emacs-directory)))

(require 'fennel-mode)
(defun matt-fennel-init ()
  (setq inferior-lisp-program
        (expand-file-name "~/code/fennel/fennel --repl")))
(add-hook 'fennel-mode-hook 'matt-fennel-init)

(use-package ruby-mode
  :ensure t
  :mode (("\\.pp\\'" . ruby-mode)
         ("\\^Vagrantfile\\'" . ruby-mode)))

(use-package image
  :bind (:map image-map
              ("=" . image-increase-size)))

(use-package alarm
  :bind (:map matt-keymap
              ("a a" . alarm)
              ("a l" . alarm-list)
              ("a n" . alarm-next)))

(defun matt-font-lock-comment-annotations ()
  "Highlight well known comment annotations."
  (font-lock-add-keywords
   nil '((" \\<\\(\\(FIX\\|TODO\\|NOTE\\|HACK\\|XXX\\|\\?\\?\\?\\)\\)[: ]"
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
(global-set-key (kbd "S-<right>") 'next-buffer)
(global-set-key (kbd "S-<left>") 'previous-buffer)
(global-set-key (kbd "S-<up>") 'matt-mru-buffer)

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
  (message "Adjust window size.")
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
    (set-transient-map map t)))
(matt-define-key "w a" 'matt-window-adjust)

(defun matt-toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(defun matt-just-one-space-multiline ()
  (interactive)
  (just-one-space -1))
(global-set-key (kbd "M-SPC") 'matt-just-one-space-multiline)
(matt-define-key "j j" 'matt-just-one-space-multiline)

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
  (destructuring-bind
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

(defun matt-log ()
  (interactive)
  (find-file "~/Documents/log.org"))
(matt-define-key "o l" 'matt-log)

(defun matt-runs ()
  (interactive)
  (find-file "~/notes/runs.org"))
(matt-define-key "o r" 'matt-runs)

;; (format-time-string
;;  (apply 'concat
;;         (-interpose " %"
;;                     '("" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
;;                          "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
;;                     ))
;;   (current-time))
;; " Wednesday February 20 02/07/18 %E 2018-02-07 2018 15 03 %J %K %L 28 312138752 %O pm %Q 15:28 35 15:28:35 05 06 06 15:28:35 2018 GMT Wed Feb Wed 07 Feb 2018 15:28:35 GMT 07  7 %f 18 Feb %i 038 15  3 02
;;  %o pm 1  3:28:35 pm GMT 1518017315 	 3 %v 3 07/02/18 18 +0000"
(defun matt-insert-date (arg)
  "Insert the current date. e.g 2018-02-08, 2018 08-Feb-2018 or Thu, 08 February."
  (interactive "p")
  (insert (format-time-string
           (cond
            ((eq arg 16) "%a, %d %B %Y") ;; Thu, 08 February 2018
            ((eq arg 4)  "%d-%b-%Y")     ;; 08-Feb-2018
            (t           "%Y-%m-%d"))    ;; 2018-02-08
           (current-time)))) ;;
(matt-define-key "i d" 'matt-insert-date)

(defun matt-insert-time (arg)
  "Insert the current time. e.g 16:34:42, 2018-02-08T16:34:42+0000 or 2018-02-08_16-34-42."
  (interactive "p")
  (insert (format-time-string
           (cond
            ((eq arg 64) "%Y-%m-%d_%H-%M-%S") ;; 2018-02-08_16-34-42
            ((eq arg 16) "%FT%T%z")           ;; 2018-02-08T16:34:42+0000
            ((eq arg 4)  "%H:%M")             ;; 16:34
            (t           "%T"))               ;; 16:34:42
           (current-time))))
(matt-define-key "i t" 'matt-insert-time)

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
  (find-file "~/.emacs.d/init.el"))
(matt-define-key "o i" 'matt-open-init)

(defun matt-open-theme ()
  (interactive)
  (if-let ((theme (car custom-enabled-themes)))
      (find-file (format "~/.emacs.d/themes/%s-theme.el" theme))
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

(defun matt-jump-to-scratch-buffer ()
  "Switch to the scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))
(matt-define-key "o s" 'matt-jump-to-scratch-buffer)

(defun matt-jump-to-messages-buffer ()
  "Switch to the messages buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))
(matt-define-key "o m" 'matt-jump-to-messages-buffer)

(defun matt-delete-file-and-buffer ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "This buffer is not associated with a file.")
      (if (y-or-n-p (format "Are you sure you want to delete %s?" filename))
          (progn (delete-file filename)
                 (kill-buffer (current-buffer))
                 (message "%s deleted." filename))
        (message "%s not deleted." filename)))))
(matt-define-key "k" 'matt-delete-file-and-buffer)

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

(require 'json)
(defun matt-whats-my-ip ()
  (let ((json (json-read-from-string (matt-wget "http://ip.jsontest.com/"))))
    (cdr (assoc 'ip json))))

(defun matt-insert-my-ip ()
  (interactive)
  (insert (matt-whats-my-ip)))
(matt-define-key "i C-i" 'matt-insert-my-ip)

(defun matt-change-at-point (f)
  (skip-chars-backward "0-9")
  (if (looking-at "[0-9]+")
      (let ((raw (match-string 0)))
        (replace-match (number-to-string (funcall f (string-to-number raw)))))
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

(defun matt-kill-emacs ()
  (interactive)
  (if (y-or-n-p "Kill Emacs?")
      (save-buffers-kill-emacs)))
(matt-define-key "q" 'matt-kill-emacs)

(defun matt-chmod+x ()
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (chmod filename
           (file-modes-symbolic-to-number "+x" (file-modes filename)))))

(defun matt-clear-text-properties (begin end)
  (interactive "r")
  (set-text-properties begin end nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keys

(global-set-key (kbd "<M-backspace>") 'backward-kill-word)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)

(global-set-key (kbd "M-DEL")         'kill-word)
(global-set-key (kbd "<M-delete>")    'kill-word)

(matt-define-key "b b"                'bury-buffer)
(matt-define-key "a r"                'align-regexp)
(matt-define-key "s r"                'replace-string)
(matt-define-key "w b"                'balance-windows)
(matt-define-key "w l"                'toggle-truncate-lines) ;; mnemonic "wrap lines"
(matt-define-key "w w"                'toggle-word-wrap)

(define-key isearch-mode-map (kbd "C-.") 'isearch-forward-symbol-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init

(when (or (display-graphic-p)
          (daemonp))
  (matt-load-dark-theme))

;; require an init-local if present
(require 'init-local nil t)

(matt-gc-uninhibit)

(setq matt-init-stop-time (current-time))

(message "init time: %.2fms"
         (* 1000.0 (float-time (time-subtract matt-init-stop-time matt-init-start-time))))
