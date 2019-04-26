;;; init --- Matt Lee's init file

;;; Commentary:
;; Lots of tweaks to my Emacs.

;;; Code:

(setq matt-init-start-time (current-time))

(setq gc-cons-threshold (* 512 1024 1024)) ;; speedup startup

(require 'cl)
(require 'package)

(package-initialize) ;; NOTE: required for first run, but issues warning thereafter

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "~/code/fennel-mode"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(defvar matt-packages
  '(browse-kill-ring
    company
    dash
    diff-hl
    elisp-slime-nav
    projectile
    magit
    move-text
    rainbow-delimiters
    rainbow-mode
    smartparens
    smex
    undo-tree
    yasnippet
    yasnippet-snippets
    ido-completing-read+
    ido-vertical-mode
    flx-ido
    ibuffer-vc
    idle-highlight-mode
    multiple-cursors
    js2-mode
    js2-refactor
    lua-mode
    markdown-mode
    web-mode
    less-css-mode
    clojure-mode
    cider
    inf-clojure
    monroe
    yaml-mode
    )
  "A list of packages to ensure are installed at launch.")

(defun matt-packages-installed-p ()
  "Check if all packages in `matt-packages' are installed."
  (every 'package-installed-p matt-packages))

(defun matt-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package matt-packages)
    (add-to-list 'matt-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun matt-require-packages (packages)
  "Ensure PACKAGES are installed. Missing packages are installed automatically."
  (mapc 'matt-require-package packages))

(defun matt-install-packages ()
  "Install all packages listed in `matt-packages'."
  (unless (matt-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Matt's Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (matt-require-packages matt-packages)))

(matt-install-packages)

(require 'dash)

(defun matt-packages-installed-implicitly ()
  "List packages that are installed (i.e. in `package-alist'),
   but not listed explicitly in `matt-packages'"
  (let ((pkgs (-map 'car package-alist)))
    (-filter (lambda (x) (not (-contains-p matt-packages x))) pkgs)))

(defun matt-packages-with-dependencies (packages)
  (-map (lambda (pkg)
          (let* ((reqs-ids (car (package-desc-reqs
                                 (cadr
                                  (assq pkg package-alist)))))
                 (reqs (-map (lambda (r) (memq r package-activated-list)) reqs-ids)))
            (cons pkg reqs)))
        packages))

;; hide tool bar & menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; scroll at same rate as the cursor - this stops the scroll "jumping" when you move off the top/bottom
(setq scroll-conservatively 1)

;; keys
(setq matt-keymap (make-sparse-keymap))
(global-set-key (kbd "C-,") matt-keymap)
(global-set-key (kbd "C-x ,") matt-keymap) ;; C-, doesn't work in terminal
(defun matt-define-key (key def)
  (define-key matt-keymap (kbd key) def))

;; themes
(setq custom-theme-load-path
      (cons "~/.emacs.d/themes/" custom-theme-load-path))
;; (secure-hash 'sha256 (current-buffer))
(setq custom-safe-themes '("3a3a9608bc30bab203cd2738d361b4d6f5074b10017d53c5caa26a59ee46e4bf" ;; witness
                           "4dc9517f2328de92e6a308c59bcbb2ee8d4fa987a30ed9ceeedc6240aa61918a" ;; footlamp
                           "c51a5ea6a72e533b262235ec0fb6bbf29d59f2c5f26b0f951b8cfd1ae90a63a7" ;; sandaldust
                           default))

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
     (ido-completing-read "Load custom theme: "
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
  (cond ((custom-theme-enabled-p 'footlamp)   (matt-load-theme 'witness))
        ((custom-theme-enabled-p 'witness)    (matt-load-theme 'sandaldust))
        ((custom-theme-enabled-p 'sandaldust) (matt-load-theme 'footlamp))
        (t (message "Current theme unknown."))))
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
        (:eval (if (not (equal server-name "server"))
                   (concat " - " server-name " ")))
        " - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
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
(global-set-key (kbd "C-d") 'backward-delete-char)
(setq set-mark-command-repeat-pop t)

;; window splitting - always horizontal
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; enable stuff
;; (put 'narrow-to-region 'disabled nil)
;; (put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(require 'dired)

(require 'browse-url)
(setq browse-url-browser-function 'browse-url-chrome)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'ido)
(require 'ido-vertical-mode)
(require 'ido-completing-read+)
(require 'flx-ido)
(setq ido-enable-prefix nil)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point 'guess)
(setq ido-max-prospects 10)
(setq ido-save-directory-list-file (expand-file-name "ido.hist" user-emacs-directory))
(setq ido-default-file-method 'selected-window)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-faces nil)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(setq ido-max-prospects 25)
(setq ido-max-window-height (+ ido-max-prospects 2))
(ido-mode 1)
(ido-vertical-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

(require 'ibuffer)
(require 'ibuffer-vc)
(setq ibuffer-default-sorting-mode 'filename/process) ;; groups file buffers together
(setq ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root)
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 52 52 :left :elide) " "
              filename-and-process)
        (mark modified read-only " "
              (name 25 25 :left :elide) " "
              (size 9 -1 :right) " "
              (mode 16 16 :left :elide) " "
              filename-and-process)))
(defun ibuffer-ido-find-file (file &optional wildcards)
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory
           (let ((buf (ibuffer-current-buffer)))
             (if (buffer-live-p buf)
                 (with-current-buffer buf
                   default-directory)
               default-directory))))
     (list (ido-read-file-name "Find file: " default-directory) t)))
  (find-file file wildcards))
(define-key ibuffer-mode-map "\C-x\C-f" 'ibuffer-ido-find-file)
(define-key ibuffer-name-map [(mouse-1)] 'ibuffer-mouse-visit-buffer)
(define-key ibuffer-name-map [(mouse-2)] 'ibuffer-mouse-toggle-mark)
(global-set-key (kbd "C-x C-b") 'ibuffer)

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
(defun matt-recentf-ido-find-file (file)
  "Find a recent file using ido."
  (interactive
   (list
    (ido-completing-read "Choose recent file: "
                         (-map 'abbreviate-file-name recentf-list)
                         nil t)))
  (find-file file))
(global-set-key (kbd "C-x f") 'matt-recentf-ido-find-file)

(require 'bookmark)
(matt-define-key "b m" 'bookmark-set)
(matt-define-key "b j" 'bookmark-jump)
(matt-define-key "b l" 'bookmark-bmenu-list)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ;; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ;; don't muck with special buffers

(require 'undo-tree)
(global-undo-tree-mode)

(require 'smartparens)
(require 'smartparens-config)
(setq sp-highlight-pair-overlay nil)
(sp-use-smartparens-bindings)
(show-smartparens-global-mode 1)
(add-hook 'prog-mode-hook 'smartparens-mode)
(add-hook 'inferior-lisp-mode-hook 'smartparens-mode)

(require 'company)
(setq company-idle-delay 0.1)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-flip-when-above t)
(global-company-mode 1)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case 't)
(define-key company-active-map (kbd "<tab>") nil) ;; conflicts with yas

(require 'linum)
(setq linum-format "%4i ")
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'css-mode-hook 'linum-mode)
(add-hook 'html-mode-hook 'linum-mode)
(add-hook 'nxml-mode-hook 'linum-mode)
(add-hook 'conf-mode-hook 'linum-mode)

(require 'rainbow-mode)
(setq-default rainbow-html-colors nil)
(setq-default rainbow-x-colors nil)

(require 'grep)
(setq grep-find-ignored-directories (append grep-find-ignored-directories '("target" "out" "node_modules" "build" "dist" "bower")))
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

(require 'flyspell)
(define-key flyspell-mode-map (kbd "C-,") nil)

(require 'yasnippet)
(yas-global-mode 1)

(require 'idle-highlight-mode)
(add-hook 'prog-mode-hook 'idle-highlight-mode)

(require 'projectile)
(projectile-global-mode)
(global-set-key (kbd "C-x p") 'projectile-find-file)
(setq projectile-svn-command "find . -type f -not -iwholename '*.svn/*' -print0") ;; see https://github.com/bbatsov/projectile/issues/520

(require 'multiple-cursors)
(matt-define-key "m m" 'mc/mark-more-like-this-extended)
(matt-define-key "m r" 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/unmark-next-like-this)

(require 'move-text)
(global-set-key (kbd "C-S-<up>") 'move-text-up)
(global-set-key (kbd "C-S-<down>") 'move-text-down)

(require 'magit)
(setq magit-status-buffer-switch-function 'switch-to-buffer)
(add-to-list 'magit-repository-directories '("~/work/" . 1))
(add-to-list 'magit-repository-directories '("~/code/" . 1))
(defun matt-magit (&optional git-dir)
  (interactive
   (list
    (when (or current-prefix-arg (not (magit-toplevel)))
      (ido-completing-read "Git Repo: " (magit-list-repos)))))
  (magit git-dir))
(define-key magit-file-mode-map (kbd "C-x g") nil)
(global-set-key (kbd "C-x g") 'matt-magit)
(setf (nth 1 magit-log-margin) "%a %d %b %R")
;; (setf (nth 1 magit-log-margin) 'age) ;; the default
;; (setf (nth 1 magit-log-margin) 'age-abbreviated) ;; an alternative

(require 'diff-hl)
(global-diff-hl-mode 1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(global-set-key (kbd "C-c g") diff-hl-command-map)

(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ;; don't start another frame
(setq ediff-split-window-function 'split-window-horizontally)

(require 'calendar)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(matt-define-key "o c" 'calendar)

(require 'org)
(require 'org-clock)
(defface org-todo-face '((t (:foreground "#f00"))) "org mode face for TODO items")
(defface org-done-face '((t (:foreground "#0f0"))) "org mode face for DONE items")
(defface org-doing-face '((t (:foreground "#ff0"))) "org mode face for DOING items")
(defface org-postponed-face '((t (:foreground "#666"))) "org mode face for POSTPONED items")
(defface org-query-face '((t (:foreground "#f0f"))) "org mode face for ??? (query) items")
(setq org-todo-keywords '((sequence "TODO" "DOING" "DONE") (sequence "POSTPONED") (sequence "???" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" 'org-todo-face)
        ("DONE" 'org-done-face)
        ("DOING" org-doing-face)
        ("POSTPONED" 'org-postponed-face)
        ("???" 'org-query-face)))
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
    (goto-char (line-end-position))
    (insert "--")
    (matt-org-insert-timestamp)))
(matt-define-key "c i" 'matt-org-clock-in)
(matt-define-key "c o" 'matt-org-clock-out)
(matt-define-key "c r" 'org-clock-report)
(matt-define-key "l l" 'org-store-link)
(matt-define-key "l t" 'org-toggle-link-display)

(require 'markdown-mode)
(define-key markdown-mode-map (kbd "M-<left>") nil)
(define-key markdown-mode-map (kbd "M-<right>") nil)

(require 'lisp-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

(require 'js)
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.cfn\\'" . js-mode)) ;; cloud formation

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js-indent-level 2)
(setq-default js2-basic-offset 2)
(setq-default js2-strict-trailing-comma-warning nil)
(setq js2-global-externs
      '("setTimeout" "setInterval" "clearTimeout" "clearInterval" "global" "require" "module" "exports" "WebSocket" "$" "angular" "_"
        "Highcharts" "google" "d3" "chai" "it" "describe" "suite" "test" "chrome" "config"))
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-m")
(define-key js2-mode-map (kbd "<C-S-down>") nil)
(define-key js2-mode-map (kbd "<C-S-up>") nil)

(require 'css-mode)
(setq css-indent-offset 2)
(add-hook 'css-mode-hook 'rainbow-mode)

(require 'cc-mode)
(add-hook 'java-mode-hook '(lambda () (c-set-offset 'arglist-intro '+)))

(require 'web-mode)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-enable-comment-interpolation nil)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

(require 'less-css-mode)

(require 'clojure-mode)
(require 'cider)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(setq cider-repl-history-size 1000)
(setq cider-repl-history-file (expand-file-name ".cider-repl-history" user-emacs-directory))
(define-key cider-mode-map (kbd "C-c ;") 'cider-pprint-eval-last-sexp-to-comment)
;; hook figwheel in to cider
(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")
;; special config for clojure & openscad
(defvar matt-openscad-file "designs/temp.scad")
(defun matt-cider-eval-to-openscad ()
  "Write the last sexp to an scad file."
  (interactive)
  (let* ((bounds (cider-last-sexp 'bounds))
         (scad (apply #'buffer-substring bounds))
         (form (concat "(spit \"" matt-openscad-file "\" (write-scad " scad "))")))
    (cider-interactive-eval form nil bounds)))
(matt-define-key "e" 'matt-cider-eval-to-openscad)
(define-clojure-indent (fact 1)) ;; midje

(require 'inf-clojure)
(add-hook 'inf-clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'inf-clojure-mode-hook 'smartparens-mode)
(add-hook 'inf-clojure-mode-hook 'rainbow-delimiters-mode)

(require 'scheme)
(add-to-list 'auto-mode-alist '("\\.ms\\'" . scheme-mode)) ;; microscheme

(require 'fennel-mode)
(defun matt-fennel-init ()
  (setq inferior-lisp-program
        (expand-file-name "~/code/fennel/fennel --repl")))
(add-hook 'fennel-mode-hook 'rainbow-delimiters-mode)
(add-hook 'fennel-mode-hook 'matt-fennel-init)

(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("\\.pp\\'" . ruby-mode)) ;; puppet
(add-to-list 'auto-mode-alist '("\\^Vagrantfile\\'" . ruby-mode)) ;; vagrant
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) ;; ruby erb files

(require 'image)
(define-key image-map "=" 'image-increase-size)

(require 'alarm)
(matt-define-key "a a" 'alarm)
(matt-define-key "a l" 'alarm-list)
(matt-define-key "a n" 'alarm-next)

(defun matt-font-lock-comment-annotations ()
  "Highlight well known comment annotations."
  (font-lock-add-keywords
   nil '((" \\<\\(\\(FIX\\|TODO\\|NOTE\\|HACK\\|XXX\\|\\?\\?\\?\\)\\)[: ]"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'matt-font-lock-comment-annotations)
(add-hook 'web-mode-hook 'matt-font-lock-comment-annotations)
(add-hook 'less-css-mode-hook 'matt-font-lock-comment-annotations)
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
(global-set-key (kbd "S-<down>") 'ido-switch-buffer)

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

(defun matt-journal ()
  (interactive)
  (find-file "~/Documents/journal.org"))
(matt-define-key "o j" 'matt-journal)

(defun matt-log ()
  (interactive)
  (find-file "~/Documents/log.org"))
(matt-define-key "o l" 'matt-log)

(defun matt-journal-title ()
  (interactive)
  (let* ((date (format-time-string "%a, %b %d %Y" (current-time))))
    (insert "** " date " ----------------------------------------------------------------\n"
            "*** 7 hrs : 09:00-12:00 13:00-17:00 : 0 hrs up")))
(matt-define-key "j t" 'matt-journal-title)

(defun matt-journal-hours-total ()
  (interactive)
  (matt-journal)
  (let* ((content (split-string (buffer-string) "\n" t))
         (time-lines (--filter (string-match "^\\*+ .* : .* : " it) content))
         (overtimes (--map (-> it
                               (split-string " : ")
                               (caddr)
                               ((lambda (x)
                                  (* (string-to-number x)
                                     (if (string-match "down$" x) -1 1)))))
                           time-lines))
         (total (apply '+ overtimes)))
    (message (format "%.1f" total))))
(matt-define-key "j h t" 'matt-journal-hours-total)

(defun matt-journal-hours-day ()
  (interactive)
  (let* ((line (thing-at-point 'line t)) ;; e.g. "** 7.5 hrs : 08:30-15:00 19:00-20:00 : 0.5 hr down"
         (times (cadr (split-string line " : " t)))
         (periods (->> (split-string times " " t)
                       (--map (->> (split-string it "-" t)
                                   (-map (lambda (x) (-map 'string-to-number (split-string x ":" t))))))))
         (hours (--map (destructuring-bind ((from-h from-m) (to-h to-m)) it
                         (+ (- to-h from-h) (/ (- to-m from-m) 60.0)))
                       periods)))
    (message (format "%.1f" (apply '+ hours)))))
(matt-define-key "j h d" 'matt-journal-hours-day)

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
  (previous-line)
  (let ((b (point))
        (e (point-at-eol)))
    (next-line)
    (insert (make-string (- e b) c))))
(matt-define-key "i u" 'matt-insert-underline)

(defun matt-open-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(matt-define-key "o i" 'matt-open-init)

(defun matt-open-theme ()
  (interactive)
  (let ((theme (car custom-enabled-themes)))
    (find-file (format "~/.emacs.d/themes/%s-theme.el" theme))))
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
(matt-define-key "o p" 'matt-open-temp-file)

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

(defun matt-window-adjust ()
  "Interactively adjust window size."
  (interactive)
  (message "Adjust window size.")
  (let ((map (make-sparse-keymap)))
    (define-key map '[?-] 'shrink-window)
    (define-key map '[?=] 'enlarge-window)
    (set-temporary-overlay-map map t)))
(matt-define-key "w a" 'matt-window-adjust)

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
  (or (looking-at "[0-9]+")
      (error "Nothing matched at point"))
  (let ((raw (match-string 0)))
    (replace-match (number-to-string (funcall f (string-to-number raw))))))

(defun matt-increment-at-point ()
  (interactive)
  (matt-change-at-point '1+))

(defun matt-decrement-at-point ()
  (interactive)
  (matt-change-at-point '1-))

(defun matt-tmux-yank ()
  (interactive)
  (insert (shell-command-to-string "tmux showb")))
(matt-define-key "t y" 'matt-tmux-yank)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keys

(global-set-key (kbd "<M-backspace>")     'backward-kill-word)
(global-set-key (kbd "<C-backspace>")     'backward-kill-word)

(global-set-key (kbd "M-DEL")             'kill-word)
(global-set-key (kbd "<M-delete>")        'kill-word)

(global-set-key (kbd "M-[")               'scroll-up-line)
(global-set-key (kbd "M-]")               'scroll-down-line)

(matt-define-key "b b"                    'bury-buffer)
(matt-define-key "g g"                    'rgrep)
(matt-define-key "g p"                    'projectile-grep)
(matt-define-key "a r"                    'align-regexp)
(matt-define-key "s r"                    'replace-string)
(matt-define-key "w b"                    'balance-windows)
(matt-define-key "w l"                    'toggle-truncate-lines) ;; mnemonic "wrap lines"
(matt-define-key "w w"                    'toggle-word-wrap)
(matt-define-key "l n"                    'linum-mode)

(define-key isearch-mode-map (kbd "C-.") 'isearch-forward-symbol-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init

(require 'server)
(if (or (display-graphic-p)
        (daemonp))
    (matt-load-theme 'witness))

;; require an init-local if present
(require 'init-local nil t)

(setq matt-init-stop-time (current-time))

(message "init time: %.2fms"
         (* 1000.0 (float-time (time-subtract matt-init-stop-time matt-init-start-time))))
