;;; init --- Matt Lee's init file

;;; Commentary:
;; Lots of tweaks to my Emacs.

;;; Code:

(require 'cl)
(require 'package)

(add-to-list 'load-path (expand-file-name "." user-emacs-directory))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(defvar matt-packages
  '(
    ;;anzu
    browse-kill-ring
    company
    dash
    diff-hl
    elisp-slime-nav
    geiser
    gitconfig-mode
    gitignore-mode
    projectile
    magit
    move-text
    rainbow-delimiters
    rainbow-mode
    smartparens
    smex
    undo-tree
    yasnippet
    clojure-mode
    cider
    js2-mode
    js2-refactor
    lua-mode
    sbt-mode
    scala-mode2
    ensime
    ido-ubiquitous
    ido-vertical-mode
    flx-ido
    markdown-mode
    highlight-symbol
    multiple-cursors
    tuareg
    web-mode
    less-css-mode
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

;; hide tool bar & menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

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
(setq custom-safe-themes '(default))
(setq custom-safe-themes '("de8f4162b577e16f9bfdee4789faa43f5b832f03bb0a8194fc2329de0cea9aa4"
                           "9eadc30c0a936fbb4db8272792e2fdc95ca42a29d38fd7ab05e684dd81848ef2"
                           default))
(require 'server)
(if (or (display-graphic-p)
        (daemonp))
    (load-theme 'witness t))

;; font
(set-face-attribute 'default nil :height 110)
;; (set-face-attribute 'default nil :family "Nimbus Mono L")
;; (set-face-attribute 'default nil :family "Liberation Mono")
;; (set-face-attribute 'default nil :family "DejaVu Sans Mono")
;; (set-face-attribute 'default nil :family "Droid Sans Mono")
(set-face-attribute 'default nil :family "Inconsolata")
;; (set-face-attribute 'default nil :family "Monofur")
;; (set-face-attribute 'default nil :family "Ubuntu Mono")
;; (set-face-attribute 'default nil :family "Andale Mono")

;; cursor - bar instead of a block
(set-default 'cursor-type '(bar . 2))

;; window title - include file's full path
(setq frame-title-format
      '("" invocation-name " - "
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
(global-auto-revert-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t) ;; delete the selection with a keypress
(savehist-mode)

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

(require 'smex)
;; (setq smex-save-file (expand-file-name ".smex-items" prelude-savefile-dir))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'ido)
(require 'ido-vertical-mode)
(require 'ido-ubiquitous)
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
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)

(require 'ibuffer)
(setq ibuffer-default-sorting-mode 'alphabetic)
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

(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never) ;; disable - can cause problems with remote files
(recentf-mode 1)
(defun matt-recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: "
                                   (-map 'abbreviate-file-name recentf-list)
                                   nil t)))
    (when file
      (find-file file))))
(global-set-key (kbd "C-x f") 'matt-recentf-ido-find-file)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(require 'undo-tree)
(global-undo-tree-mode)

(require 'smartparens)
(require 'smartparens-config)
(setq sp-highlight-pair-overlay nil)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(show-smartparens-global-mode 1)
(add-hook 'prog-mode-hook 'smartparens-mode)

(require 'company)
(setq company-idle-delay 0.1)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-flip-when-above t)
(global-company-mode 1)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case 't)

(require 'linum)
(setq linum-format "%4i ")
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'css-mode-hook 'linum-mode)
(add-hook 'html-mode-hook 'linum-mode)
(add-hook 'nxml-mode-hook 'linum-mode)

(require 'yasnippet)
(yas-global-mode)
;;(setq yas-snippet-dirs '("~/.emacs.d/snippets" "~/.emacs.d/elpa/yasnippet-20140106.1009/snippets"))
;;(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(setq yas-snippet-dirs (cons "~/.emacs.d/snippets" (cdr yas-snippet-dirs)))
;; HACK - relies on the second element of yas-snippets-dir being the snippets from melpa
;; NOTE - yas fails to load if a dir that doesn't exist is in yas-snippets-dir
;;        ~/.emacs.d/snippets is in there by default, and doesn't exist
(yas-reload-all)

(require 'highlight-symbol)
(setq highlight-symbol-colors '("#f9b" "#fb7" "#fe8" "#ce9" "#9fb" "#9dd" "#9bf" "#b9f"))
(matt-define-key "C-," 'highlight-symbol-at-point)
(matt-define-key "C-n" 'highlight-symbol-next)
(matt-define-key "C-p" 'highlight-symbol-prev)

(require 'projectile)
(projectile-global-mode)

(require 'speedbar)
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)
(add-to-list 'speedbar-frame-parameters '(width . 30))
(defun speedbar-kill ()
  (interactive)
  (if (get-buffer " SPEEDBAR")
      (kill-buffer " SPEEDBAR")))

(require 'multiple-cursors)
(matt-define-key "m c" 'mc/mark-more-like-this-extended)

(require 'move-text)
(global-set-key (kbd "C-S-<up>") 'move-text-up)
(global-set-key (kbd "C-S-<down>") 'move-text-down)

(require 'magit)
(setq magit-diff-options '()) ;; '("--ignore-all-space" "--ignore-blank-lines"))
(setq magit-status-buffer-switch-function 'switch-to-buffer)
;;(setq magit-server-window-for-commit nil)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'diff-hl)
(global-diff-hl-mode 1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(global-set-key (kbd "C-c g") diff-hl-command-map)

(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ;; don't start another frame
(setq ediff-split-window-function 'split-window-horizontally)

(defface org-todo-face '((t (:foreground "#f00"))) "org mode face for TODO items")
(defface org-done-face '((t (:foreground "#0f0"))) "org mode face for DONE items")
(defface org-doing-face '((t (:foreground "#ff0"))) "org mode face for DOING items")
(defface org-postponed-face '((t (:foreground "#666"))) "org mode face for POSTPONED items")

(require 'org)
(setq org-todo-keywords '((type "TODO" "DOING" "DONE" "|" "POSTPONED")))
(setq org-todo-keyword-faces
      '(("TODO" 'org-todo-face)
        ("DONE" 'org-done-face)
        ("DOING" org-doing-face)
        ("POSTPONED" 'org-postponed-face)))
(setq org-startup-folded nil)
(setq org-log-done nil)
(setq org-export-html-preamble nil)
(setq org-export-html-postamble nil)
(define-key org-mode-map (kbd "S-<up>") nil)
(define-key org-mode-map (kbd "S-<down>") nil)
(define-key org-mode-map (kbd "S-<left>") nil)
(define-key org-mode-map (kbd "S-<right>") nil)
(define-key org-mode-map (kbd "M-<left>") nil)
(define-key org-mode-map (kbd "M-<right>") nil)
(define-key org-mode-map (kbd "C-<left>") nil)
(define-key org-mode-map (kbd "C-<right>") nil)
(define-key org-mode-map (kbd "C-,") nil)

(require 'markdown-mode)
(define-key markdown-mode-map (kbd "M-<left>") nil)
(define-key markdown-mode-map (kbd "M-<right>") nil)

(defun matt-font-lock-comment-annotations ()
  "Highlight well known comment annotations."
  (font-lock-add-keywords
   nil '((" \\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\)\\)[: ]"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'matt-font-lock-comment-annotations)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-basic-offset 2)
(setq js2-global-externs
      '("setTimeout" "setInterval" "require" "module" "exports" "$" "angular" "_"
        "Highcharts" "google" "d3" "chai" "it" "describe" "suite" "test"))
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-m")
(define-key js2-mode-map (kbd "<C-S-down>") nil)
(define-key js2-mode-map (kbd "<C-S-up>") nil)

(require 'css-mode)
(setq css-indent-offset 4)
(add-hook 'css-mode-hook 'rainbow-mode)

(require 'scala-mode2)
;;(require 'ensime)
;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;;(setq sbt:program-name "~/opt/sbt/sbt")
;;(add-hook 'sbt-mode-hook '(lambda ()
;;  (setq compilation-skip-threshold 1)
;;  (local-set-key (kbd "C-a") 'comint-bol)))
;;(add-hook 'scala-mode-hook '(lambda ()
;;   (local-set-key (kbd "M-.") 'sbt-find-definitions)
;;   (local-set-key (kbd "C-x '") 'sbt-run-previous-command)
;;   (local-set-key (kbd "C-M-x") 'sbt-send-region)))

(require 'geiser)
(setq geiser-active-implementations '(chicken))
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'geiser-repl-mode-hook 'smartparens-mode)

(require 'tuareg)

(require 'sgml-mode)
(add-to-list 'auto-mode-alist '("\\.\\(jspf?\\|tag\\)$" . html-mode))

(require 'web-mode)
(setq web-mode-markup-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(require 'less-css-mode)

(require 'alarm)
(matt-define-key "a a" 'alarm)
(matt-define-key "a l" 'alarm-list)
(matt-define-key "a n" 'alarm-next)

(defun matt-kill-this-buffer ()
  "Kill buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

;; window & buffer switching
(defun matt-buffer-swith-ignore-p (bn)
  "Ignore special buffers except *scratch*."
  (and (not (equal "*scratch*" bn))
       (equal "*" (substring bn 0 1))))
(defun matt-normal-buffer-switch (f)
  "Switch buffers with `f` ignoring those when `matt-buffer-swith-ignore-p' is true"
  (funcall f)
  (if (matt-buffer-swith-ignore-p (buffer-name))
      (progn
        (message "Skipped buffer %s" (buffer-name))
        (matt-normal-buffer-switch f))))
(define-key global-map (kbd "S-<menu>")
  (lambda () (interactive) (matt-normal-buffer-switch 'previous-buffer)))
(define-key global-map (kbd "<menu>")
  (lambda () (interactive) (matt-normal-buffer-switch 'next-buffer)))
(define-key global-map (kbd "C-<menu>") 'other-window)

(defun matt-mru-buffer ()
  "Switch to the most recently used buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun matt-swap-windows ()
  "Swap the buffers in `selected-window' and `next-window'."
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

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

(defun matt-speedbar-and-arrange-windows ()
  (interactive)
  (let ((top 29) (height 56) (width 160))
    (set-frame-position (selected-frame) 299 top)
    (set-frame-size (selected-frame) width height)
    (speedbar)
    (set-frame-position (car (frame-list)) 0 top)))

(defun matt-journal ()
  (interactive)
  (find-file "~/work/journal.org"))

(defun matt-journal-title ()
  (interactive)
  (let* ((date (format-time-string "%a, %b %d %Y" (current-time))))
    (insert "* " date " ----------------------------------------------------------------\n")))

(defun matt-font-size (sz)
  (interactive "NSize: ")
  (set-face-attribute 'default nil :height (* sz 10)))

(defun matt-font-size-small ()
  (interactive)
  (matt-font-size 11))
(matt-define-key "f s" 'matt-font-size-small)

(defun matt-font-size-large ()
  (interactive)
  (matt-font-size 20))
(matt-define-key "f l" 'matt-font-size-large)

(defun matt-insert-date ()
  "Insert the current date. e.g 16-Jun-2014"
  (interactive)
  (insert (format-time-string "%d-%b-%Y" (current-time))))
(matt-define-key "i d" 'matt-insert-date)

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

(defun matt-open-temp-file ()
  (interactive)
  (let* ((timestamp (format-time-string "%Y-%m-%d_%H-%M-%S" (current-time)))
         (filename  (format "~/tmp/emacs-temp-%s" timestamp)))
    (find-file filename)))

(defun matt-window-adjust ()
  "Interactively adjust window size."
  (interactive)
  (message "Adjust window size.")
  (let ((map (make-sparse-keymap)))
    (define-key map '[?-] 'shrink-window)
    (define-key map '[?=] 'enlarge-window)
    (set-temporary-overlay-map map t)))

(defun matt-toggle-theme ()
  (interactive)
  (cond ((custom-theme-enabled-p 'footlamp) (matt-disable-all-themes) (load-theme 'witness t))
        ((custom-theme-enabled-p 'witness) (matt-disable-all-themes) (load-theme 'footlamp t))
        (t (message "Current theme unknown."))))
(matt-define-key "t t" 'matt-toggle-theme)

(defun matt-disable-current-theme ()
  (interactive)
  (disable-theme (car custom-enabled-themes)))

(defun matt-disable-all-themes ()
  (interactive)
  (if (eql nil custom-enabled-themes)
      (message "All themes disabled.")
    (progn (matt-disable-current-theme)
           (matt-disable-all-themes))))
(matt-define-key "t ESC" 'matt-disable-all-themes)

(defun matt-load-dark-theme ()
  (interactive)
  (matt-disable-all-themes)
  (load-theme 'witness t))
(matt-define-key "t d" 'matt-load-dark-theme)

(defun matt-load-light-theme ()
  (interactive)
  (matt-disable-current-theme)
  (load-theme 'footlamp t))
(matt-define-key "t l" 'matt-load-light-theme)

(defun matt-create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*scratch*")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    (insert initial-scratch-message)))

(defun matt-delete-file-and-buffer ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename) (message "This buffer is not associated with a file.")
      (if (y-or-n-p (format "Are you sure you want to delete %s?" filename))
          (progn (delete-file filename)
                 (kill-buffer (current-buffer))
                 (message "%s deleted." filename))
        (message "%s not deleted." filename)))))

(defun matt-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
(matt-define-key "<tab>" 'matt-indent-buffer)

(defun matt-insert-filename ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename) (message "This buffer is not associated with a file.")
      (insert (file-name-base filename)))))
(matt-define-key "i f" 'matt-insert-filename)

(defun matt-wget (url)
  (interactive "sURL: ")
  (with-current-buffer (url-retrieve-synchronously url)
    (let ((content (buffer-substring (marker-position url-http-end-of-headers) (buffer-size))))
      (kill-buffer)
      (let ((wget-buffer (get-buffer-create "*wget*")))
        (with-current-buffer wget-buffer
          (insert content)
          (view-buffer wget-buffer))))))

(require 'json)
(defun matt-whats-my-ip ()
  (with-current-buffer (url-retrieve-synchronously "http://ip.jsontest.com/")
    (goto-char url-http-end-of-headers)
    (let ((json (json-read)))
      (kill-buffer)
      (cdr (assoc 'ip json)))))

(defun matt-insert-my-ip ()
  (interactive)
  (insert (matt-whats-my-ip)))

(defun matt-tmux-yank ()
  (interactive)
  (insert (shell-command-to-string "tmux showb")))
(matt-define-key "t y" 'matt-tmux-yank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYS

(global-set-key (kbd "C-x k") 'matt-kill-this-buffer)

(global-set-key (kbd "M-SPC") 'matt-just-one-space-multiline)

(global-set-key (kbd "<M-backspace>") 'backward-kill-word)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)

(global-set-key (kbd "M-DEL") 'kill-word)
(global-set-key (kbd "<M-delete>") 'kill-word)

(global-set-key (kbd "S-<right>") 'next-buffer)
(global-set-key (kbd "S-<left>") 'previous-buffer)
(global-set-key (kbd "S-<up>") 'matt-mru-buffer)
(global-set-key (kbd "S-<down>") 'ido-switch-buffer)

(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-S-<iso-lefttab>") 'previous-buffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-o") 'other-window)

(matt-define-key "g" 'rgrep)
