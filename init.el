;;; init --- Matt Lee's init file

;;; Commentary:
;; Lots of tweaks to my Emacs.

;;; Code:

(prelude-require-packages
 '(auto-complete
   yasnippet
   js2-mode
   js2-refactor
   skewer-mode
   ac-js2
   sbt-mode
   ))

(disable-theme 'zenburn)

(setq custom-theme-load-path
      (cons "~/.emacs.d/personal/themes/" custom-theme-load-path))
(load-theme 'witness t)

(setq prelude-guru nil)
(setq prelude-whitespace nil)
(setq prelude-flyspell nil)

;; font
(set-face-attribute 'default nil :height 110)
(set-face-attribute 'default nil :family "Liberation Mono")
;; (set-face-attribute 'default nil :family "DejaVu Sans Mono")

;; cursor - bar instead of a block
(set-default 'cursor-type '(bar . 2))

;; window title - include file's full path
(setq frame-title-format
      '("" invocation-name " - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; kill whole line
(setq kill-whole-line t)

;; middle click paste at point (not mouse cursor)
(setq mouse-yank-at-point t)

;; turn off newline on save
(setq require-final-newline nil)

;; window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; enable stuff
;(put 'upcase-region 'disabled nil)
;(put 'narrow-to-region 'disabled nil)
;(put 'narrow-to-page 'disabled nil)

;; smartparens
(require 'smartparens)
(setq sp-highlight-pair-overlay nil)

;; rainbow delimiters
;; colors
(require 'rainbow-delimiters)
(setq rainbow-delimiters-max-face-count 7)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#800")
(set-face-foreground 'rainbow-delimiters-depth-2-face "#070")
(set-face-foreground 'rainbow-delimiters-depth-3-face "#23b")
(set-face-foreground 'rainbow-delimiters-depth-4-face "#870")
(set-face-foreground 'rainbow-delimiters-depth-5-face "#808")
(set-face-foreground 'rainbow-delimiters-depth-6-face "#068")
(set-face-foreground 'rainbow-delimiters-depth-7-face "#840")

;; vcs - disable on windows as too slow
;(when (eq system-type 'windows-nt)
;  (setq vc-handled-backends ())
;  (global-git-gutter-mode 0))

;; nrepl tweaks
;; (setq nrepl-popup-stacktraces-in-repl t)
;; default: (setq nrepl-popup-stacktraces-in-repl nil)

;; auto complete
(require 'auto-complete-config)
(ac-config-default)

;; linum
(add-hook 'prog-mode-hook 'linum-mode)

;; yas
(require 'yasnippet)
(add-hook 'prog-mode-hook
          '(lambda () (yas-minor-mode)))
(setq yas-snippet-dirs '("~/.emacs.d/personal/snippets" "~/.emacs.d/elpa/yasnippet-20140106.1009/snippets"))
;(setq yas-snippet-dirs '("~/.emacs.d/personal/snippets"))
(setq yas-snippet-dirs (cons "~/.emacs.d/personal/snippets" (cdr yas-snippet-dirs)))
;; HACK - relies on the second element of yas-snippets-dir being the snippets from melpa
;; NOTE - yas fails to load if a dir that doesn't exist is in yas-snippets-dir
;;        ~/.emacs.d/snippets is in there by default, and doesn't exist
(yas-reload-all)

;; speedbar
(require 'speedbar)
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)
(add-to-list 'speedbar-frame-parameters '(width . 30))
(defun speedbar-kill ()
  (interactive)
  (if (get-buffer " SPEEDBAR")
      (kill-buffer " SPEEDBAR")))

;; magit
;(setq magit-diff-options '("--ignore-space-change"))
;(setq magit-status-buffer-switch-function 'switch-to-buffer)
;(setq magit-server-window-for-commit nil)

;; org
(setq org-todo-keywords '((type "TODO" "DOING" "DONE" "|" "POSTPONED")))
(setq org-todo-keyword-faces
      '(("TODO" :foreground "#d55" :weight bold)
        ("DONE" :foreground "#5d5" :weight bold)
        ("DOING" :foreground "#ed5" :weight bold)
        ("POSTPONED" :foreground "#aa9" :weight bold)))
(setq org-startup-folded nil)
(setq org-log-done nil)
(setq org-export-html-preamble nil)
(setq org-export-html-postamble nil)

;; hl line
;(set-face-background 'hl-line "#323")
;(set-face-attribute 'hl-line nil :underline nil)

;; whitespace mode
(setq  whitespace-style '(face tabs empty trailing))

;; js2
(require 'js2-mode)
(set-face-foreground 'js2-function-param "#0c0")
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-global-externs '("$" "angular" "_" "Highcharts" "google" "d3"))
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-m")

;; skewer mode
(add-hook 'js2-mode-hook 'skewer-mode)
(setq httpd-port 8081) ; default is 8080

;; auto complete for js2
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq ac-js2-evaluate-calls t)

;; css
(setq css-indent-offset 4)

;; scala
(setq sbt:program-name "~/opt/sbt/sbt")
(add-hook 'sbt-mode-hook '(lambda ()
  (setq compilation-skip-threshold 1)
  (local-set-key (kbd "C-a") 'comint-bol)
  (local-set-key (kbd "M-RET") 'comint-accumulate)))
(add-hook 'scala-mode-hook '(lambda ()
   (local-set-key (kbd "M-.") 'sbt-find-definitions)
   (local-set-key (kbd "C-x '") 'sbt-run-previous-command)
   (local-set-key (kbd "C-M-x") 'sbt-send-region)))

;; auto mode
(add-to-list 'auto-mode-alist '("\\.\\(jspf?\\|tag\\)$" . html-mode))

;; window & buffer switching
(defun matt-normal-buffer-switch (f)
  "Switch buffers with `f` until the `buffer-name` doesn't start with a \"*\"."
  (funcall f)
  (if (equal "*" (substring (buffer-name) 0 1))
      (progn
        (message "Skipped buffer %s" (buffer-name))
        (matt-normal-buffer-switch f))))
(define-key global-map (kbd "S-<menu>")
  (lambda () (interactive) (matt-normal-buffer-switch 'previous-buffer)))
(define-key global-map (kbd "<menu>")
  (lambda () (interactive) (matt-normal-buffer-switch 'next-buffer)))
(define-key global-map (kbd "C-<menu>") 'other-window)

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

;; journal
(defun matt-journal ()
  (interactive)
  (find-file-other-frame "~/work/journal.org"))

(defun matt-journal-title ()
  (interactive)
  (insert "* --------------------------------------------------------------------------------\n")
  (shell-command "date +\"%a %b %d %Y\"" t))

(defun matt-font-size (sz)
  (interactive "nSize: ")
  (set-face-attribute 'default nil :height (* sz 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYS

;; kill buffer - without confirmation
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "M-SPC") 'matt-just-one-space-multiline)

(global-set-key (kbd "<M-backspace>") 'backward-kill-word)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)

(global-set-key (kbd "M-DEL") 'kill-word)
(global-set-key (kbd "<M-delete>") 'kill-word)
