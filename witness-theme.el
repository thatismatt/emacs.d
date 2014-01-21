;;; witness-theme.el --- A dark theme for Emacs.
;; Copyright (C) 2014 Matt Lee
;; Author: Matt Lee
;; URL: http://github.com/thatismatt
;; License: MIT

;;; Commentary:
;; A dark theme.

;;; Code:

(unless (>= 24 emacs-major-version)
  (error "Requires Emacs 24 or later"))

(deftheme witness "A dark color theme for Emacs based on spolsky from sublime-themes")

(custom-theme-set-variables
  'witness
  '(linum-format "%4i "))

(let* ((*background*         "#011")
       (*normal*             "#AAA")
       (*current-line*       "#111")
       (*cursor-underscore*  "#999")
       (*visual-selection*   "#135")
       (*visual-selection-1* "#012")
       (*visual-selection-2* "#123")

       (*line-number*        "#333")
       (*line-fg*            "#888")

       (*mode-line-bg*       "#333")
       (*mode-inactive-bg*   "#222")
       (*mode-line-fg*       "#888")
       (*mode-inactive-fg*   "#555")

       (*red*                "#810")
       (*green*              "#080")

       (*comments*           "#555")
       (*keywords*           "#ba1")
       (*constant*           "#c0a")
       (*type-face*          "#349")
       (*method-declaration* "#09f")
       (*number*             "#00f")
       (*operators*          "#83e")
       (*warning*            *red*)
       (*regexp*             "#a36")
       (*string*             *green*)
       (*variable*           "#06c"))

  (custom-theme-set-faces
   'witness

   `(bold ((t (:bold t))))
   `(button ((t (:foreground ,*keywords* :underline t))))
   `(default ((t (:background ,*background* :foreground ,*normal*))))
   `(header-line ((t (:background ,*mode-line-bg* :foreground ,*normal*))))
   `(highlight ((t (:background ,*current-line*))))
   `(highlight-face ((t (:background ,*current-line*))))
   `(hl-line ((t (:bold t))))
   `(info-xref ((t (:foreground ,*keywords* :underline t))))
   `(region ((t (:background ,*visual-selection*))))
   `(secondary-selection ((t (:background ,*visual-selection-2*))))
   `(underline ((nil (:underline t))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,*operators*))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,*comments*))))
   `(font-lock-comment-face ((t (:foreground ,*comments*))))
   `(font-lock-constant-face ((t (:foreground ,*constant*))))
   `(font-lock-doc-face ((t (:foreground ,*string*))))
   `(font-lock-doc-string-face ((t (:foreground ,*string*))))
   `(font-lock-function-name-face ((t (:foreground ,*method-declaration*))))
   `(font-lock-keyword-face ((t (:foreground ,*keywords*))))
   `(font-lock-negation-char-face ((t (:foreground ,*warning*))))
   `(font-lock-number-face ((t (:foreground ,*number*))))
   `(font-lock-preprocessor-face ((t (:foreground ,*keywords*))))
   `(font-lock-reference-face ((t (:foreground ,*constant*))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,*regexp*))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,*regexp*))))
   `(font-lock-string-face ((t (:foreground ,*string*))))
   `(font-lock-type-face ((t (:foreground ,*type-face*))))
   `(font-lock-variable-name-face ((t (:foreground ,*variable*))))
   `(font-lock-warning-face ((t (:foreground ,*warning*))))

   ;; GUI
   `(fringe ((t (:background ,*background*))))
   `(linum ((t (:background ,*line-number* :foreground ,*line-fg* :height 90 :bold t))))
   `(minibuffer-prompt ((t (:foreground ,*variable*))))
   `(mode-line ((t (:background ,*mode-line-bg* :foreground ,*mode-line-fg*))))
   `(mode-line-inactive ((t (:background ,*mode-inactive-bg* :foreground ,*mode-inactive-fg*))))
   `(cursor ((t (:background ,*cursor-underscore*))))
   `(text-cursor ((t (:background ,*cursor-underscore*))))
   `(vertical-border ((t (:foreground ,*background*))))

   ;; show-paren
   `(show-paren-mismatch ((t (:background ,*warning* :foreground ,*normal* :weight bold))))
   `(show-paren-match ((t (:background ,*keywords* :foreground ,*normal* :weight bold))))

   ;; search
   `(isearch ((t (:background ,*regexp* :foreground ,*visual-selection*))))
   `(isearch-fail ((t (:background ,*warning*))))
   `(lazy-highlight ((t (:background ,*operators* :foreground ,*visual-selection*))))

   ;; diff
   `(diff-header ((t (:background ,*visual-selection-1*))))
   `(diff-file-header ((t (:bold t))))
   `(diff-added ((t (:foreground ,*green*))))
   `(diff-removed ((t (:foreground ,*red*))))
   ))

(provide-theme 'witness)

;;; witness-theme.el ends here
