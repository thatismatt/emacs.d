;;; footlamp-theme.el --- A light theme for Emacs.
;; Copyright (C) 2014 Matt Lee
;; Author: Matt Lee
;; URL: http://github.com/thatismatt
;; License: MIT

;;; Commentary:
;; A light theme.

;;; Code:

(unless (>= 24 emacs-major-version)
  (error "Requires Emacs 24 or later"))

(deftheme footlamp "A light color theme for Emacs by Matt Lee")

(custom-theme-set-variables
  'footlamp
  '(rainbow-delimiters-max-face-count 5))

(let* ((*grey-1*             "#111")
       (*grey-2*             "#222")
       (*grey-3*             "#333")
       (*grey-4*             "#444")
       (*grey-7*             "#777")
       (*grey-8*             "#888")
       (*grey-9*             "#999")
       (*grey-a*             "#aaa")
       (*grey-c*             "#ccc")
       (*grey-d*             "#ddd")
       (*grey-e*             "#eee")
       (*red*                "#b10")
       (*purple*             "#64a")
       (*blue*               "#009")
       (*blue-1*             "#26d")
       (*blue-2*             "#28b")
       (*cyan*               "#098")
       (*green*              "#083")
       (*green-1*            "#8a0")
       (*yellow*             "#e90")

       (*background*         *grey-e*)
       (*normal*             *grey-1*)
       (*cursor-underscore*  *grey-9*)
       (*visual-selection*   "#cef")
       (*visual-selection-1* "#def")
       (*visual-selection-2* "#cde")
       (*visual-selection-3* "#bcd")

       (*comments*           *grey-9*)
       (*keywords*           *blue*)
       (*constant*           *green*)
       (*type-face*          *blue-2*)
       (*function*           *blue-2*)
       (*number*             *blue-2*)
       (*string*             *cyan*)
       (*builtins*           *purple*)
       (*warning*            *red*)
       (*regexp*             "#a36")
       (*search-1*           *yellow*)
       (*search-2*           *red*)
       (*variable*           *blue-1*)
       (*variable-2*         *cyan*)
       (*highlight-1*        *blue*)
       (*highlight-2*        *red*)
       (*highlight-3*        *yellow*)
       )

  (custom-theme-set-faces
   'footlamp

   `(bold ((t (:bold t))))
   `(button ((t (:foreground ,*purple* :underline t))))
   `(default ((t (:background ,*background* :foreground ,*normal*))))
   `(header-line ((t (:foreground ,*highlight-1*))))
   `(highlight ((t (:foreground ,*background* :background ,*highlight-2*))))
   `(hl-line ((t (:weight bold :inherit nil))))
   `(info-xref ((t (:foreground ,*purple* :underline t))))
   `(region ((t (:background ,*visual-selection*))))
   `(secondary-selection ((t (:background ,*visual-selection-2*))))
   `(underline ((t (:underline t))))
   `(success ((t (:foreground ,*green*))))
   `(match ((t (:foreground ,*background* :background ,*highlight-1*))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,*builtins*))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,*comments*))))
   `(font-lock-comment-face ((t (:foreground ,*comments* :slant italic))))
   `(font-lock-doc-face ((t (:foreground ,*comments* :slant italic))))
   `(font-lock-doc-string-face ((t (:foreground ,*comments* :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,*constant*))))
   `(font-lock-function-name-face ((t (:foreground ,*function*))))
   `(font-lock-keyword-face ((t (:foreground ,*keywords*))))
   `(font-lock-negation-char-face ((t (:foreground ,*constant*))))
   `(font-lock-number-face ((t (:foreground ,*number*))))
   `(font-lock-preprocessor-face ((t (:foreground ,*keywords*))))
   `(font-lock-reference-face ((t (:foreground ,*constant*))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,*regexp*))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,*regexp*))))
   `(font-lock-string-face ((t (:foreground ,*string*))))
   `(font-lock-type-face ((t (:foreground ,*type-face*))))
   `(font-lock-variable-name-face ((t (:foreground ,*variable*))))
   `(font-lock-warning-face ((t (:foreground ,*warning* :slant italic))))

   ;; GUI
   `(fringe ((t (:background ,*background*))))
   `(linum ((t (:background ,*grey-d* :foreground ,*grey-8*))))
   `(minibuffer-prompt ((t (:foreground ,*variable*))))
   `(cursor ((t (:background ,*cursor-underscore*))))
   `(text-cursor ((t (:background ,*cursor-underscore*))))
   `(vertical-border ((t (:foreground ,*background*))))

   ;; mode line
   `(mode-line ((t (:background ,*grey-c* :foreground ,*grey-4*))))
   `(mode-line-inactive ((t (:background ,*grey-d* :foreground ,*grey-7*))))

   ;; show-paren
   `(show-paren-mismatch ((t (:background ,*warning* :foreground ,*normal* :weight bold))))
   `(show-paren-match ((t (:background ,*visual-selection-3* :foreground ,*normal* :weight bold))))

   ;; smartparens
   `(sp-show-pair-match-face ((t (:background ,*visual-selection-3*))))

   ;; rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground "#b00"))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground "#23d"))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground "#a70"))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground "#a0a"))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground "#090"))))

   ;; search
   `(isearch ((t (:background ,*search-1* :foreground ,*background*))))
   `(isearch-fail ((t (:background ,*warning*))))
   `(lazy-highlight ((t (:background ,*search-2* :foreground ,*background*))))

   ;; diff
   `(diff-header ((t (:background ,*visual-selection-1*))))
   `(diff-file-header ((t (:background ,*visual-selection-1*))))
   `(diff-added ((t (:foreground ,*green* :background nil))))
   `(diff-removed ((t (:foreground ,*red* :background nil))))

   ;; ediff
   `(ediff-even-diff-A ((t (:foreground ,*grey-8* :background ,*visual-selection-2*))))
   `(ediff-odd-diff-A ((t (:foreground ,*grey-a* :background ,*visual-selection-2*))))
   `(ediff-even-diff-B ((t (:foreground ,*grey-8* :background ,*visual-selection-2*))))
   `(ediff-odd-diff-B ((t (:foreground ,*grey-a* :background ,*visual-selection-2*))))

   ;; magit
   `(magit-branch ((t (:foreground ,*highlight-1*))))
   `(magit-log-sha1 ((t (:foreground ,*highlight-2*))))
   `(magit-tag ((t (:foreground ,*highlight-3* :background nil))))

   ;; speedbar
   `(speedbar-file-face ((t (:foreground ,*purple*))))
   `(speedbar-directory-face ((t (:foreground ,*purple*))))
   `(speedbar-selected-face ((t (:foreground ,*highlight-2*))))
   `(speedbar-highlight-face ((t (:foreground ,*grey-a*))))
   `(speedbar-button-face ((t (:foreground ,*green*))))
   `(speedbar-separator-face ((t (:foreground ,*grey-4*))))

   ;; org
   `(org-level-1 ((t (:foreground ,*blue*))))
   `(org-code ((t (:foreground ,*green*))))
   `(org-table ((t (:foreground ,*blue-1*))))
   `(org-document-title ((t (:foreground ,*yellow*))))
   `(org-document-info-keyword ((t (:foreground ,*purple*))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,*highlight-2*))))

   ;; shell
   `(sh-heredoc ((t (:foreground ,*green*))))
   `(sh-quoted-exec ((t (:foreground ,*highlight-2*))))

   ;; scala
   `(scala-font-lock:var-face ((t (:foreground ,*variable-2* :inherit nil))))

   ;; web
   `(web-mode-html-tag-face ((t (:foreground ,*function*))))
   `(web-mode-html-attr-name-face ((t (:foreground ,*variable*))))
   `(web-mode-block-attr-name-face ((t (:foreground ,*variable*))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'footlamp)

;;; footlamp-theme.el ends here
