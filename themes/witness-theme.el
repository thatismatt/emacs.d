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

(deftheme witness "The Witness color theme.")

(let* ((*grey-1*             "#111")
       (*grey-2*             "#222")
       (*grey-3*             "#333")
       (*grey-4*             "#444")
       (*grey-6*             "#666")
       (*grey-7*             "#777")
       (*grey-8*             "#888")
       (*grey-9*             "#999")
       (*grey-a*             "#aaa")
       (*grey-c*             "#ccc")
       (*grey-d*             "#ddd")
       (*grey-e*             "#eee")
       (*red*                "#d10")
       (*red-1*              "#a00")
       (*yellow*             "#f90")
       (*yellow-1*           "#d80")
       (*green*              "#090")
       (*green-1*            "#060")
       (*pink*               "#c0a")
       (*purple*             "#93e")
       (*purple-1*           "#609")
       (*blue*               "#19b")
       (*blue-1*             "#59f")

       (*background*         "#011")
       (*normal*             *grey-a*)
       (*cursor-underscore*  *grey-9*)
       (*visual-selection*   "#135")
       (*visual-selection-1* "#012")
       (*visual-selection-2* "#123")
       (*visual-selection-3* "#328")

       (*comments*           *grey-7*)
       (*keywords*           *purple*)
       (*constant*           *pink*)
       (*type-face*          *blue-1*)
       (*function*           *blue*)
       (*number*             *blue-1*)
       (*string*             *green*)
       (*builtins*           *pink*)
       (*link*               *purple*)
       (*warning*            *red*)
       (*regexp*             "#a36")
       (*search-1*           *yellow*)
       (*search-2*           "#a30")
       (*variable*           "#0a8")
       (*variable-2*         "#585")
       (*highlight-1*        *blue*)
       (*highlight-2*        *pink*)
       (*highlight-3*        *yellow-1*)
       (*highlight-4*        *green*)
       (*mode-line-bg*       *grey-6*)
       (*mode-line-fg*       *grey-1*)
       (*mode-line-bg-2*     *grey-4*)
       (*mode-line-fg-2*     *grey-2*)
       (*linum-bg*           *grey-6*)
       (*linum-fg*           *grey-1*)
       )

  (custom-theme-set-faces
   'witness

   `(bold ((t (:bold t))))
   `(button ((t (:foreground ,*link* :underline t))))
   `(default ((t (:background ,*background* :foreground ,*normal*))))
   `(header-line ((t (:foreground ,*highlight-1*))))
   `(highlight ((t (:background ,*highlight-1* :foreground ,*background*))))
   `(hl-line ((t (:weight bold :inherit nil))))
   `(info-xref ((t (:foreground ,*link* :underline t))))
   `(region ((t (:background ,*visual-selection* :foreground nil))))
   `(secondary-selection ((t (:background ,*visual-selection-2*))))
   `(underline ((t (:underline t))))
   `(success ((t (:foreground ,*green*))))
   `(match ((t (:foreground ,*background* :background ,*highlight-1*))))
   `(shadow ((t (:foreground ,*purple*))))
   `(link ((t (:foreground ,*link*))))
   `(error ((t (:foreground ,*warning*))))

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
   `(linum ((t (:background ,*linum-bg* :foreground ,*linum-fg*))))
   `(minibuffer-prompt ((t (:foreground ,*variable*))))
   `(cursor ((t (:background ,*cursor-underscore*))))
   `(text-cursor ((t (:background ,*cursor-underscore*))))
   `(vertical-border ((t (:foreground ,*background*))))
   `(mode-line ((t (:background ,*mode-line-bg* :foreground ,*mode-line-fg*))))
   `(mode-line-inactive ((t (:background ,*mode-line-bg-2* :foreground ,*mode-line-fg-2*))))

   ;; show-paren
   `(show-paren-mismatch ((t (:background ,*warning* :foreground ,*normal* :weight bold))))
   `(show-paren-match ((t (:background ,*visual-selection-3* :foreground ,*normal* :weight bold))))

   ;; smartparens
   `(sp-show-pair-match-face ((t (:background ,*visual-selection-3*))))

   ;; rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground "#f0a"))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground "#f0e"))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground "#d0f"))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground "#90f"))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground "#60f"))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground "#90f"))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground "#c0f"))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground "#f0f"))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground "#f0d"))))

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
   `(magit-diff-context ((t (:foreground ,*grey-8* :background ,*background*))))
   `(magit-diff-removed ((t (:foreground ,*red-1* :background ,*background*))))
   `(magit-diff-added ((t (:foreground ,*green-1* :background ,*background*))))
   `(magit-diff-context-highlight ((t (:foreground ,*normal* :background ,*background*))))
   `(magit-diff-removed-highlight ((t (:foreground ,*red* :background ,*background*))))
   `(magit-diff-added-highlight ((t (:foreground ,*green* :background ,*background*))))
   `(magit-branch-local ((t (:foreground ,*highlight-1*))))
   `(magit-branch-remote ((t (:foreground ,*highlight-2*))))
   `(magit-log-sha1 ((t (:foreground ,*highlight-2*))))
   `(magit-tag ((t (:foreground ,*highlight-3* :background nil))))
   `(magit-log-head-label-tags ((t (:foreground ,*highlight-3* :background nil :box 1))))
   `(magit-log-head-label-head ((t (:foreground ,*highlight-1* :background nil :box 1))))
   `(magit-log-head-label-local ((t (:foreground ,*highlight-2* :background nil :box 1))))
   `(magit-log-head-label-remote ((t (:foreground ,*highlight-3* :background nil :box 1))))
   `(magit-log-head-label-default ((t (:foreground ,*highlight-4* :background nil :box 1))))
   `(magit-section-heading ((t (:foreground ,*highlight-1* :background nil))))
   `(magit-section-highlight ((t (:foreground nil :background nil))))
   `(magit-hash ((t (:foreground ,*highlight-3*))))

   ;; speedbar
   `(speedbar-file-face ((t (:foreground ,*normal*))))
   `(speedbar-directory-face ((t (:foreground ,*normal*))))
   `(speedbar-selected-face ((t (:foreground ,*highlight-1*))))
   `(speedbar-highlight-face ((t (:foreground ,*highlight-2* :background nil))))
   `(speedbar-button-face ((t (:foreground ,*highlight-4*))))
   `(speedbar-separator-face ((t (:foreground ,*grey-4*))))

   ;; org
   `(org-level-1 ((t (:foreground ,*highlight-1*))))
   `(org-level-2 ((t (:foreground ,*highlight-2*))))
   `(org-level-3 ((t (:foreground ,*highlight-3*))))
   `(org-level-4 ((t (:foreground ,*highlight-4*))))
   `(org-code ((t (:foreground ,*green*))))
   `(org-table ((t (:foreground ,*blue-1*))))
   `(org-document-title ((t (:foreground ,*yellow*))))
   `(org-document-info-keyword ((t (:foreground ,*link*))))
   `(org-todo-face ((t (:foreground ,*red*))))
   `(org-done-face ((t (:foreground ,*green*))))
   `(org-doing-face ((t (:foreground ,*yellow*))))
   `(org-postponed-face ((t (:foreground ,*grey-7*))))

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
   `(web-mode-block-face ((t (:foreground ,*pink*))))

   ;; js2
   `(js2-external-variable ((t (:foreground ,*warning*))))

   ;; ocaml
   `(tuareg-font-lock-governing-face ((t (:foreground ,*keywords*))))
   `(tuareg-font-lock-operator-face ((t (:foreground ,*function*))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'witness)

;;; witness-theme.el ends here
