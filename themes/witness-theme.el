;;; witness-theme.el --- A dark theme for Emacs.
;; Copyright (c) Matt Lee
;; Author: Matt Lee
;; URL: http://github.com/thatismatt
;; License: MIT

;;; Commentary:
;; A dark theme.

;;; Code:

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(deftheme witness "The Witness color theme.")

(defun color-hsl-to-hex (h s l)
  "Create a hex color from H, S, & L."
  (apply 'color-rgb-to-hex (color-hsl-to-rgb h s l)))

(let* ((*grey-1*             "#111")
       (*grey-2*             "#222")
       (*grey-3*             "#333")
       (*grey-4*             "#444")
       (*grey-6*             "#666")
       (*grey-7*             "#777")
       (*grey-8*             "#888")
       (*grey-9*             "#999")
       (*grey-a*             "#aaa")
       (*grey-b*             "#bbb")
       (*grey-c*             "#ccc")
       (*grey-d*             "#ddd")
       (*grey-e*             "#eee")
       (*red*                "#f03")
       (*red-1*              "#a01")
       (*red-2*              "#700")
       (*yellow*             "#f91")
       (*yellow-1*           "#d70")
       (*orange*             "#a30")
       (*green*              "#0a2")
       (*green-1*            "#060")
       (*green-2*            "#094")
       (*green-3*            "#040")
       (*pink*               "#e0b")
       (*purple*             "#a3f")
       (*blue*               "#0bd")
       (*blue-1*             "#59f")

       (*background*         "#011")
       (*background-1*       "#10191f")

       (*mode-line-bg*       *grey-6*)
       (*mode-line-fg*       *background*)
       (*mode-line-bg-2*     *grey-4*)
       (*mode-line-fg-2*     *grey-2*)

       (*normal*             *grey-b*)
       (*cursor*             *grey-9*)
       (*visual-selection*   "#135")
       (*visual-selection-1* "#012")
       (*visual-selection-2* "#123")
       (*visual-selection-3* "#328")

       (*comments*           *grey-8*)
       (*keywords*           *purple*)
       (*constant*           *pink*)
       (*type-face*          *blue-1*)
       (*function*           *blue*)
       (*number*             *blue-1*)
       (*string*             *green-2*)
       (*builtins*           *pink*)
       (*link*               *purple*)
       (*warning*            *red*)
       (*regexp*             "#a36")
       (*search-1*           *yellow*)
       (*search-2*           *orange*)
       (*variable*           "#0a8")
       (*highlight-1*        *blue*)
       (*highlight-2*        *pink*)
       (*highlight-3*        *yellow*)
       (*highlight-4*        *green*)
       (*highlight-bg*       *grey-3*)
       )

  (custom-theme-set-faces
   'witness

   `(bold ((t (:bold t))))
   `(button ((t (:foreground ,*link* :underline t))))
   `(default ((t (:background ,*background* :foreground ,*normal*))))
   `(header-line ((t (:foreground ,*highlight-1*))))
   `(highlight ((t (:background ,*highlight-1* :foreground ,*background*))))
   `(italic ((t (:foreground ,*grey-9* :slant italic))))
   `(hl-line ((t (:weight bold :inherit nil))))
   `(info-xref ((t (:foreground ,*link* :underline t))))
   `(region ((t (:background ,*visual-selection* :foreground nil))))
   `(secondary-selection ((t (:background ,*visual-selection-2*))))
   `(underline ((t (:underline t))))
   `(success ((t (:foreground ,*green*))))
   `(match ((t (:foreground ,*background* :background ,*highlight-1*))))
   `(shadow ((t (:foreground ,*purple*))))
   `(link ((t (:foreground ,*link* :underline t))))
   `(error ((t (:foreground ,*warning*))))
   `(fixed-pitch ((t (:family nil))))
   `(fixed-pitch-serif ((t (:family nil))))
   `(minibuffer-prompt ((t (:foreground ,*function*))))

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

   ;; gui
   `(fringe ((t (:background ,*background-1*))))
   `(line-number ((t (:background ,*mode-line-bg-2* :foreground ,*mode-line-fg-2*))))
   `(line-number-current-line ((t (:background ,*mode-line-bg* :foreground ,*mode-line-fg*))))
   `(minibuffer-prompt ((t (:foreground ,*variable*))))
   `(cursor ((t (:background ,*cursor*))))
   `(text-cursor ((t (:background ,*cursor*))))
   `(vertical-border ((t (:foreground ,*background*))))
   `(mode-line ((t (:background ,*mode-line-bg* :foreground ,*mode-line-fg*))))
   `(mode-line-inactive ((t (:background ,*mode-line-bg-2* :foreground ,*mode-line-fg-2*))))

   ;; show-paren
   `(show-paren-mismatch ((t (:background ,*warning* :foreground ,*normal* :weight bold))))
   `(show-paren-match ((t (:background ,*visual-selection-3* :foreground ,*normal* :weight bold))))

   ;; smartparens
   `(sp-show-pair-match-face ((t (:background ,*visual-selection-3*))))

   ;; rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground "#e0b"))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground "#f0e"))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground "#d1f"))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground "#a2f"))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground "#82f"))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground "#a2f"))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground "#c1f"))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground "#f0f"))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground "#f0d"))))

   ;; search
   `(isearch ((t (:background ,*search-1* :foreground ,*background*))))
   `(isearch-fail ((t (:background ,*warning* :foreground ,*background*))))
   `(lazy-highlight ((t (:background ,*search-2* :foreground ,*background*))))

   ;; diff
   `(diff-header ((t (:background ,*visual-selection-1*))))
   `(diff-file-header ((t (:background ,*visual-selection-1*))))
   `(diff-added ((t (:foreground ,*green* :background nil))))
   `(diff-removed ((t (:foreground ,*red* :background nil))))
   `(diff-refine-added ((t (:background ,*green-1*))))
   `(diff-refine-removed ((t (:background ,*red-1*))))

   ;; smerge
   `(smerge-refined-added ((t (:background ,*green-1*))))
   `(smerge-refined-removed ((t (:background ,*red-1*))))
   `(smerge-upper ((t (:background ,*red-2*))))
   `(smerge-lower ((t (:background ,*green-3*))))

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
   `(magit-diff-file-heading-selection ((t (:foreground ,*highlight-2*))))
   `(magit-diff-lines-heading ((t (:foreground ,*background* :background ,*highlight-2*))))
   `(magit-branch-local ((t (:foreground ,*highlight-1*))))
   `(magit-branch-remote ((t (:foreground ,*highlight-2*))))
   `(magit-tag ((t (:foreground ,*highlight-4* :background nil :box t))))
   `(magit-section-heading ((t (:foreground ,*highlight-1* :background nil))))
   `(magit-section-highlight ((t (:foreground nil :background nil))))
   `(magit-section-heading-selection ((t (:foreground ,*highlight-2*))))
   `(magit-hash ((t (:foreground ,*highlight-3*))))

   ;; speedbar
   `(speedbar-file-face ((t (:foreground ,*normal*))))
   `(speedbar-directory-face ((t (:foreground ,*normal*))))
   `(speedbar-selected-face ((t (:foreground ,*highlight-1*))))
   `(speedbar-highlight-face ((t (:foreground ,*highlight-2* :background nil))))
   `(speedbar-button-face ((t (:foreground ,*highlight-4*))))
   `(speedbar-separator-face ((t (:foreground ,*grey-4*))))

   ;; idle-highlight
   `(idle-highlight ((t (:background ,*grey-3* :foreground nil))))

   ;; org
   `(org-level-1 ((t (:foreground ,*highlight-1*))))
   `(org-level-2 ((t (:foreground ,*highlight-2*))))
   `(org-level-3 ((t (:foreground ,*highlight-3*))))
   `(org-level-4 ((t (:foreground ,*highlight-4*))))
   `(org-code ((t (:foreground ,*green*))))
   `(org-table ((t (:foreground ,*blue-1*))))
   `(org-document-title ((t (:foreground ,*yellow*))))
   `(org-document-info-keyword ((t (:foreground ,*link*))))
   `(org-todo ((t (:foreground ,*red* :box t))))
   `(org-done ((t (:foreground ,*green* :box t))))
   `(org-todo-face ((t (:foreground ,*red* :box t))))
   `(org-done-face ((t (:foreground ,*green* :box t))))
   `(org-doing-face ((t (:foreground ,*yellow* :box t))))
   `(org-postponed-face ((t (:foreground ,*grey-7* :box t))))
   `(org-query-face ((t (:foreground ,*purple* :box t))))
   `(org-checkbox ((t (:foreground ,*yellow*))))
   `(org-date ((t (:foreground ,*blue*))))
   `(org-block ((t (:foreground nil))))
   `(org-priority ((t (:foreground ,*yellow*))))

   ;; markdown
   `(markdown-link-face ((t (:foreground ,*blue-1*))))
   `(markdown-code-face ((t (:foreground ,*purple*))))

   ;; calendar
   `(calendar-today ((t (:foreground ,*green* :background nil :box t))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,*highlight-2*))))
   `(eshell-ls-directory ((t (:foreground ,*blue*))))
   `(eshell-ls-symlink ((t (:foreground ,*blue-1*))))
   `(eshell-ls-executable ((t (:foreground ,*green*))))
   `(eshell-ls-readonly ((t (:foreground ,*red*))))
   `(eshell-ls-missing ((t (:foreground ,*red*))))

   ;; shell
   `(sh-heredoc ((t (:foreground ,*green*))))
   `(sh-quoted-exec ((t (:foreground ,*highlight-2*))))

   ;; eldoc
   `(eldoc-highlight-function-argument ((t (:foreground ,*pink*))))

   ;; cider
   `(cider-test-failure-face ((t (:foreground ,*grey-1* :background ,*red-1*))))
   `(cider-test-error-face ((t (:foreground ,*grey-1* :background ,*yellow-1*))))
   `(cider-test-success-face ((t (:foreground ,*grey-1* :background ,*green*))))

   ;; web
   `(web-mode-html-tag-face ((t (:foreground ,*function*))))
   `(web-mode-html-attr-name-face ((t (:foreground ,*variable*))))
   `(web-mode-block-attr-name-face ((t (:foreground ,*variable*))))
   `(web-mode-block-face ((t (:foreground ,*pink*))))

   ;; js2
   `(js2-external-variable ((t (:foreground ,*warning*))))
   `(js2-object-property ((t (:foreground ,*variable*))))

   ;; ocaml
   `(tuareg-font-lock-governing-face ((t (:foreground ,*keywords*))))
   `(tuareg-font-lock-operator-face ((t (:foreground ,*function*))))

   ;; nxml
   `(nxml-glyph ((t (:foreground ,*keywords*))))

   ;; erc
   `(erc-notice-face ((t (:foreground ,*keywords*))))
   `(erc-timestamp-face ((t (:foreground ,*type-face*))))
   `(erc-current-nick-face ((t (:foreground ,*builtins*))))
   `(erc-my-nick-face ((t (:foreground ,*string*))))
   `(erc-input-face ((t (:foreground ,*normal*))))
   `(erc-nick-default-face ((t (:weight bold))))
   `(erc-prompt-face ((t (:foreground ,*function*))))
   `(erc-nick-msg-face ((t (:foreground ,*builtins*))))
   `(erc-direct-msg-face ((t (:foreground ,*normal*))))
   `(erc-error-face ((t (:foreground ,*warning*))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'witness)

;;; witness-theme.el ends here
