;; matt-theme-make.el -- Matt Lee's theme helpers

;;; Commentary:

;;; Code:

;; https://paletton.com/

(require 'color)
(require 'seq)

(defun color-hsl-to-hex (h s l)
  "Create a hex color from H, S, & L."
  (apply 'color-rgb-to-hex (color-hsl-to-rgb h s l)))

(defun color-hue-spread (hue-mid num hue-range)
  (let ((hue-delta (/ hue-range 1.0 (1- num)))
        (hue-min   (mod (- hue-mid (/ hue-range 1.0 2.0)) 1.0)))
    (mapcar
     (lambda (i)
       (mod (+ hue-min (* hue-delta i)) 1))
     (number-sequence 0 (1- num)))))

(defun color-rgb-luminance (red green blue)
  (+ (* 0.2126 red) (* 0.7152 green) (* 0.0722 blue)))

(defun color-luminance (color)
  (apply 'color-rgb-luminance (color-name-to-rgb color)))

(defun palette (primary-hue s l)
  (seq-map (lambda (h) (color-hsl-to-hex h s l)) (color-hue-spread primary-hue 5 0.5)))

;; (let ((h 0.1)
;;       (s 0.5)
;;       (l 0.5))
;;   (list (color-hsl-to-hex h s l) (palette h s l)))

;; ("#bfff8ccc3fff" ("#bfff3fffb332"
;;                   "#bfff3fff5332"
;;                   "#bfff8ccc3fff"
;;                   "#9332bfff3fff"
;;                   "#3fffbfff4ccc"))

;; ("#ffff99990000" ("#ffff0000e665"
;;                   "#ffff00002666"
;;                   "#ffff99990000"
;;                   "#a665ffff0000"
;;                   "#0000ffff1999"))

;; ("#ffff99990000" ("#00006665ffff"
;;                   "#99980000ffff"
;;                   "#ffff00006665"
;;                   "#ffff99990000"
;;                   "#6666ffff0000"
;;                   "#0000ffff9998"
;;                   "#00006665ffff"))


;; ("#ffffe6650000" ("#00001999ffff"
;;                   "#e6650000ffff"
;;                   "#ffff00001999"
;;                   "#ffffe6650000"
;;                   "#1999ffff0000"
;;                   "#0000ffffe665"
;;                   "#00001999ffff"))

;; (color-luminance "#000") ;; 0.0
;; (color-luminance (color-lighten-name "#000" 10)) ;; 0.09999237048905164
;; (color-luminance (color-lighten-name "#000" 20)) ;; 0.2
;; (color-luminance (color-lighten-name "#000" 30)) ;; 0.2999923704890516

;; (color-luminance (color-hsl-to-hex 0.6 0.15 0.05)) ;; 0.047868256656748306
;; (color-luminance (color-hsl-to-hex 0.6 0.15 0.05))

;; (let* ((theme-hue 0.2)
;;        (*background*   (color-hsl-to-hex theme-hue 0.15 0.05))
;;        (*background-1* (color-lighten-name *background* 10))
;;        (*background-2* (color-lighten-name *background* 20))
;;        (*background-3* (color-lighten-name *background* 30)))
;;   (list theme-hue
;;         (color-hsl-to-hex theme-hue 1.0 0.5)
;;         *background*
;;         (color-luminance *background*  )
;;         (color-luminance *background-1*)
;;         (color-luminance *background-2*)
;;         (color-luminance *background-3*)))

;; (0.2 "#cccbffff0000" "#0df30eb80ae1" 0.05577394064240482 0.16732182192721445 0.27888386053253983 0.3904328435187304)
;; (0.0 "#ffff00000000" "#0eb80ae10ae1" 0.04568529488059815 0.13705588464179444 0.22842971847104598 0.3198123231860837)

(defun matt-theme-color-lighten-hsl (H S L percent)
  "A version of `color-lighten-hsl' that works as it used to."
  ;; https://github.com/emacs-mirror/emacs/commit/656c2dd66e77a5fbeb99d358017e8327401fae05
  (list H S (color-clamp (+ L (/ percent 100.0)))))

(defun matt-theme-color-lighten (name percent)
  "A version of `color-lighten-name' that works as it used to."
  ;; https://github.com/emacs-mirror/emacs/commit/656c2dd66e77a5fbeb99d358017e8327401fae05
  (apply 'color-rgb-to-hex
	     (apply 'color-hsl-to-rgb
		        (apply 'matt-theme-color-lighten-hsl
		               (append
			            (apply 'color-rgb-to-hsl
			                   (color-name-to-rgb name))
			            (list percent))))))

(defun matt-theme-make (theme-name &rest theme-colors)
  "Given a THEME-NAME and a base set of THEME-COLORS create a theme."
  (let* ((*background*   (plist-get theme-colors :background))
         (lightp         (< 0.5 (color-luminance *background*)))
         (*background-0* (matt-theme-color-lighten *background* (if lightp -5 5)))
         (*background-1* (matt-theme-color-lighten *background* (if lightp -10 10)))
         (*background-2* (matt-theme-color-lighten *background* (if lightp -20 20)))
         (*background-3* (matt-theme-color-lighten *background* (if lightp -30 30)))
         (*default*      (plist-get theme-colors :default))
         (default-hsl    (apply 'color-rgb-to-hsl (color-name-to-rgb *default*)))
         (*default-1*    (color-darken-name *default* (if lightp -10 10)))
         (*default-2*    (color-darken-name *default* (if lightp -20 20)))
         (*default-3*    (color-darken-name *default* (if lightp -30 30)))
         (*default-4*    (color-darken-name *default* (if lightp -40 40)))
         (*default-5*    (color-darken-name *default* (if lightp -50 50)))
         (*cursor*       (plist-get theme-colors :cursor))
         (*comment*      (plist-get theme-colors :comment))
         (*success*      (plist-get theme-colors :success))
         (*success-1*    (color-darken-name *success* 10))
         (*success-bg-1* (color-darken-name *success* 45))
         (*success-bg-2* (color-darken-name *success* 60))
         (*warn*         (plist-get theme-colors :warn))
         (*warn-1*       (color-darken-name *warn* 10))
         (*warn-bg-1*    (color-darken-name *warn* 60))
         (*error*        (plist-get theme-colors :error))
         (*error-1*      (color-darken-name *error* 10))
         (*error-bg-1*   (color-darken-name *error* 45))
         (*error-bg-2*   (color-darken-name *error* 60))
         (*a1*           (plist-get theme-colors :a1))
         (*a2*           (color-darken-name *a1* (if lightp -10 10)))
         (*a3*           (color-darken-name *a1* (if lightp -25 25)))
         (*b1*           (plist-get theme-colors :b1))
         (*b2*           (color-darken-name *b1* 10))
         (*b3*           (color-darken-name *b1* 20))
         (*b4*           (color-desaturate-name *b3* 20)) ;; TODO: rename
         (*c1*           (plist-get theme-colors :c1))
         (*c2*           (color-darken-name *c1* 10))
         (*c3*           (color-darken-name *c1* 20))
         (*d1*           (plist-get theme-colors :d1))
         (*d2*           (color-darken-name *d1* 10))
         (*e1*           (plist-get theme-colors :e1))
         (*e2*           (color-darken-name *e1* 10))
         (*rainbow-colors*
          (seq-map (lambda (h) (color-hsl-to-hex h 0.8 0.5)) (color-hue-spread (car default-hsl) 9 -0.3))))

    ;; (setq matt-theme-debug (list :background *background*
    ;;                              :background-0 *background-0*
    ;;                              :background-1 *background-1*
    ;;                              :background-2 *background-2*
    ;;                              :background-3 *background-3*))

    (custom-theme-set-faces
     theme-name

     `(bold ((t (:bold t))))
     `(button ((t (:foreground ,*b2* :underline t))))
     `(default ((t (:background ,*background* :foreground ,*default*))))
     `(header-line ((t (:foreground ,*a1*))))
     `(highlight ((t (:background ,*a1* :foreground ,*background*))))
     `(italic ((t (:foreground ,*c2* :slant italic))))
     `(info-xref ((t (:foreground ,*b2* :underline t))))
     `(region ((t (:background ,*background-2*))))
     `(secondary-selection ((t (:background ,*background-1* :foreground unspecified))))
     `(underline ((t (:underline t))))
     `(success ((t (:foreground ,*success*))))
     `(error ((t (:foreground ,*error*))))
     `(warning ((t (:foreground ,*warn* :weight normal))))
     `(match ((t (:background ,*background-1*))))
     `(shadow ((t (:foreground ,*default-5*))))
     `(link ((t (:foreground ,*b4* :underline t))))
     `(fixed-pitch ((t (:foreground ,*a2* :family nil))))
     `(fixed-pitch-serif ((t (:family nil))))
     `(minibuffer-prompt ((t (:foreground ,*a1*))))

     ;; font-lock
     `(font-lock-comment-delimiter-face ((t (:foreground ,*comment*))))
     `(font-lock-comment-face ((t (:foreground ,*comment* :slant italic))))
     `(font-lock-doc-face ((t (:foreground ,*comment* :slant italic))))
     `(font-lock-doc-string-face ((t (:foreground ,*comment* :slant italic))))
     `(font-lock-builtin-face ((t (:foreground ,*a1*))))
     `(font-lock-keyword-face ((t (:foreground ,*d1*))))
     `(font-lock-variable-name-face ((t (:foreground ,*b1*))))
     `(font-lock-type-face ((t (:foreground ,*c1*))))
     `(font-lock-constant-face ((t (:foreground ,*a1*))))
     `(font-lock-string-face ((t (:foreground ,*e1*))))
     `(font-lock-function-name-face ((t (:foreground ,*b1*))))
     `(font-lock-negation-char-face ((t (:foreground ,*d1*))))
     `(font-lock-number-face ((t (:foreground ,*c1*))))
     `(font-lock-preprocessor-face ((t (:foreground ,*b1*))))
     `(font-lock-reference-face ((t (:foreground ,*a1*))))
     `(font-lock-regexp-grouping-backslash ((t (:foreground ,*e1*))))
     `(font-lock-regexp-grouping-construct ((t (:foreground ,*e1*))))
     `(font-lock-warning-face ((t (:foreground ,*error*))))

     ;; gui
     `(fringe ((t (:background ,*background-1*))))
     `(line-number ((t (:background ,*background-2* :foreground ,*background*))))
     `(line-number-current-line ((t (:background ,*background-3* :foreground ,*background*))))
     `(mode-line ((t (:background ,*background-3* :foreground ,*background*))))
     ;; `(header-line ((t (:background ,*background-3* :foreground ,*background*))))
     `(mode-line-inactive ((t (:background ,*background-2* :foreground ,*background*))))
     `(minibuffer-prompt ((t (:foreground ,*a1*))))
     `(cursor ((t (:background ,*cursor*))))
     `(text-cursor ((t (:background ,*cursor*))))
     `(vertical-border ((t (:foreground ,*background*))))

     ;; show-paren
     `(show-paren-mismatch ((t (:background ,*error* :foreground ,*default* :weight bold))))
     `(show-paren-match ((t (:background ,*background-1* :foreground ,*default* :weight bold))))

     ;; smartparens
     `(sp-show-pair-match-face ((t (:background ,*background-3* :foreground ,*default*))))

     ;; rainbow delimiters
     `(rainbow-delimiters-depth-1-face ((t (:foreground ,(nth 0 *rainbow-colors*)))))
     `(rainbow-delimiters-depth-2-face ((t (:foreground ,(nth 1 *rainbow-colors*)))))
     `(rainbow-delimiters-depth-3-face ((t (:foreground ,(nth 2 *rainbow-colors*)))))
     `(rainbow-delimiters-depth-4-face ((t (:foreground ,(nth 3 *rainbow-colors*)))))
     `(rainbow-delimiters-depth-5-face ((t (:foreground ,(nth 4 *rainbow-colors*)))))
     `(rainbow-delimiters-depth-6-face ((t (:foreground ,(nth 5 *rainbow-colors*)))))
     `(rainbow-delimiters-depth-7-face ((t (:foreground ,(nth 6 *rainbow-colors*)))))
     `(rainbow-delimiters-depth-8-face ((t (:foreground ,(nth 7 *rainbow-colors*)))))
     `(rainbow-delimiters-depth-9-face ((t (:foreground ,(nth 8 *rainbow-colors*)))))
     `(rainbow-delimiters-base-error-face ((t (:foreground ,*error*))))

     ;; search
     `(isearch ((t (:background ,*a2* :foreground ,*background*))))
     `(isearch-fail ((t (:background ,*error* :foreground ,*background*))))
     `(lazy-highlight ((t (:background ,*default-4* :foreground ,*background*))))

     ;; diff
     ;; `(diff-header ((t (:background ,*visual-selection-1*))))
     ;; `(diff-file-header ((t (:background ,*visual-selection-1*))))
     ;; `(diff-added ((t (:foreground ,*green* :background nil))))
     ;; `(diff-removed ((t (:foreground ,*red* :background nil))))
     ;; `(diff-refine-added ((t (:background ,*green-1*))))
     ;; `(diff-refine-removed ((t (:background ,*red-1*))))

     ;; smerge
     `(smerge-refined-added ((t (:background ,*success-bg-1*))))
     `(smerge-refined-removed ((t (:background ,*error-bg-1*))))
     `(smerge-markers ((t (:background ,*background-1* :extend t))))
     `(smerge-upper ((t (:background ,*error-bg-2* :extend t))))
     `(smerge-lower ((t (:background ,*success-bg-2* :extend t))))
     `(smerge-base ((t (:background ,*warn-bg-1* :extend t))))

     ;; ediff
     `(ediff-even-diff-A ((t (:foreground ,*default-1* :background ,*background-1*))))
     `(ediff-even-diff-B ((t (:inherit 'ediff-even-diff-A))))
     `(ediff-even-diff-C ((t (:inherit 'ediff-even-diff-A))))
     `(ediff-odd-diff-A ((t (:inherit 'ediff-even-diff-A))))
     `(ediff-odd-diff-B ((t (:inherit 'ediff-even-diff-A))))
     `(ediff-odd-diff-C ((t (:inherit 'ediff-even-diff-A))))
     `(ediff-current-diff-A ((t (:foreground ,*default* :background ,*background-2*))))
     `(ediff-fine-diff-A ((t (:foreground ,*default* :background ,*background-3*))))
     `(ediff-current-diff-B ((t (:foreground ,*default* :background ,*background-2*))))
     `(ediff-fine-diff-B ((t (:foreground ,*default* :background ,*background-3*))))
     `(ediff-current-diff-C ((t (:foreground ,*default* :background ,*background-2*))))
     `(ediff-fine-diff-C ((t (:foreground ,*default* :background ,*background-3*))))

     ;; transient
     `(transient-key-stay ((t (:foreground ,*c1*))))
     `(transient-key-return ((t (:foreground ,*b1*))))
     `(transient-key-exit ((t (:foreground ,*a1*))))

     ;; magit
     `(magit-diff-context ((t (:foreground ,*comment* :background unspecified))))
     `(magit-diff-removed ((t (:foreground ,*error-1* :background unspecified))))
     `(magit-diff-added ((t (:foreground ,*success-1* :background unspecified))))
     `(magit-diff-base ((t (:foreground ,*warn-1* :background unspecified))))
     `(magit-diff-context-highlight ((t (:foreground ,*default* :background unspecified))))
     `(magit-diff-removed-highlight ((t (:foreground ,*error* :background unspecified))))
     `(magit-diff-added-highlight ((t (:foreground ,*success* :background unspecified))))
     `(magit-diff-base-highlight ((t (:foreground ,*warn* :background unspecified))))
     `(magit-diff-file-heading-selection ((t (:foreground ,*b1*))))
     `(magit-diff-hunk-heading-selection ((t (:foreground ,*b1*))))
     `(magit-diff-hunk-heading ((t (:foreground ,*default-3*))))
     `(magit-diff-hunk-heading-highlight ((t (:foreground ,*default-1*))))
     ;; (magit-diff-hunk-region ((t (:foreground ,*c1*))))
     `(magit-diff-lines-heading ((t (:foreground ,*background* :background ,*b1*))))
     ;; magit-diff-lines-boundary
     `(magit-diff-revision-summary ((t (:foreground ,*b2*))))
     `(magit-diff-revision-summary-highlight ((t (:foreground ,*b1*))))
     `(magit-branch-local ((t (:foreground ,*a1*))))
     `(magit-branch-remote ((t (:foreground ,*b1*))))
     `(magit-tag ((t (:foreground ,*d1* :background unspecified :box t))))
     `(magit-section-heading ((t (:height 1.2 :foreground ,*a1*))))
     `(magit-section-highlight ((t (:foreground unspecified :background unspecified))))
     `(magit-section-heading-selection ((t (:foreground ,*b1*))))
     `(magit-hash ((t (:foreground ,*c1*))))

     ;; idle-highlight
     `(idle-highlight ((t (:background ,*background-1*))))

     ;; org
     `(org-level-1 ((t (:foreground ,*a1*))))
     `(org-level-2 ((t (:foreground ,*b1*))))
     `(org-level-3 ((t (:foreground ,*c1*))))
     `(org-level-4 ((t (:foreground ,*d1*))))
     `(org-level-5 ((t (:foreground ,*e1*))))
     `(org-level-6 ((t (:foreground ,*a2*))))
     `(org-level-7 ((t (:foreground ,*b2*))))
     `(org-level-8 ((t (:foreground ,*c2*))))
     `(org-level-9 ((t (:foreground ,*d2*))))
     `(org-table ((t (:foreground ,*d1*))))
     `(org-document-title ((t (:height 1.2 :foreground ,*c1*))))
     `(org-document-info-keyword ((t (:foreground ,*b2*))))
     `(org-todo ((t (:foreground ,*error* :box (:line-width (1 . -1))))))
     `(org-done ((t (:foreground ,*success* :box (:line-width (1 . -1))))))
     `(org-doing-face ((t (:foreground ,*warn* :box (:line-width (1 . -1))))))
     `(org-postponed-face ((t (:foreground ,*comment* :box (:line-width (1 . -1))))))
     `(org-query-face ((t (:foreground ,*a1* :box (:line-width (1 . -1))))))
     `(org-headline-done ((t (:foreground ,*comment*))))
     `(org-tag ((t (:foreground ,*background-2* :background ,*background-0*))))
     `(org-date ((t (:foreground ,*b2*))))
     `(org-checkbox ((t (:foreground ,*success-1*))))
     `(org-block ((t (:foreground unspecified))))
     `(org-block-begin-line ((t (:foreground ,*background-1*))))
     `(org-checkbox ((t (:foreground ,*e2*))))
     `(org-priority ((t (:foreground ,*e2*))))
     `(org-meta-line ((t (:foreground ,*background-1*))))
     ;; `(org-special-keyword ((t (:foreground ,*c3*))))
     `(org-drawer ((t (:foreground ,*background-1*))))

     ;; calendar
     `(calendar-today ((t (:foreground ,*a1* :background unspecified :box t))))

     ;; eshell
     `(eshell-prompt ((t (:foreground ,*a1*))))
     `(eshell-ls-directory ((t (:foreground ,*a1*))))
     `(eshell-ls-symlink ((t (:foreground ,*b1*))))
     `(eshell-ls-executable ((t (:foreground ,*c1*))))
     `(eshell-ls-readonly ((t (:foreground ,*error*))))
     `(eshell-ls-missing ((t (:foreground ,*error*))))
     `(eshell-ls-backup ((t (:foreground ,*d1*))))

     ;; shell
     `(sh-heredoc ((t (:foreground ,*a1*))))
     `(sh-quoted-exec ((t (:foreground ,*b1*))))

     ;; term
     `(term-color-green ((t (:foreground ,*a1* :background ,*a1*))))

     ;; eldoc
     `(eldoc-highlight-function-argument ((t (:foreground ,*a1*))))

     ;; flycheck
     `(flycheck-warning ((t (:underline (:style wave :color ,*warn*) :strike-through unspecified))))

     ;; vertico
     `(vertico-current ((t (:background ,*background-1*))))

     ;; lsp
     `(lsp-face-highlight-textual ((t (:background ,*background-1* :foreground unspecified))))
     `(lsp-ui-sideline-global ((t (:italic nil))))

     ;; cider
     `(cider-test-failure-face ((t (:foreground ,*background* :background ,*error*))))
     `(cider-test-error-face ((t (:foreground ,*background* :background ,*warn*))))
     `(cider-test-success-face ((t (:foreground ,*background* :background ,*success*))))
     `(cider-result-overlay-face ((t (:background ,*background-1*))))

     ;; web
     ;; `(web-mode-html-tag-face ((t (:foreground ,*function*))))
     ;; `(web-mode-html-attr-name-face ((t (:foreground ,*variable*))))
     ;; `(web-mode-block-attr-name-face ((t (:foreground ,*variable*))))
     ;; `(web-mode-block-face ((t (:foreground ,*pink*))))

     ;; js2
     `(js2-external-variable ((t (:foreground ,*error*))))
     ;; `(js2-object-property ((t (:foreground ,*variable*))))

     ;; nxml
     `(nxml-glyph ((t (:foreground ,*background* :background ,*default-3*))))

     ;; erc
     ;; `(erc-notice-face ((t (:foreground ,*keywords*))))
     ;; `(erc-timestamp-face ((t (:foreground ,*type-face*))))
     ;; `(erc-current-nick-face ((t (:foreground ,*builtins*))))
     ;; `(erc-my-nick-face ((t (:foreground ,*string*))))
     `(erc-input-face ((t (:foreground ,*default*))))
     `(erc-nick-default-face ((t (:weight bold))))
     ;; `(erc-prompt-face ((t (:foreground ,*function*))))
     ;; `(erc-nick-msg-face ((t (:foreground ,*builtins*))))
     `(erc-direct-msg-face ((t (:foreground ,*default*))))
     `(erc-error-face ((t (:foreground ,*error*))))

     )

    (custom-theme-set-variables
     theme-name
     `(rainbow-delimiters-max-face-count 9))))

(provide 'matt-theme-make)

;;; matt-theme-make.el ends here
