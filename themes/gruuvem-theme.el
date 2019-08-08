;; gruuvem-theme.el -- Matt Lee's light theme

;;; Commentary:

;;; Code:

(require 'matt-theme-make)

(deftheme gruuvem)

(let ((theme-hue 0.6))
  (matt-theme-make
   'gruuvem

   :background (color-hsl-to-hex theme-hue 0.20 0.90) ;; "#e146e520eb84"
   :default    (color-hsl-to-hex theme-hue 0.20 0.10) ;; "#147a18541eb8"
   :cursor     (color-hsl-to-hex theme-hue 0.20 0.25) ;; "#39993e694666"
   :comment    (color-hsl-to-hex theme-hue 0.20 0.30) ;; "#73327cd28ccc"
   :success    "#071"
   :warn       "#b80"
   :error      "#b10"
   ;; builtins, constants, clojure keywords
   :a1         "#419"
   ;; functions, preprocessors
   :b1         "#939"
   ;; types, numbers
   :c1         "#00b"
   ;; keywords, variables
   :d1         "#057"
   ;; strings
   :e1         "#071"
   ))

(provide-theme 'gruuvem)

(provide 'gruuvem-theme)

;;; gruuvem-theme.el ends here
