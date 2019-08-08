;; xorbit-theme.el -- Matt Lee's dark theme

;;; Commentary:

;;; Code:

(require 'matt-theme-make)

(deftheme xorbit)

(setq xorbit-theme-hue 0.57)

(matt-theme-make
 'xorbit

 :background (color-hsl-to-hex xorbit-theme-hue 0.15 0.05) ;; "#0ae10d1b0eb8"
 :default    (color-hsl-to-hex xorbit-theme-hue 0.15 0.70) ;; "#a7adb50abeb7"
 :cursor     "#999"
 :comment    (color-hsl-to-hex xorbit-theme-hue 0.10 0.50) ;; "#7332820b8ccc"
 :success    "#091"
 :warn       "#e70"
 :error      "#c00"
 ;; builtins, constants, clojure keywords
 :a1         (color-hsl-to-hex 0.78 1.0 0.60) ;; "#be763332ffff"
 ;; functions, preprocessors, variables
 :b1         (color-hsl-to-hex 0.55 1.0 0.48) ;; "#0000ac07f5c1"
 ;; types, numbers
 :c1         (color-hsl-to-hex 0.61 1.0 0.60) ;; "#333278d4ffff"
 ;; keywords
 :d1         (color-hsl-to-hex 0.71 1.0 0.67) ;; "#82f75709fffe"
 ;; strings
 :e1         (color-hsl-to-hex 0.44 1.0 0.38) ;; "#0000c28e7c84"
 )

(provide-theme 'xorbit)

(provide 'xorbit-theme)

;;; xorbit-theme.el ends here
