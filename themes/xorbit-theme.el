(require 'matt-theme-make)

(deftheme xorbit)

(let ((theme-hue 0.57))
  (matt-theme-make
   'xorbit

   :background (color-hsl-to-hex theme-hue 0.15 0.05)
   :default    (color-hsl-to-hex theme-hue 0.15 0.7)
   :cursor     "#999"
   :comment    (color-hsl-to-hex theme-hue 0.1 0.5)
   :success    "#0a2"
   :warn       "#f91"
   :error      (color-hsl-to-hex (+ theme-hue 0.5) 1.0 0.5)
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
   ))

(provide-theme 'xorbit)

(provide 'xorbit-theme)
