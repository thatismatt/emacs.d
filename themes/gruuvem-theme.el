(require 'matt-theme-make)

(deftheme gruuvem)

(matt-theme-make
 'gruuvem

 :background "#e6e9ee"
 :default    "#444"
 :cursor     "#999"
 :comment    "#666"
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
 )

(provide-theme 'gruuvem)

(provide 'gruuvem-theme)
