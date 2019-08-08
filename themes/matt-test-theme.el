
(require 'matt-theme-make)

(deftheme matt-test)

(let ((theme-hue 0.6))

  (matt-theme-make
   'matt-test

   :background (color-hsl-to-hex theme-hue 0.6 0.06) ;; "#06240d841893"
   :default    (color-hsl-to-hex theme-hue 0.4 0.75) ;; "#a665bae0d998"
   :cursor     (color-hsl-to-hex theme-hue 0.5 0.50) ;; "#3fff7332bfff"
   :comment    (color-hsl-to-hex theme-hue 0.2 0.50) ;; "#66667ae09999"
   :success    (color-hsl-to-hex 0.37 1.00 0.30) ;; "#0000999921ca"
   :warn       (color-hsl-to-hex 0.17 1.00 0.30) ;; "#968699990000"
   :error      (color-hsl-to-hex 0.92 1.00 0.40) ;; "#cccc0000624d"
   :a1         (color-hsl-to-hex 0.77 0.84 0.55) ;; "#a4052c08ed90"
   :b1         (color-hsl-to-hex 0.58 0.70 0.50) ;; "#26668394d998"
   :c1         (color-hsl-to-hex 0.52 0.70 0.45) ;; "#228fb07bc3d6"
   :d1         (color-hsl-to-hex 0.85 0.70 0.45) ;; "#c3d6228fb3b5"
   :e1         (color-hsl-to-hex 0.43 0.83 0.40) ;; "#1168bb6373ff"
   ))

(when nil ;; debug ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq theme-hue 0.6)
  (makunbound 'theme-hue)

  (let ((theme-hue 0.65))
    (deftheme matt-test-2)
    (matt-theme-make
     'matt-test-2
     :background (color-hsl-to-hex theme-hue 0.6 0.10) ;; "#0a3d168728f5"
     :default    (color-hsl-to-hex theme-hue 0.4 0.75) ;; "#a665bae0d998"
     :cursor     (color-hsl-to-hex theme-hue 0.5 0.50) ;; "#3fff7332bfff"
     :comment    (color-hsl-to-hex theme-hue 0.2 0.50) ;; "#66667ae09999"
     :success    (color-hsl-to-hex 0.37 1.00 0.30) ;; "#0000999921ca"
     :warn       (color-hsl-to-hex 0.17 1.00 0.30) ;; "#978c99993332"
     :error      (color-hsl-to-hex 0.92 1.00 0.40) ;; "#cccc0000624d"
     :a1         (color-hsl-to-hex (+ theme-hue 0.1) 0.8 0.6)
     :b1         (color-hsl-to-hex (+ theme-hue 0.2) 0.8 0.6)
     :c1         (color-hsl-to-hex (+ theme-hue 0.3) 0.8 0.6)
     :d1         (color-hsl-to-hex (- theme-hue 0.1) 0.8 0.6)
     :e1         (color-hsl-to-hex (- theme-hue 0.2) 0.8 0.6)
     )
    (matt-disable-all-themes)
    (enable-theme 'matt-test-2))

  (matt-disable-all-themes)

  custom-enabled-themes

  ;; colour palettes

  (color-hsl-to-hex 0.71 0.6 0.5)
  "#5b223332cccc"
  "#428f0000ffff"

  (color-hsl-to-hex (mod (+ (/ 1.0 3) 0.71) 1) 0.6 0.5)
  "#cccc5b223332"
  "#ffff428f0000"

  (color-hsl-to-hex (mod (+ (/ 2.0 3) 0.71) 1) 0.6 0.5)
  "#3332cccc5b22"
  "#0000ffff428f"

  (color-complement-hex "#5b223332cccc")
  "#a4dccccd3332"

  (color-hsl-to-hex (mod (+ 0.5 0.71) 1) 0.6 0.5)
  "#a4dccccc3332"

  (let* ((d-min 0.0)
         (num 12)
         (dh (/ 1.0 num)))
    (mapcar
     (lambda (i)
       (color-hsl-to-hex (mod (+ d-min (* dh i)) 1) 1.0 0.5))
     (number-sequence 0 (1- num))))

  ("#ffff00000000" "#ffff7fff0000" "#fffeffff0000" "#7fffffff0000" "#0000ffff0000" "#0000ffff7fff" "#0000fffeffff" "#00007fffffff" "#00000000ffff" "#7fff0000ffff" "#fffe0000ffff" "#ffff00007fff")

  (defun color-hue-spread (hue-mid num hue-range)
    (let ((hue-delta (/ hue-range 1.0 (1- num)))
          (hue-min   (mod (- hue-mid (/ hue-range 1.0 2.0)) 1.0)))
      (mapcar
       (lambda (i)
         (mod (+ hue-min (* hue-delta i)) 1))
       (number-sequence 0 (1- num)))))

  (thread-last (color-hue-spread 0 3 (/ 2.0 3))
    (seq-mapcat (lambda (h) (color-hue-spread h 3 0.1)))
    (seq-map (lambda (h) (color-hsl-to-hex h 1.0 0.5))))
  ("#00004cccffff" "#00000000ffff" "#4ccc0000ffff"
   "#ffff00004ccc" "#ffff00000000" "#ffff4ccc0000"
   "#4cccffff0000" "#0000ffff0000" "#0000ffff4ccc")


  (color-hsl-to-hex 0.43 1.0 0.5)
  "#0000ffff947a"

  (mapcar
   (lambda (h)
     (destructuring-bind
         (L a b) (apply 'color-srgb-to-lab
                        (color-hsl-to-rgb h 1 0.5))
       (mapcar
        (lambda (L)
          (list (color-hsl-to-hex h 1 0.5)
                (apply 'color-rgb-to-hex
                       (mapcar 'color-clamp
                               (color-lab-to-srgb L a b)))))
        ;; '(40 50 60)
        ;; (number-sequence 0 160 20)
        '(60))))
   ;; (number-sequence 0.84 0.4 -0.07)
   ;; '(0.84 0.73 0.62 0.51 0.4)
   ;; '(0.43)
   ;; '(0.77 0.58 0.52 0.85 0.43)
   (number-sequence 0 1 (/ 1.0 12)))
  ((("#ffff00000000" "#ffff304c175a"))
   (("#ffff7fff0000" "#e9466d8f0000"))
   (("#fffeffff0000" "#8ea1992c0000"))
   (("#7fffffff0000" "#0000aa1c0000"))
   (("#0000ffff0000" "#0000aeea0000"))
   (("#0000ffff7fff" "#0000ad1734f7"))
   (("#0000fffeffff" "#0000a72ba8bf"))
   (("#00007fffffff" "#339e8dc5ffff"))
   (("#00000000ffff" "#95b15d08ffff"))
   (("#7fff0000ffff" "#bd8f50a6ffff"))
   (("#fffe0000ffff" "#fefa0000ff0a"))
   (("#ffff00007fff" "#ffff2a5b8d27"))
   (("#ffff00000000" "#ffff304c175a")))



  (("#a1ef00000000" "#f4e000000000" "#ffff667e3f61")
   ("#7d540c0c0000" "#c8ef537f0000" "#ffff95ad2522")
   ("#3a1241d70000" "#723e7f3e0000" "#bba6c17e0000")
   ("#00004c350000" "#00008e9a0000" "#4d33d4880000")
   ("#00004e7f0000" "#000092bc0000" "#0000da3c0000")
   ("#00004de90000" "#0000911e18ae" "#0000d82e5c44")
   ("#00004b6b500f" "#00008bfe8e40" "#0000d13ad1f6")
   ("#00003b29ab2e" "#00007499f282" "#7265b550ffff")
   ("#00000000ea33" "#70c8411effff" "#c931868bffff")
   ("#483b0000d14e" "#9daf304fffff" "#ed2f7cccffff")
   ("#932900009b76" "#dfbb0000e1ca" "#ffff56f6ffff")
   ("#a0a0000038f1" "#f041000073b4" "#ffff644cb4fe"))

  (("#ae7a2348ffff") ("#0000762cee74") ("#00008882a57a") ("#e41d0000ccda") ("#0000908a3128"))
  (("#eceb67cfffff") ("#5a76a99dffff") ("#0000beb4dcca") ("#ffff47f8ffff") ("#0000c8f2633b"))

  (((0 "#000022810000") (20 "#000041530000") (40 "#0000754a17b1") (60 "#0000ac6f49f7") (80 "#0000e60c7d2c") (100 "#51acffffb330") (120 "#9aebffffebe3") (140 "#dc72ffffffff") (160 "#ffffffffffff")))

  (("#c2b20000bd19" "#e18d0000d97b" "#ffff03d7f67e")
   ("#8f470000f152" "#ae7a2348ffff" "#cda04891ffff")
   ("#52841f00ffff" "#7a2f3eccffff" "#9d935b39ffff")
   ("#1e9f3d54ffff" "#57265617ffff" "#7e046f9affff")
   ("#00006569b9b8" "#00007e1fd5e0" "#000097d7f2af")
   ("#000072386894" "#00008cce81b5" "#0000a81d9bc4")
   ("#000075a90000" "#000090fb1fc7" "#0000acef3a7e"))

  (("#c2b20000bd19" "#e18d0000d97b" "#ffff03d7f67e")
   ("#6adb11f7ffff" "#8d93384cffff" "#aeaa564fffff")
   ("#000045a9fd70" "#49205dbeffff" "#71e976ddffff")
   ("#0000702d7fb5" "#00008a5a99c6" "#0000a54eb4ab")
   ("#000076470000" "#000091b60000" "#0000adc21038"))

  (mapcar
   (lambda (h)
     (color-hsl-to-hex h 1 0.5))
   '(0.84 0.73 0.62 0.51 0.4))
  ("#ffff0000f5c1" "#61470000ffff" "#000047adffff" "#0000f0a2ffff" "#0000ffff6666")
  ("#e18d0000d97b" "#8d93384cffff" "#49205dbeffff" "#00008a5a99c6" "#000091b60000")


(let ((a '(1 2 3)))
  (setf (car a) 4)
  a)

  ((lambda (hsl) hsl) (list 1 2 3))

(46.59393433510748 75.51260937537963 -73.46137527018672)

  :a1         (color-hsl-to-hex 0.77 0.84 0.55) ;; "#a4052c08ed90"
  :b1         (color-hsl-to-hex 0.58 0.70 0.50) ;; "#26668394d998"
  :c1         (color-hsl-to-hex 0.52 0.70 0.45) ;; "#228fb07bc3d6"
  :d1         (color-hsl-to-hex 0.85 0.70 0.45) ;; "#c3d6228fb3b5"
  :e1         (color-hsl-to-hex 0.43 0.83 0.40) ;; "#1168bb6373ff"

  0.43
  0.52 9
  0.58 6
  0.77 9
  0.85 7

  (/ (- 85 43) 4.0)

  )

(provide-theme 'matt-test)

(provide 'matt-test-theme)

;;; matt-test-theme.el ends here
