;;; mortimer-tests --- mortimer tests  -*- lexical-binding: t -*-

;;; Code:

(require 'mortimer)

(defun mortimer-reduced-log-equal (l1 l2)
  "Test two reduced logs, L1 and L2, for equality."
  (seq-every-p
   (lambda (i)
     (let ((e1 (elt l1 i))
           (e2 (elt l2 i)))
       (seq-every-p
        (lambda (k)
          (equal (gethash k e1)
                 (gethash k e2)))
        '(:start :complete :status))))
   ;; TODO: test all items, not just first
   '(0)))

(defun ht (&rest kvs)
  "Build a hash-table from KVS."
  (map-into kvs 'hash-table))

(ert-deftest mortimer-group-log-test ()
  (should (equal '()
                  (mortimer-reduce-log '())))
  (should (mortimer-reduced-log-equal
           `(,(ht :start '(:id :start :time 1)
                  :status :start))
           (mortimer-reduce-log '((:id :start :time 1)))))
  (should (mortimer-reduced-log-equal
           `(,(ht :start    '(:id :start :time 1)
                  :complete '(:id :complete :time 2)
                  :status   :complete)
             ,(ht :start    '(:id :start :time 3)
                  :complete '(:id :complete :time 4)
                  :status   :complete))
           (mortimer-reduce-log
            '((:id :start    :time 1)
              (:id :complete :time 2)
              (:id :start    :time 3)
              (:id :complete :time 4)))))
  (should (mortimer-reduced-log-equal
           `(,(ht :start    '(:id :start    :time 1)
                  :complete '(:id :complete :time 2)
                  :success  '(:id :success  :time 3)
                  :status   :success))
           (mortimer-reduce-log
            '((:id :start    :time 1)
              (:id :complete :time 2)
              (:id :success  :time 3)))))
  (should (mortimer-reduced-log-equal
           `(,(ht :start    '(:id :start    :time 1)
                  :complete '(:id :complete :time 2)
                  :success  '(:id :success  :time 5 :time-ref 1)
                  :status   :success)
             ,(ht :start    '(:id :start :time 3)
                  :complete '(:id :complete :time 4)
                  :status   :complete))
           (mortimer-reduce-log
            '((:id :start    :time 1)
              (:id :complete :time 2)
              (:id :start    :time 3)
              (:id :complete :time 4)
              (:id :success  :time 5 :time-ref 1)))))
  (should (mortimer-reduced-log-equal
           `(,(ht :start    '(:id :start :time 1)
                  :stop     '(:id :stop  :time 2)
                  :status   :incomplete)
             ,(ht :start    '(:id :start :time 3)
                  :status   :start))
           (mortimer-reduce-log
            '((:id :start :time 1)
              (:id :stop  :time 2)
              (:id :start :time 3))))))

(when
nil

(ert t)

(ert-run-tests t (lambda (event-type &rest event-args)
                   (message "%s %s" event-type event-args)))

(define-key emacs-lisp-mode-map (kbd "C-c t t") '(lambda () (interactive) (ert t)))

;; TEST CASES
;; - marked fail THEN marked success -> success
;; - starting a timer while another timer is paused -> paused timer should be incomplete
;; - paused then resumed -> ongoing
;; - convert to a test: (mapcar (lambda (s) (list s (mortimer-duration-friendly-string s))) (list 1 10 60 61 3600 3601 3660))

(let* ((now (current-time))
       (1h-ago (time-subtract now (seconds-to-time (* 60 60)))))
  (setq mortimer-log
        (list (list :time 1h-ago :id :stop :args nil)
              (list :time 1h-ago :id :start :args '("5 mins")))))

(mortimer-clear-log)

(reverse mortimer-log)
((:time (25472 40615 626804 234000) :id :start  :args (1500))
 (:time (25472 40622 290491 337000) :id :pause  :args nil)
 (:time (25472 40625 431137 124000) :id :resume :args nil)
 (:time (25472 40628 715940 370000) :id :stop   :args nil))

((:time (25468 32218  54922 252000) :id :start    :args (1500))
 (:time (25468 33718  65550  18000) :id :complete :args nil)
 (:time (25468 39129 995125 981000) :id :start    :args (1500))
 (:time (25468 40629 997986 171000) :id :complete :args nil)
 (:time (25468 40641 416241  52000) :id :start    :args (1500))
 (:time (25468 40655 490194 215000) :id :stop     :args nil)
 (:time (25468 40656 959932 759000) :id :start    :args (1500))
 (:time (25468 42156 962915 450000) :id :complete :args nil)
 (:time (25469 59690 919786 878000) :id :success  :args nil)
 (:time (25469 59694 606690 924000) :id :start    :args (1500))
 (:time (25469 61194 616778 179000) :id :complete :args nil)
 (:time (25469 61360 583671 685000) :id :fail     :args nil)
 (:time (25469 61370 103913 532000) :id :start    :args (1500))
 (:time (25469 62870 106578 104000) :id :complete :args nil))

;; ((:time (25444 62601 290580 114000) :id :complete :args nil)
;;  (:time (25444 61101 287658 672000) :id :start :args ("25 mins"))
;;  (:time (25444 61098 662135 518000) :id :success :args nil)
;;  (:time (25444 61092 718018 799000) :id :complete :args nil)
;;  (:time (25444 59592 715747 288000) :id :start :args ("25 mins")))

;; (ert-deftest foo ()
;;   ;; (should (equal 1 1))
;;   )

;; (ert-set-test 'bar
;;               (make-ert-test :name 'bar :body (lambda () (should (equal 2 2)))
;;                              ;; :file-name "/home/matt/.config/emacs/lisp/mortimer-tests.el"
;;                              ))
;; (ert-delete-test 'bar)

)

(provide 'mortimer-tests)

;;; mortimer-tests.el ends here
