(setq input '(356261 846303))

(setq test-1 "111111")
(setq test-2 "234550")
(setq test-3 "123789")
(setq test-4 "112233")
(setq test-5 "123444")
(setq test-6 "111122")

(defun doubled (s)
  (let ((l (string-to-list s)))
    (if (equal (cl-first l) (cl-second l))
        (cl-first l)
      nil)))

(defun mor (a b)
  (or a b))

(defun check-adjacent-old (n-string)
  (thread-last '(0 1 2 3 4)
    (mapcar (lambda (x)
              (thread-first n-string
                (substring x (+ x 2))
                string-to-list
                doubled)))))

(defun check-adjacent (n-string)
  (thread-last n-string
    (seq-group-by #'identity)
    (seq-filter (lambda (group) (= 3 (length group))))))

(check-adjacent test-4)
(check-adjacent test-5)
(check-adjacent test-6)

(defun check-increasing (n-string)
  (thread-last n-string
    string-to-list
    (apply '<=)))

(check-increasing test-1)
(check-increasing test-2)
(check-increasing test-3)

(defun solve ()
  (setq n 356261)
  (setq crit 0)
  (while (<= n 846303)
    (let ((n-string (number-to-string n)))
      (when (and (check-adjacent n-string)
                 (check-increasing n-string))
        (setq crit (1+ crit))))
    (setq n (1+ n)))
  crit)

(print (solve))
