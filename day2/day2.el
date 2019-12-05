(defun load-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (thread-last (split-string input-string ",")
    (mapcar 'string-to-number)))

(setq day2-input (thread-first "input"
                   load-input
                   parse-input))

(setq day2-test-input "1,0,0,3,99")


(defun opcode (n)
  (cond ((= n 1) '+)
        ((= n 2) '*)
        ((= n 99) "finish")
        (t "bad")))

(first '(4 2 3 1))
(second '(4 2 3 1))
(third '(4 2 3 1))
(fourth '(4 2 3 9))

(defun get-op (n table)
  (gethash (* n 4) table))
(defun get-apos (n table)
  (gethash (+ 1 (* n 4)) table))
(defun get-bpos (n table)
  (gethash (+ 2 (* n 4)) table))
(defun get-out (n table)
  (gethash (+ 3 (* n 4)) table))

(defun lookup-fn (table)
  (lambda (n)
    (gethash n table)))

(defvar result nil)

(defun run (program)
  (setq result nil)
  (let ((table (make-hash-table))
        (i 0))
    (dolist (n program)
      (puthash i n table)
      (setq i (1+ i)))
    (dotimes (i (/ (length program) 4.0))
      (when (eq result nil)
        (let ((op (opcode (get-op i table))))
          (cond ((equal op "finish")
                 (setq result (gethash 0 table)))
                ((equal op "bad")
                 (setq result '("bad opcode")))
                (t (let* ((apos (get-apos i table))
                          (a (gethash apos table))
                          (bpos (get-bpos i table))
                          (b (gethash bpos table))
                          (slot (get-out i table)))
                     (puthash slot (funcall op a b) table))))))))
  result)


(run (parse-input "1,9,10,3,2,3,11,0,99,30,40,50"))
(run (parse-input "1,0,0,0,99"))
(run (parse-input "2,3,0,3,99"))
(run (parse-input "2,4,4,5,99,0"))
(run (parse-input "1,1,1,4,99,5,6,0,99"))
(run (parse-input (load-input "input")))

(cons 99 (cdr (cdr (cdr (parse-input (load-input "input"))))))

(defun run2 (input)
  (let ((chomped (cdr (cdr (cdr input)))))
    (dotimes (x 100)
      (dotimes (y 100)
        (let* ((program (thread-last chomped
                          (cons y)
                          (cons x)
                          (cons 1)))
               (result (run program)))
          (when (= result 19690720)
            (print program)))))))

(run2 (parse-input (load-input "input")))
