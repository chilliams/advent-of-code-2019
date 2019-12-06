(require 'cl)

(setq example-input "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")

(defun parse-input (input)
  (thread-last (split-string input "\n" t)
    (seq-map (lambda (s) (split-string s ")" t)))))

(setq example-alist (parse-input example-input))

(defun get-orbiters (x l)
  (thread-last l
    (seq-filter (lambda (p) (equal (first p) x)))
    (seq-map 'second)))

(defun orbit-count (x value l)
  (let ((sum value))
    (dolist (orbiter (get-orbiters x l))
      (let ((i (orbit-count orbiter (1+ value) l)))
        (setq sum (+ sum i))))
    sum))

(orbit-count "COM" 0 example-alist)

(defun load-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(setq max-specpdl-size 13000)
(setq max-lisp-eval-depth 10000)

(orbit-count "COM" 0 (parse-input (load-input "input")))

(defun get-transfers (x l)
  (thread-last l
    (seq-filter (lambda (p) (or (equal (first p) x)
                                (equal (second p) x))))
    (seq-map (lambda (p) (if (equal (first p) x)
                             (second p)
                           (first p))))))

(setq example-2 "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")

(defun explore (santa-map l)
  (let ((s nil))
    (setq s (cons nil '("YOU" 1)))
    (while s
      (let* ((v (cdr s))
             (planet (first v))
             (transfer-count (second v))
             (current-value (gethash planet santa-map most-positive-fixnum)))
        (setq s (car s))
        (when (> current-value transfer-count)
          (puthash planet transfer-count santa-map)
          (dolist (transfer (get-transfers planet l))
            (setq s (cons s (list transfer (1+ transfer-count))))))))))

(defun find-santa (input)
  (let ((santa-map (make-hash-table :test 'equal)))
    (explore santa-map input)
    (- (gethash "SAN" santa-map) 3)))

(find-santa (parse-input example-2))

(find-santa (parse-input (load-input "input")))
