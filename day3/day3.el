(setq test-1 "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83")
(setq test-2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

(defun load-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-instruction (instruction)
  (let ((direction (substring instruction 0 1))
        (distance (thread-first instruction
                    (substring 1)
                    string-to-number)))
    (list direction distance)))

(parse-instruction "R87")

(defun parse-line (line)
  (mapcar 'parse-instruction (split-string line "," t)))

(defun parse-input (input)
  (let ((lines (split-string input "\n" t)))
    (mapcar 'parse-line lines)))

(parse-input test-2)

(equal '(0 0) '(0 0))

(defun vadd (a b)
  (let ((ax (first a))
        (bx (first b))
        (ay (second a))
        (by (second b)))
    (list (+ ax bx) (+ ay by))))

(setq grid (make-hash-table :test 'equal))

(defun inchash (x grid)
  (let ((old (gethash x grid)))
    (if old
        (puthash x (1+ old) grid)
      (puthash x 1 grid))))

(inchash '(0 0) grid)
(gethash '(0 0) grid)

(defun move (position instruction grid)
  (let* ((direction (first instruction))
         (distance (second instruction))
         (x (first position))
         (y (second position)))
    (cond ((equal "R" direction)
           (dotimes (n distance)
             (let ((new-x (+ x n 1)))
               (setq cost (1+ cost))
               (when (not (gethash (list new-x y) grid))
                 (puthash (list new-x y) cost grid))))
           (list (+ x distance) y))

          ((equal "L" direction)
           (dotimes (n distance)
             (let ((new-x (- x n 1)))
               (setq cost (1+ cost))
               (when (not (gethash (list new-x y) grid))
                 (puthash (list new-x y) cost grid))))
           (list (- x distance) y))

          ((equal "D" direction)
           (dotimes (n distance)
             (let ((new-y (- y n 1)))
               (setq cost (1+ cost))
               (when (not (gethash (list x new-y) grid))
                 (puthash (list x new-y) cost grid))))
           (list x (- y distance)))

          ((equal "U" direction)
           (dotimes (n distance)
             (let ((new-y (+ y n 1)))
               (setq cost (1+ cost))
               (when (not (gethash (list x new-y) grid))
                 (puthash (list x new-y) cost grid))))
           (list x (+ y distance))))))

(defun follow-line (line grid)
  (let ((position '(0 0)))
    (setq cost 0)
    (dolist (instruction line)
      (setq position (move position instruction grid)))
    position))

(defun trace (input grid)
  (follow-line (first input) grid)
  (follow-line (second input) grid))

(follow-line (second (parse-input test-1)) grid)

(setq grid (make-hash-table :test 'equal))

(trace (parse-input test-2) grid)

(defun find-answer (input-string)
  (let* ((input (parse-input input-string)))
    (setq grid1 (make-hash-table :test 'equal))
    (setq grid2 (make-hash-table :test 'equal))
    (follow-line (first input) grid1)
    (follow-line (second input) grid2)
    (setq best-answer 0)
    (maphash (lambda (k cost1)
               (let ((cost2 (gethash k grid2)))
                 (when cost2
                   (let ((m-distance (+ cost1 cost2)))
                     (when (> m-distance 0)
                       (if (= best-answer 0)
                           (setq best-answer m-distance)
                         (setq best-answer (min m-distance best-answer))))))))
             grid1)
    best-answer))

(find-answer test-1)
(find-answer test-2)
(find-answer (load-file "input"))
