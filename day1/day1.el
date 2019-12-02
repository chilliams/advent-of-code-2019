(defun fuel-required (mass)
  (thread-first mass
    (/ 3)
    floor
    (- 2)))

(setq test-input '(12 14 1969 100756))

(mapcar 'fuel-required test-input)

(cl-reduce '+ (mapcar 'fuel-required test-input))

(defun load-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(thread-last (split-string (load-input "input") "\n" t)
  (mapcar 'string-to-number)
  (mapcar 'fuel-required)
  (cl-reduce '+))
