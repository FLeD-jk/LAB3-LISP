(defun replace-at (lst idx new-elem)
  (cond
    ((null lst) nil)
    ((= idx 0) (cons new-elem (rest lst)))
    (t (cons (first lst) (replace-at (rest lst) (- idx 1) new-elem)))))

(defun shell-sort-rec (lst n gap i)
  (if (>= gap 1)
      (if (< i n)
          (let ((j i))
            (if (and (>= j gap) (> (nth (- j gap) lst) (nth j lst)))
                (shell-sort-rec (replace-at (replace-at lst j (nth (- j gap) lst)) 
                                             (- j gap) (nth j lst)) 
                                      n gap (- j gap))
              (shell-sort-rec lst n gap (+ i 1))))
        (shell-sort-rec lst n (floor (/ gap 2)) 0))  
    lst))

(defun shell-sorting-functional (lst)
  (let ((n (length lst)))
    (shell-sort-rec lst n (floor (/ n 2)) 0))

(defun check-shell-sorting-functional (name input expected)
  "Execute shell-sorting-functional on input, compare result with expected and print comparison status"
  (let ((result (shell-sorting-functional input))) 
    (format t "~:[~a failed! Expected: ~a Obtained: ~a~;~a passed! Expected: ~a Obtained: ~a~]~%"
            (equal result expected)
            name expected result)))


(defun test-shell-sorting-functional ()
 (format t "Start testing shell-sorting-functional function~%")
(check-shell-sorting-functional "test 1" '(346 23 0 32 44 76 2 120 34  32 65) '(0 2 23 32 32 34 44 65 76 120 346))
(check-shell-sorting-functional "test 2" '(0 0 2 56 78 21 34 90 6751 1 1 1 -1 1) '(-1 0 0 1 1 1 1 2 21 34 56 78 90 6751))
(check-shell-sorting-functional "test 3" '(3 4 2 9 34) '(9 2 3 4 34))
                       (format t "EnD~%"))

  
(test-shell-sorting-functional)
  
(defun generate-gaps (n)
  (if (< n 1)
      nil 
      (cons n (generate-gaps(floor n 2))))) 


(defun shell-sorting-imperative (lst)
  (let* ((my-copy-list (copy-list lst))
         (gaps (generate-gaps (length my-copy-list))))  
    (dolist (k gaps my-copy-list)      
      (loop for i from k below (length my-copy-list) do
        (let ((tmp (nth i my-copy-list))  
              (j i))
          (loop while (and (>= (- j k) 0) (> (nth (- j k) my-copy-list) tmp)) do
            (setf (nth j my-copy-list) (nth (- j k) my-copy-list))
            (setf j (- j k)))
          (setf (nth j my-copy-list) tmp)))) 
    my-copy-list))  


(defun check-shell-sorting-imperative (name input expected)
  "Execute shell-sorting-imperative on input, compare result with expected and print comparison status"
  (let ((result (shell-sorting-imperative input))) 
    (format t "~:[~a failed! Expected: ~a Obtained: ~a~;~a passed! Expected: ~a Obtained: ~a~]~%"
            (equal result expected)
            name expected result)))


(defun test-shell-sorting-imperative ()
 (format t "Start testing shell-sorting-imperative function~%")
(check-shell-sorting-imperative "test 1" '(346 23 0 32 44 76 2 120 34  32 65) '(0 2 23 32 32 34 44 65 76 120 346))
(check-shell-sorting-imperative "test 2" '(0 0 2 56 78 21 34 90 6751 1 1 1 -1 1) '(-1 0 0 1 1 1 1 2 21 34 56 78 90 6751))
(check-shell-sorting-imperative "test 3" '(3 4 2 9 34) '(9 2 3 4 34))
                       (format t "EnD~%"))

(test-shell-sorting-imperative)
