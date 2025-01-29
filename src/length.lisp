
;; リストの長さを計算するlengthの色々なパターンを書いてみる

;; dolistでlistをなめて数える
(defun length-1 (list)
  "return length of list."
  (let ((ret 0))
    (dolist (e list) (incf ret))
    ret))

;; 再帰
(defun length-2 (list)
  (if (null list)
      0
      (+ 1 (length-2 (rest list)))))

(defun length-3 (list &optional (sum 0))
  (if (null list)
      sum
      (length-3 (rest list) (+ 1 sum))))

(defun length-3-3 (list)
  (labels ((length-aux (list sum)
	     (if (null list)
		 sum
		 (length-aux (rest list) (+ 1 sum)))))
    (length-aux list 0)))

;; sequence
(defun length-4 (list)
  (count-if #'(lambda (x) (= x 1))
	    (mapcar #'(lambda (x) 1) list)))

(defun length-5 (list)
  (reduce #'+ (mapcar #'(lambda (x) 1) list)))

(defun length-6 (list)
  (reduce #'(lambda (x y) (+ x 1)) list :initial-value 0))

(defun length-7 (list)
  (count-if #'(lambda (x) (if x 1 0)) list))

;; loop

(defun length-8 (list)
  (loop for e in list
	count e))

(defun length-9 (list)
  (loop with value = 0
	for e in list
	while e
	do (incf value)
	finally (return value)))

(defun length-0 (list)
  0)

(defun length-test (length-fn list expect)
  (format t "~:[Fail~;Pass~] function: ~a input: ~a output: ~a~%"
	  (= (funcall length-fn list) expect)
	  length-fn
	  list
	  (funcall length-fn list)))

(defun length-test-all (length-fn)
  (let ((input-expect-values
	  '((() 0)
	    ((1) 1)
	    ((1 2) 2)
	    ((1 2 3) 3)
	    (nil 0))))
    (mapc #'(lambda (x) (length-test length-fn (get-input x) (get-expect x))) input-expect-values)))

(defun length-test-all-functions ()
  "apply length-test-all to all length functions."
  (mapc #'length-test-all
	'(length-0
	  length-1
	  length-2
	  length-3
	  length-4)))

(defun get-input (input-expect)
  "return input in list."
  (car input-expect))

(defun get-expect (input-expect)
  "return expect in list"
  (cadr input-expect))

