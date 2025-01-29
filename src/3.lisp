
;;3.3
(defun dot-print (x)
  "Print x as dot expression."
  (cond
    ((atom x)
     (princ x))
    ((listp x)
     (princ "(")
     (princ (first x))
     (princ " . ")
     (dot-print (rest x))
     (princ ")"))))

;;3.4
(defun dot-print-v2 (x)
  "Print x as dot-expression if x is cons sel."
  (cond
    ((or (null (rest x)) (listp (rest x)))
     (print x))
    ((consp x)
     (dot-print x))))

;; 試したいパターン
(defun test-fn ()
  "Apply fn1 and f2 to each value of list and Compare the results"
  (let ((values
	  (list
	   (cons 1 1)
	   (cons 1 '(2))
	   (cons 1 '(2 3))
	   (cons 1 nil)
	   (cons nil nil)
	   '()
	   '(1)
	   '(1 2)
	   '(1 nil)
	   '(nil nil))))

    ;; 戻り値がない＆副作用があるのでmapcarではなくdolistで処理をする
    (dolist (x values) (dot-print-and-print x))
    ))

(defun dot-print-and-print (x)
  "Apply dot-print-v2 and print to x."
  (dot-print-v2 x)
  (print x))

