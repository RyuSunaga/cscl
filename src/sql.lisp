;; SQLを自動生成するプログラムを書いてみる

;; SQLの文法定義
(defparameter *simple-sql*
  '((query -> (select column* from table where*))
    (column* -> * (column column**))	;; column*は1つ以上選択する
    (column** -> nil (column column**))
    (where* -> nil (where column condition))
    
    (condition -> (number-condition number))
    (number-condition -> = < <= > >=)
    (number -> -10 0 10)
    
    (column -> info-of-table) ;; column1, column2 column3 が欲しい
    (table -> info-of-table))
  "Simple SQL grammer and user-tables"
  )

(defparameter *initialized-table-p* nil "initialized table flag for generating query.")

(defparameter *sample-table1*
  '((table -> table1)
    (column -> column1 column2 column3))
  "Ssample table")

(defparameter *sample-table2*
  '((table -> tableA)
    (column -> columnA columnB columnC))
  "Ssample table")

(defparameter *tables* '(*sample-table1* *sample-table2*) "Tables used for generating random query.")


(defparameter *target-table* *sample-table1* "table used by generating random query.")

(defgeneric info-of-table (category)
  (:documentation "Return info from table"))

(defmethod info-of-table ((category (eql 'column)))
  "Return a list of columns from table info."
  (rule-rhs (assoc-rule-for-target-table category)))

(defmethod info-of-table ((category (eql 'table)))
  "Return a table-name from table info."
  (rule-rhs (assoc-rule-for-target-table category)))

;; table-utility
(defun init-target-table ()
  "Init target table at random."
  (setf *target-table* (random-elt *tables*))
  (setf *initialized-table-p* t))

(defun reset-init-target-table-flag ()
  "Reset target table at random."
  (setf *target-table* nil)
  (setf *initialized-table-p* nil))

(defun assoc-rule-for-target-table (category)
  "Assoc for getting target table."
  (assoc category (eval *target-table*)))

;; utility
(defun mappend (fn the-list)
  "apply fn to each element of the list, and append the results."
  (apply #'append (mapcar fn the-list)))

(defun random-elt (the-list)
  "return random element of the list."
  (elt the-list (random (length the-list))))

;; helper関数 文法規則に対するデータの取得と書き換えを定義する
(defun rule-lhs (rule)
  "Return left-hand side of rule."
  (first rule))

(defun rule-rhs (rule)
  "Return right-hand sise of rule."
  (rest (rest rule)))

(defun get-rewrite-rule (category)
  "Return rule left-hand side coresspont to rule"
  (rule-rhs (assoc category *simple-sql*)))


;; 処理全体の流れ
;;; 1. 対象のカテゴリーを与えられる
;;; 2. カテゴリーがリストなら全て生成する必要があるので処理をリストの各要素に適用する
;;; 3. リストでないなら他のカテゴリーに書き換え可能かどうか確認する
;;;; 3-1. 書き換え可能なら書き換えたカテゴリーで再生成
;;;; 3-2. 書き換えできないならそれをそのまま出力する（末端のため）

(defun generate-with-random (ph)
  "Generate random query from ph at random."
  (let ((ret nil))
    
    ;; random set value for table used by generating query
    (unless *initialized-table-p*
      (init-target-table))
    ;; generate query
    (setf ret (generate ph))
    ;; reset 
    (reset-init-target-table-flag)
    ret))

(defun generate (ph)
  "Generate random query from ph."
  (let ((rewrite-ph nil))
    (cond
      ((listp ph)
       ;; apply generate to each element in ph list.
       (mappend #'generate ph))
      ((setf rewrite-ph (get-rewrite-rule ph))
       (ecase (first rewrite-ph)
	 ('info-of-table (generate (random-elt (info-of-table ph))))
	 (t (generate (random-elt (get-rewrite-rule ph))))))
      (t (list ph)))))

