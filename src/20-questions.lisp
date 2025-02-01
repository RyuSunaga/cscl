;;; ゲーム20の質問を実装する

(defparameter *answer* nil "answer")
(defparameter *item-table* (make-hash-table) "hash table for item and infomation")

(defparameter *items*
  `((elephant . ((animal-p . yes)
		 (color . gray)
		 (bigger-than-you . yes)
		 (fly-p . yes)
		 (eat-meat-p . yes)))
    (apple . ((animal-p . no)
	      (color . red)
	      (bigger-than-you . no)
	      (fly-p . no)
	      (eat-meat-p . no))))
  "Items and their infomation. These are data for *item-table*.")

(defparameter *categories*
  '((animal-p . (yes no))
    (color . (red gray yellow))
    (bigger-than-you . (yes no))
    (fly-p . (yes no))
    (eat-meat-p . (yes no)))
  "Categories and infomation")

(defun init-item-table ()
  "Init table for 20 questions."

  ;; Init table.
  (setf *item-table* (make-hash-table))

  ;; Check item-and-info-list from *items* and insert into *item-table*.
  (mapc (lambda (x) (insert-item-and-info-list (item-name x) (info-list x))) *items*))

(defun insert-item-and-info-list (item info-list)
  "Insert a new item and infomation to *item-table* ."
  (set-item item)
  (mapc (lambda (x) (set-item-info item (info-category x) (info-value x))) info-list))

;; Helper for *item-table*
(defun set-item (item)
  (setf (gethash item *item-table*) (make-hash-table)))

(defun gethash-item-info (item)
  (gethash item *item-table*))

(defun set-item-info (item-name key value)
  (setf (gethash key (gethash-item-info item-name)) value))

(defun show-item-table ()
  "Show item table for debugging."
  (format t "===== Item Table =====~%")
  (maphash (lambda (item item-info-table)
	     (format t "item: ~a~%" item)
	     (maphash (lambda (info value) (format t "info: ~a value: ~a~%" info value))
		      item-info-table)
	     (format t "-------------------~%"))
	   *item-table*))

;; Helper for *items*
(defun info-category (info)
  "Return a category from infomation."
  (first info))

(defun info-value (info)
  "Return a category from infomation."
  (rest info))

(defun item-name (item-and-info-list)
  "Return item name"
  (first item-and-info-list))

(defun info-list (item-and-info-list)
  "Return item infomation list"
  (rest item-and-info-list))

(defun delete-item-info (item info-key)
  "Delete key from item info."
  (remhash info-key (gethash-item-info item)))

(defun count-item-info (item)
  (hash-table-count (gethash item *item-table*)))

(defun answer-p (item)
  "Check the item is answer or not."
  (if (= (count-item-info item) 0) t nil))

;; 回答候補を絞っていく
;; 方針：*items*のitemに紐づくinfomationを一つずつ消していく。全て無くなればそれが回答と言える

(defun question ()
  "Question for user."
  ;; categoryの先頭から確認する
  ;; categoryに紐づく要素を確認
  ;; itemsの対応するカテゴリーの要素を確認して一致していれば削除
  ;; itemの各要素を確認して要素数が0になっていれば全て一致するということなのでOK
  )

;; helper for game.

(defun read-line-as-symbol ()
  (intern (string-upcase (read-line))))

(defun true-answer-p (answer)
  "Check the answer is correct or not."
  (if (eq answer *answer*) t nil))

(defun main ()

  ;; init *answer*
  (setf *answer* nil)
  
  ;; Init *item-table* and set infomation from *items*.
  (init-item-table)
  (show-item-table)

  ;; Player decide answer
  (loop
    until *answer*
    do
       (format t "Hi! Please input the answer in this game. ~%>>")
       (setf *answer* (read-line-as-symbol))
       (if (gethash-item-info *answer*)
	   (format t "Answer is ~a in this game.~%" *answer*)
	   (setf *answer* nil)))
  
  ;; 20 questions
  (dotimes (i 20)
    ;; Programm ask question or answer.

    ;; Player answer for the question.

    ;; TODO: questoinの回答で分岐
    ;; 質問した属性がyesなら：全リストでその要素を持つものを削除する
    ;; 質問した属性がnoなら：カテゴリーの情報を削除
    ;; カテゴリーの情報が1つになってしまったら：全リストでその情報を持つものを削除する
    
    )

  ;; Show the answer. (Programm lose.)

  )

