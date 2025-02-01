;;; ゲーム20の質問を実装する
(defparameter *answer* nil "answer")
(defparameter *item-table* (make-hash-table) "hash table for item and infomation")
(defparameter *items* nil)
(defparameter *categories* nil)

(defvar *simple-items*
  `((elephant . ((animal-p . yes)
		 (color . gray)
		 (bigger-than-you . yes)
		 (fly-p . no)
		 (eat-meat-p . no)))
    (apple . ((animal-p . no)
	      (color . red)
	      (bigger-than-you . no)
	      (fly-p . no)
	      (eat-meat-p . no))))
  "Items and their infomation. These are data for *item-table*.")

(defvar *simple-categories*
  '((animal-p . (yes no))
    (color . (red gray yellow))
    (bigger-than-you . (yes no))
    (fly-p . (yes no))
    (eat-meat-p . (yes no)))
  "Categories and infomation")

(defun init-item-table ()
  "Init table for 20 questions."
  ;; Init Category
  (setf *categories* (copy-alist *simple-categories*))
  ;; Init Item
  (setf *items* (copy-alist *simple-items*))
  
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

(defun item-info-value (item-name info-key)
  (gethash info-key (gethash-item-info item-name)))

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

;; Helpler for *categories*
(defun get-category (category-name)
  (assoc category-name *categories*))

(defun category-name (category)
  (first category))

(defun category-values (category)
  (rest category))

(defun delete-category-value (category-name value)
  "Delete value from category-name."
  (delete value (assoc category-name *categories*)))


(defun question (&optional (answer nil))
  "Question for user and return multiple values within category-name and value."

  ;; If input answer, question about answer.
  (if answer
      (format t "Answer is ~a ?~%" answer)
      ;; categoryの先頭から確認する
      (let* ((candidate-category (one-of-candidate-category))
	     (category-name (category-name candidate-category))
	     ;; 回答用のvalueを一つ選択
	     (category-value (car (category-values candidate-category))))

	(format t "~a is ~a ?~%" category-name category-value)
	(values category-name category-value))))

(defun one-of-candidate-category ()
  "Return one of category for answer."
  ;; カテゴリーに紐づく属性の数が存在していれば回答候補のカテゴリー
  (find-if #'(lambda (x) (category-values x)) *categories*))

;; helper for game.
(defun init-game-settings ()
  "Init game settings"
  ;; init *answer*
  (setf *answer* nil)
  ;; Init *item-table* and set infomation from *items*.
  (init-item-table))

(defun read-line-as-symbol ()
  (intern (string-upcase (read-line))))

(defun read-answer-from-player ()
  "Player decide answer in this game with prompt."
  (loop
    until *answer*
    do
       (format t "Hi! Please input the answer in this game. ~%>>")
       (setf *answer* (read-line-as-symbol))
       (if (gethash-item-info *answer*)
	   (format t "Answer is ~a in this game.~%" *answer*)
	   (setf *answer* nil))))

(defun read-yes-or-not-from-player ()
  (format t "yes or no >> ")
  (let ((answer (read-line-as-symbol)))
    (if (member answer '(yes no))
	answer
	(read-yes-or-not-from-player))))

(defun answer-p (item)
  "Check the item is answer or not."
  (if (= (count-item-info item) 0) t nil))

(defun search-answer ()
  "Search answer in *item-table*, and return answer if exsists."
  (maphash (lambda (item _) (when (answer-p item) (return-from search-answer item))) *item-table*))

(defun true-answer-p (answer)
  "Check the answer is correct or not."
  (if (eq answer *answer*) t nil))

(defun main ()
  "Main for 20 Questions."

  (init-game-settings)

  ;; show table for user.
  (show-item-table)
  
  ;; set answer in this game.
  (read-answer-from-player)
  
  ;; 20 questions
  (dotimes (i 20)

    (let ((answer (search-answer)))
      (cond
	;; 回答が確定していない場合
	((null answer)
	 ;; Question about category.
	 (multiple-value-bind (category-name value) (question)
	   ;; Player answer for the question.
	   (when (eq (read-yes-or-not-from-player) 'yes)
	     ;; *item-table*から該当の要素を全て消したい (e.q. 'animal-p 'yes の要素を持つinfoを全て消す
	     (maphash (lambda (item _)
			(when (eq (item-info-value item category-name) value)
			  (format t "DELETE ~a ~a ~a%" item category-name value)
			  ;; *item-tableから該当の要素を消す*
			  (delete-item-info item category-name)))
		      *item-table*))
	   ;; 今回の質問をできないようにカテゴリーを更新する
	   (delete-category-value category-name value)))
	
	;; 回答が確定している場合
	(t
	 (question answer)
	 (when (eq (read-yes-or-not-from-player) 'yes)
	   (format t "Programm is Win!!~%")
	   (return-from main)))))
    ;; debuggin
    (show-item-table))
  
  ;; Show the answer.
  (format t "Player is WIN!!~% Answer is ~a~%" *answer*))
