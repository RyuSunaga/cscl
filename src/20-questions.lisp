;;; ゲーム20の質問を実装する

(defparameter *answer* nil "answer")

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
  "Items and their infomation of each category.")

(defparameter *categories*
  '((animal-p . (yes no))
    (color . (red gray yellow))
    (bigger-than-you . (yes no))
    (fly-p . (yes no))
    (eat-meat-p . (yes no)))
  "Categories and infomation"
  )

;; itemsの情報取得用のヘルパー関数
(defun item-and-infomation (name)
  "Return item and infomation"
  (assoc name *items*))

(defun item-name (name)
  "Return item name"
  (first (item-and-infomation name)))

(defun item-info (name)
  "Return item infomation list"
  (rest (item-and-infomation name)))


;; 回答候補を絞っていく
;; 方針：*items*のitemに紐づくinfomationを一つずつ消していく。全て無くなればそれが回答と言える

(defun question ()
  "Question for user."
  ;;
  ;;
  ;;

  
  )




(defun main ()

  ;; Player decide answer
  (format t "Hi! Please input the answer in this game. ~%>>")
  (setf *answer* (read-line))
  ;; TODO itemsにあるものしか入力できなくする
  (format t "Answer is ~a in this game.~%" *answer*)
  
  ;; 20 questions
  (dotimes (i 20)
    ;; Programm ask question or answer.

    ;; Player answer for the question.
    
    )

  ;; Show the answer. (Programm lose.)


  )

