(defpackage #:greed
  (:use :cl)
  (:import-from :alexandria :with-gensyms))

(in-package #:greed)

;;; Debugging
;;; (start-debug :score)
;;; (undebug :score)

;;; The scoring function
(defun score (dice)
  "Returns the score of the DICE according to the rules of the greed game."
  (reduce #'+
          (mapcar
           #'(lambda (die)
               (let ((counts (count die dice)))
                 (case die
                   (1 (cond ((< counts 3) (* 100 counts))
                            ((= counts 3) 1000)
                                   ((> counts 3) (+ 1000 (* 100 (- counts 3))))))
                   ((2 3 4 6) (cond ((= counts 3) (* 100 die))
                                    (t 0)))
                   (5 (cond ((< counts 3) (* 50 counts))
                            ((= counts 3) 500)
                            ((> counts 3) (+ 500 (* 50 (- counts 3))))))
                   (otherwise 0))))
           (remove-duplicates dice))))

;;; The class dice-set
(defclass dice-set ()
  ((value :reader get-value :writer roll :initform ())))

(defmethod get-values ((object dice-set))
  (slot-value object 'value))

(defmethod roll (how-many (object dice-set))
  (let ((dice-list))
    (dotimes (counter how-many)
      (push (+ 1 (random 6)) dice-list))
    (setf (slot-value object 'value) dice-list)))

;;; We will define the game as a class with the following slots
;;; 1.) A list of all players as index, i.e. (0 1 2 ...)
;;; 2.) The index of the current player, running from 0 to n-1
;;; 3.) A hash of the status of players
;;; 4.) A hash of the scores for all players
(defclass game ()
  ((players :reader get-player :initform '())
   (current-player-index :initform 0)
   (in-game-p-hash :initform (make-hash-table))
   (score-hash :initform (make-hash-table))))

(defun make-game ()
  "Returning a new instance of the game class."
  (make-instance 'game))

;;; The player class holding the name of the players
(defclass player ()
  ((name :reader get-name :initarg :name)))

(defun make-player (name)
  "Returning a new instance of the player class."
  (make-instance 'player :name name))

;;; Adding players to the game.
(defmethod mitspielen ((game game) (player player))
  "A method to add new players to the game."
  (with-slots (players in-game-p-hash score-hash) game
    (setf players (append players (list player)))
    (setf (gethash player in-game-p-hash) nil)
    (setf (gethash player score-hash) 0)))

(defun ask-user (format-string &rest format-arguments)
  (format *query-io* "~&~?: " format-string format-arguments)
  (finish-output *query-io*)
  (read-line *query-io*))

;;; Chechking who is already in the game, i.e. scored more than 300
;;; points.
(defmethod in-game-p ((player player) (game game))
  "A predicate to see which players are already in the game."
  (with-slots (in-game-p-hash) game
    (gethash player in-game-p-hash)))


(defmethod account-dice ((game game) (dice dice-set))
  (with-slots (in-game-p-hash score-hash) game
    (let ((player (get-current-player game))
          (score (score dice)))
      (if (and (not (in-game-p player game)) (>= score 300))
          (progn
            (setf (gethash player in-game-p-hash) t)
            (format t "Player ~a is now in the game~%" (get-name player))))
      (if (in-game-p player game)
          (progn (incf (gethash player score-hash) score)
                 (format t "Player ~a now has ~a points~%"
                         (get-name player)
                         (get-score player game)))))))

(defmethod next-player ((game game))
  (with-slots (current-player-index players) game
    (incf current-player-index)
    (if (= (length players) current-player-index) (setf current-player-index 0))))

(defmethod get-score ((player player) (game game))
  (with-slots (score-hash) game (gethash player score-hash)))

(defmethod end-game-p ((game game))
  (<= 3000 (get-score (get-current-player game) game)))


(defun neues-spiel ()
  (let ((spiel (make-game)))
    (format t "~&~3T Neues Spiel, bitte mindestens zwei Spieler eingeben~%")
    (with-slots (players in-game-p-hash score-hash) spiel
      (loop :with player-count = 1
            :for player = (ask-user "Name of player #~D (RET to stop)" player-count)
            :until (and (zerop (length player)) (<= 2 (length players)))
            :if (plusp (length player))
            :do (mitspielen spiel (make-player player))
                (incf player-count))
      (loop :for player in players
            :for i :from 1
            :do (format t "~&Player #~D ~A ~A ~A"
			i (get-name player) (get-score player spiel) (in-game-p player spiel))))))



;; (defmethod start ((game game))
;;   (with-slots (players) game
;;     (if (< (length players) 2) (error (make-condition 'not-enough-players)))
;;     (set-current-player-index 0 game)
;;     (loop while (not (eq :end-game (tick game))))))

;; (defmethod tick ((game game))
;;   (let ((dice (make-dice-set))
;;         (current-player (get-current-player game)))
;;     (roll '(0 1 2 3 4) dice)
;;     (format t "Player ~a rolled ~a. Score ~a~%"
;;             (get-name current-player)
;;             (get-dice-set-rolls dice)
;;             (score dice))
;;     (roll (decide current-player dice) dice)
;;     (account-dice game dice)
;;     (if (end-game-p game) :end-game (next-player game))))

      
    




