;;;; greed.lisp
;;;
;;; The main file of my little greed project. See README.txt for details.
;;;
;;; Time-stamp: <2017-01-23 20:53:24 Martin>
;;;
;;; ToDo
;;; DONE! 1.) Get rid of germen/english mix-up
;;; DONE! 2.) Implement the main game loop as a stub
;;; 3.) Determine how many dice are scoring and implement a decision for rolling again.
;;; 
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
		 (dbg :score "~&Die: ~D Counts: ~D~%" die counts)
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

;;; How many dice are actually scoring
(defun scoring-p (dice)
  "Returns the number of scoring dice, i.e. 1, 5 or a triple of 2, 4 or 6."
  (count-if #'(lambda (x) (or (= x 1) (= x 5) (and (= 3 (count x dice))))) dice))

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
(defmethod add-player ((game game) (player player))
  "A method to add new players to the game."
  (with-slots (players in-game-p-hash score-hash) game
    (setf players (append players (list player)))
    (setf (gethash player in-game-p-hash) nil)
    (setf (gethash player score-hash) 0)))

;;; Getting the current player
(defmethod get-current-player ((game game))
  "Returns the current player in GAME."
  (with-slots (players current-player-index) game
    (nth current-player-index players)))

;;; Prompting for user input
(defun ask-user (format-string &rest format-arguments)
  "A helper function to ask for the next player's name."
  (format *query-io* "~?: " format-string format-arguments)
  (finish-output *query-io*)
  (read-line *query-io*))

;;; Chechking who is already in the game, i.e. scored more than 300
;;; points.
(defmethod in-game-p ((player player) (game game))
  "A predicate to see which players are already in the game."
  (with-slots (in-game-p-hash) game
    (gethash player in-game-p-hash)))

;;; Switch to the next player.
(defmethod next-player ((game game))
  "Setting the slot current-player-index to the next value."
  (with-slots (current-player-index players) game
    (incf current-player-index)
    (if (= (length players) current-player-index) (setf current-player-index 0))))

;;; Getting the score of the current player
(defmethod get-score ((player player) (game game))
  "Returns the score of the current player."
  (with-slots (score-hash) game (gethash player score-hash)))

;;; Has the current player reached 3,000 points?
(defmethod end-game-p ((game game))
  "A predicate if the current player's score exceeds 3,000"
  (<= 3000 (get-score (get-current-player game) game)))

;;; Starting a new game, work in progress...
(defun new-game ()
  "Starts a new game, i.e. adds the players."
  (let ((current-game (make-game)))
    (format t "~3T New game, please enter at least two players:~%")
    (with-slots (players) current-game
      (loop :with player-count = 1
            :for player = (ask-user "Name of player #~D (RET to stop)" player-count)
            :until (and (zerop (length player)) (<= 2 (length players)))
            :if (plusp (length player))
            :do (add-player current-game (make-player player))
                (incf player-count))
      ;; Just looping over the current game until it is fully implemented.
      ;; Giving debugging information to check that the players were correctly
      ;; initialised.
      (loop :for player in players
            :for i :from 1
            :do (dbg :players "~&Player #~D ~A ~A ~A"
		     i (get-name player) (get-score player current-game)
		     (in-game-p player current-game)))
      (dbg :players "~&Current player: ~a" (get-name (get-current-player current-game)))
      (loop ;; :until (end-game-p current-game)
	    :with dice = (make-instance 'dice-set)
	    :repeat 1
	    :do (play-greed current-game dice)))))

(defgeneric play-greed (game dice-set))

;;; Playing the game, work in progres...
;;; Implement the decision taking part (ask user?) and the limits = 0, > 300...
;;; Update the hashes in the game object...
(defmethod play-greed ((game game) (dice dice-set))
  (with-slots (players in-game-p-hash score-hash) game
    (loop for player in players
	  do (loop :until (= 0 number-of-dice) 
		   :with number-of-dice = 5
		   :with cumulated-score = 0
		   :for list-of-dice = (roll number-of-dice dice)
		   :for score   = (score list-of-dice)
		   :for scoring = (scoring-p list-of-dice)
		   :do (decf number-of-dice scoring)
		       (incf cumulated-score score)
		       (dbg :players "~& ~A Dice: ~{~D ~} ~D ~D~%"
			   (get-name player) list-of-dice cumulated-score number-of-dice)))))

