;;;; greed.lisp
;;;
;;; The main file of my little greed project. See README.txt for details.
;;;
;;; Time-stamp: <2017-01-26 21:29:02 Martin>
;;;
(in-package #:greed)

(defparameter *max-score* 1000 "The winning score")

;;; The generic functions
(defgeneric play-greed (game dice-set))
(defgeneric get-values (dice-set))
(defgeneric roll (x dice-set))
(defgeneric add-player (game player))
(defgeneric get-current-player (game))
(defgeneric next-player (game))
(defgeneric in-game-p (player game))
(defgeneric get-score (player game))
(defgeneric end-game-p (game))

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
   (score-hash :initform (make-hash-table) :accessor score-hash)))

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
  "A helper function to ask for some user's input."
  (format *query-io* "~?: " format-string format-arguments)
  (finish-output *query-io*)
  (read-line *query-io*))

;;; Switch to the next player.
(defmethod next-player ((game game))
  "Setting the slot current-player-index to the next value."
  (with-slots (current-player-index players) game
    (incf current-player-index)
    (if (= (length players) current-player-index) (setf current-player-index 0))))

;;; Chechking who is already in the game, i.e. scored more than 300
;;; points.
(defmethod in-game-p ((player player) (game game))
  "A predicate to see which players are already in the game."
  (with-slots (in-game-p-hash) game
    (gethash player in-game-p-hash)))

;;; Getting the score of the current player
(defmethod get-score ((player player) (game game))
  "Returns the score of the current player."
  (with-slots (score-hash) game (gethash player score-hash)))

;;; Has the current player reached *max-score*?
(defmethod end-game-p ((game game))
  "A predicate if the current player's score exceeds 3,000"
  (<= *max-score* (get-score (get-current-player game) game)))

;;; Starting a new game This function will ask for the players (at least two)
;;; and initialise the corresponding game object.  It will then call
;;; 'play-greed' until one player has reached *max-score* points.
;;; ToDo:
;;; * Test wether the game is ended correctly
(defun new-game ()
  "Starts a new game, i.e. adds the players."
  (let ((current-game (make-game))
	(dice (make-instance 'dice-set)))
    (format t "~3T New game, please enter at least two players:~%")
    (with-slots (players score-hash) current-game
      (loop :with player-count = 1
            :for player = (ask-user "Name of player #~D (RET to stop)" player-count)
            :until (and (zerop (length player)) (<= 2 (length players)))
            :if (plusp (length player))
            :do (add-player current-game (make-player player))
                (incf player-count))
      ;; Giving debugging information to check that the players were correctly
      ;; initialised.
      (loop :for player in players
            :for i :from 1
            :do (dbg :players "~&Player #~D ~A ~A ~A"
		     i (get-name player) (get-score player current-game)
		     (in-game-p player current-game)))
      (dbg :players "~&Current player: ~a" (get-name (get-current-player current-game)))
      (loop :do (play-greed current-game dice)
		(format t "~& ~A's total score: ~D~%"
			(get-name (get-current-player current-game))
			(gethash (get-current-player current-game) score-hash))
	    :until (end-game-p current-game)
	    :do (next-player current-game)
	    :finally (format t "~&The winner is: ~A with ~D points!"
			     (get-name (get-current-player current-game))
			     (gethash (get-current-player current-game) score-hash))))))

;;; Asking the player if he/she wants to continue.
(defun continue? ()
  "A helper function to ask for some user's input."
  (y-or-n-p "~&~3TContinue to roll the dice?"))

;;; Debugging information
;; (start-debug :players)
;; (undebug :players)

;;; Playing the game...
(defmethod play-greed ((game game) (dice dice-set))
  "Rolling the dice..."
  (with-slots (in-game-p-hash score-hash) game
   (loop :named play-greed
	 :with number-of-dice = 5
	 :with cumulated-score = 0
	 :with player = (get-current-player game)
	 :for list-of-dice = (roll number-of-dice dice)
	 :for score   = (score list-of-dice)
	 :for scoring = (scoring-p list-of-dice)
	 :do
	    (format t "~& ~A rolls: ~{~D ~} scoring: ~D cumulated in this turn: ~D~%"
		 (get-name player) list-of-dice score (+ score cumulated-score))
	    (cond ((zerop score)
		   (setf cumulated-score 0)
		   (return-from play-greed))
		  ((in-game-p player game)
		   (decf number-of-dice scoring)
		   (incf cumulated-score score))
		  ((and (<= 300 score) (not (gethash player in-game-p-hash)))
		   (format t "~&~5T~A is now in the game!" (get-name player))
		   (setf (gethash player in-game-p-hash) t)
		   (decf number-of-dice scoring)
		   (incf cumulated-score score))
		  (t (return-from play-greed)))
	 :while (and (continue?) (< 0 number-of-dice))
	 :finally (incf (gethash player score-hash) cumulated-score))))
