(in-package #:greed)

;;; Testing infrastructure

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~&~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;;; Tests
(deftest simple-values ()
  (check
    (=   0 (score nil))
    (=  50 (score '(5)))
    (= 100 (score '(1)))))

(deftest different-dices ()
  (check
    (= 300 (score '(1 5 5 1)))
    (=   0 (score '(2 3 4 6)))))

(deftest triples ()
  (check
    (= 1000 (score '(1 1 1)))
    (=  200 (score '(2 2 2)))
    (=  300 (score '(3 3 3)))
    (=  400 (score '(4 4 4)))
    (=  500 (score '(5 5 5)))
    (=  600 (score '(6 6 6)))))

(deftest mixed-sums ()
  (check
    (= 250 (score '(2 5 2 2 3)))
    (= 550 (score '(5 5 5 5)))
    (= 250 (score '(5 1 3 4 1)))
    (= 1100 (score '(1 1 1 3 1)))
    (= 450 (score '(2 4 4 5 4)))))

(deftest scoring? ()
  (check
    (= 5 (scoring-p '(1 1 1 1 1)))
    (= 4 (scoring-p '(1 1 1 5 2)))
    (= 3 (scoring-p '(1 1 1 2 4)))
    (= 3 (scoring-p '(5 5 5 4 6)))
    (= 3 (scoring-p '(2 2 2 4 6)))
    (= 2 (scoring-p '(1 5 2 4 6)))
    (= 1 (scoring-p '(1 2 4 4 6)))
    (= 0 (scoring-p '(2 2 4 4 6)))))

(deftest scoring-tests ()
  (combine-results
    (simple-values)
    (different-dices)
    (triples)
    (mixed-sums)
    (scoring?)))

(deftest test-create-dice-set ()
    ;; tests making an instance of the dice-set
    (check
      (typep (make-instance 'dice-set) 'dice-set)))

;; (start-debug :dice)
;; (undebug :dice)

(deftest test-rolling-the-dice-returns-a-set-of-integers-between-1-and-6 ()
    ;; tests rolling the dice
    (let ((dice (make-instance 'dice-set)))
      (roll 5 dice)
      (dbg :dice ";;;~&~{~D ~}~%" (get-values dice))
      (check
        (typep (get-values dice) 'list)
        (= 5 (length (get-values dice)))
        (>= 6 (reduce #'max (get-values dice)))
        (<= 1 (reduce #'min (get-values dice))))))

(deftest test-dice-values-do-not-change-unless-explicitly-rolled ()
    ;; tests that dice don't change just by looking at them
    (let ((dice (make-instance 'dice-set)))
      (roll 100 dice)
      (let ((first-time (get-values dice))
            (second-time (get-values dice)))
        (check
          (equal first-time second-time)))))

(deftest test-dice-values-should-change-between-rolls ()
    ;; tests that rolling the dice DOES change the values.
    (let ((dice (make-instance 'dice-set))
          (first-time nil)
          (second-time nil))
      (roll 100 dice)
      (setf first-time (get-values dice))
      (roll 100 dice)
      (setf second-time (get-values dice))
      (check
        (not (equal first-time second-time)))))

(deftest test-you-can-roll-different-numbers-of-dice ()
    ;; tests count parameter of how many dice to roll
    (let ((dice (make-instance 'dice-set)))
      (check
        (=  5 (length (roll 5 dice)))
        (= 100 (length (roll 100 dice)))
        (= 1 (length (roll 1 dice))))))

(deftest dice-tests ()
  (combine-results
    (test-create-dice-set)
    (test-rolling-the-dice-returns-a-set-of-integers-between-1-and-6)
    (test-dice-values-do-not-change-unless-explicitly-rolled)
    (test-dice-values-should-change-between-rolls)
    (test-you-can-roll-different-numbers-of-dice)))

(deftest greed-tests ()
  (combine-results
    (scoring-tests)
    (dice-tests)))

(greed-tests)

;;; Some test objects
(defvar *spiel* (make-instance 'game))
(defvar *spieler1* (make-instance 'player :name "Martin"))
(defvar *spieler2* (make-instance 'player :name "André"))
(defvar *dice* (make-instance 'dice-set))

(add-player *spiel* *spieler1*)
(add-player *spiel* *spieler2*)
