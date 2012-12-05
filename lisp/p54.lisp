#|

Problem 54
10 October 2003

In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

    High Card: Highest value card.
    One Pair: Two cards of the same value.
    Two Pairs: Two different pairs.
    Three of a Kind: Three cards of the same value.
    Straight: All cards are consecutive values.
    Flush: All cards of the same suit.
    Full House: Three of a kind and a pair.
    Four of a Kind: Four cards of the same value.
    Straight Flush: All cards are consecutive values of same suit.
    Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

The cards are valued in the order:
2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.

Consider the following five hands dealt to two players:
Hand	 	Player 1	 	Player 2	 	Winner
1	 	5H 5C 6S 7S KD
Pair of Fives
	 	2C 3S 8S 8D TD
Pair of Eights
	 	Player 2
2	 	5D 8C 9S JS AC
Highest card Ace
	 	2C 5C 7D 8S QH
Highest card Queen
	 	Player 1
3	 	2D 9C AS AH AC
Three Aces
	 	3D 6D 7D TD QD
Flush with Diamonds
	 	Player 2
4	 	4D 6S 9H QH QC
Pair of Queens
Highest card Nine
	 	3D 6D 7H QD QS
Pair of Queens
Highest card Seven
	 	Player 1
5	 	2H 2D 4C 4D 4S
Full House
With Three Fours
	 	3C 3D 3S 9S 9D
Full House
with Three Threes
	 	Player 1

The file, poker.txt, contains one-thousand random hands dealt to two players. Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.

How many hands does Player 1 win?

|#

(defun p54 ()
  (let ((cards (read-hands "~/src/project-euler/poker.txt")))
    (loop for game in cards
       for hands = (deal game)
	 when (poker-beats (first hands) (second hands))
       count 1)))

(defun line->cards (line)
  (loop for i from 0 to 27 by 3 
       for j from 2 to 29 by 3
       collect (card-from-string (subseq line i j))))

(defun read-hands (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
	 while line
	 collect (line->cards line))))


#|
poker code follows
|#

;; avoid idiosyncracy in SBCL regarding constant redefinition
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))


(define-constant suits (list :diamonds :spades :clubs :hearts))
(define-constant values (list 2 3 4 5 6 7 8 9 10 :jack :queen :king :ace))

(defun parse-suit (char)
  "given a character, produce a symbol designating the appropriate suit"
  (cond 
    ((char= char #\D) :diamonds)
    ((char= char #\S) :spades)
    ((char= char #\C) :clubs)
    ((char= char #\H) :hearts)
    (t (error "expected one of D S C H, got ~S" char))))

(defun parse-value (char)
  "given a character, produce either a number or symbol for the face value"
  (if (alpha-char-p char)
      (cond 
	((char= char #\T) 10)
	((char= char #\J) :jack)
	((char= char #\Q) :queen)
	((char= char #\K) :king)
	((char= char #\A) :ace)
	(t (error "expected one of T J Q K A, got ~S" char)))
      (- (char-code char) (char-code #\0))))

(defun card-from-string (string)
  "given a two character string, produce a card, as a list of value, suit"
  (let ((value (char string 0))
	(suit (char string 1)))
    (list (parse-value value) (parse-suit suit))))
	
(defun card-value (card)
  "accessor for the value of the card."
  (first card))

(defun card-suit (card)
  "accessor for the suit of the card."
  (second card))

(defun deal (cards)
  "divide ten cards into two sets of five"
  ;; this corresponds to the way the file is formatted, 10 cards per line
  (list (subseq cards 0 5)
	  (subseq cards 5)))

(defun value->number (value)
  "given a value, if number, return it, otherwise give a numeric value to a face card."
  ;; used when sorting a hand
  (if (numberp value)
      value
      (case value
	(:jack 11)
	(:queen 12)
	(:king 13)
	(:ace 14))))

(defun number->value (number)
  "return a number to its symbolic face value"
  (cond ((< number 11) number)
	(t (case number
	  (11 :jack)
	  (12 :queen)
	  (13 :king)
	  (14 :ace)))))


(defun sort-values (hand)
  "sort hand by values"
  (sort (copy-tree hand)
	(lambda (c d) 
	  (< (value->number (card-value c))
	     (value->number (card-value d))))))

(defun hand->values (hand)
  "produce a list of numeric values for a hand of cards."
  ;; copy-tree avoids destructive sort
  (mapcar #'value->number
	  (mapcar #'card-value
		  (sort-values  hand))))

;;; winning hand detectors
;;; each of these with the exception of royal flush returns the value associated with ties
;;; for example, detect-full-house answers true, plus the face value of the triple
;;; detect-two-pair answers the higher of the two pairs


(defun detect-straight (hand)
  "determine if the hand is a straight."
  (let* ((v (hand->values hand))
	 (diff (map 'list (let ((last 0)) (lambda (c) (prog1 (- c last) (setf last c)))) v)))
    (if (every (lambda (d) (= 1 d)) (rest diff))
	(values T (last1 v))
	(values nil nil))))
       
(defun detect-flush (hand)
  "determine if the hand is a flush."
  (let ((suit (card-suit (first hand)))
	(high (last1 (hand->values hand))))
    (if (every (lambda (s) (eq s suit))
		 (mapcar #'card-suit (rest hand)))
      (values T high)
      (values nil nil))))
       
(defun detect-straight-flush (hand)
  (multiple-value-bind (straight high)
      	   (detect-straight hand)
  (if (and straight (detect-flush hand))
      (values T high)
      (values nil nil))))



(defun detect-royal-flush (hand)
  "it seems this is unnecessary, since det-str-flush captures this"
  (multiple-value-bind (sf high)
      (detect-straight-flush hand)
    (and sf (= (value->number :ace) high))))

(defun detect-four-of-a-kind (hand)
  (let* ((v (hand->values hand))
	 (counts (map 'list (lambda (c) (count c v)) v)))
    (if (member 4 counts)
	(values T (elt v (position 4 counts)))
	(values nil nil))))

(defun detect-full-house (hand)
  (multiple-value-bind (three v)
      (detect-three-of-a-kind hand)
    (if (and three (detect-pair hand))
	(values T v)
	(values nil nil))))


(defun detect-three-of-a-kind (hand)
  (let* ((v (hand->values hand))
	 (counts (map 'list (lambda (c) (count c v)) v)))
    (if (member 3 counts)
	(values T (elt v (position 3 counts)))
	(values nil nil))))

(defun detect-pair (hand)
  "return t if there is a pair, and the value of the pair, or two nils"
  (let* ((v (hand->values hand))
	 (counts (map 'list (lambda (c) (count c v)) v))
	 (pairs (member 2 counts)))
     (if pairs
	 (let* ((base (elt v (position 2 counts)))
		(next-high (apply #'max (remove base v))))
	   (values T  base (+ base (* next-high 1/100))))
	 (values nil nil nil))))
	 


(defun detect-two-pair (hand)
  (multiple-value-bind (pair? card1 card1+)
      (detect-pair hand)
    (if pair?
	(multiple-value-bind (two-pair? card2 card2+)
	    (detect-pair 
	     (remove-if (lambda (card) (eql (card-value card) (number->value card1))) hand))
	  (declare (ignorable card2))
	  (if two-pair?
	      (values T (max card1+ card2+))
	      (values nil nil)))
	(values nil nil))))

(defun high-card (hand)
  (last1 (hand->values hand)))

;; translate a hand into a number
(defparameter +pair-offset+ 15)
(defparameter +two-pair-offset+ 30)
(defparameter +three-of-a-kind-offset+ 45)
(defparameter +straight-offset+ 60)
(defparameter +flush-offset+ 75)
(defparameter +full-house-offset+ 90)
(defparameter +four-of-a-kind-offset+ 105)
(defparameter +straight-flush-offset+ 120)


(defun rank-hand (hand)
  "give a numerical value to hand, such that higher numbers win"
  (multiple-value-bind (sf high)
      (detect-straight-flush hand)
    (if sf 
	(+ high +straight-flush-offset+)
	(multiple-value-bind (four high)
	    (detect-four-of-a-kind hand)
	  (if four
	      (+ high +four-of-a-kind-offset+)
	      (multiple-value-bind (full high)
		  (detect-full-house hand)
		(if full
		    (+ high +four-of-a-kind-offset+)
		    (multiple-value-bind (flush high)
			(detect-flush hand)
		      (if flush
			  (+ high +flush-offset+)
			  (multiple-value-bind (straight high)
			      (detect-straight hand)
			    (if straight
				(+ high +straight-offset+)
				(multiple-value-bind (three high)
				    (detect-three-of-a-kind hand)
				  (if three
				      (+ high +three-of-a-kind-offset+)
				      (multiple-value-bind (two-pair high)
					  (detect-two-pair hand)
					(if two-pair
					    (+ high +two-pair-offset+)
					    (multiple-value-bind (pair high high+fraction)
						(detect-pair hand)
					      (declare (ignorable high))
					      (if pair
						  (+ high+fraction +pair-offset+)
						  (high-card hand))))))))))))))))))
  


(defun poker-beats (hand1 hand2)
  "true if hand1 beats hand2"
  (let ((r1 (rank-hand hand1))
	(r2 (rank-hand hand2)))
    (> r1 r2)))
