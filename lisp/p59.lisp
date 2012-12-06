#|
Problem 59
19 December 2003

Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code for Information Interchange). For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given value, taken from a secret key. The advantage with the XOR function is that using the same encryption key on the cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random bytes. The user would keep the encrypted message and the encryption key in different locations, and without both "halves", it is impossible to decrypt the message.

Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key. If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message. The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.

Your task has been made easy, as the encryption key consists of three lower case characters. Using cipher1.txt (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes, and the knowledge that the plain text must contain common English words, decrypt the message and find the sum of the ASCII values in the original text.

|#

(defun read-ciphertext ()
  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char #\, #\Space)
    (with-open-file (stream "~/src/project-euler/cipher1.txt")
      (loop for number = (read stream nil)
	   while number
	   collect number))))

;; codes for lowercase characters are between 97 and 122
;; code for e is 101
;; code for space is 32, we expect this often enough

(defun section (ciphertext initial step)
  (loop for i from 0
       for c in ciphertext
     when (= initial (mod i step))
       collect c))

(let ((ct (read-ciphertext)))
  (defun try (c1 c2 c3)
    (loop for i from 0
	 for j in ct
	 collect 
	 (cond ((= 0 (mod i 3)) (logxor c1 j))
		((= 1 (mod i 3)) (logxor c2 j))
			 ((= 2 (mod i 3)) (logxor c3 j))))))

(defun valid (char)
  (or (alpha-char-p char)
      (digit-char-p char)
      (member char (list #\Space #\. #\, #\;))))

(defun invalid (char)
  (member char (list #\Rubout #\`)))

(defun to-chars (ct)
  (mapcar #'code-char ct))


(defun all-trials ()
  (loop for i from 97 to 122
     append 
       (loop for j from 97 to 122
	  append  
	    (loop for k from 97 to 122
	       for trial = (to-chars (try i j k))
	       for tuple = (list trial i j k )
		 when (every (complement #'invalid) trial)
	       collect tuple))))


(apply #'+ (try 103 111 100))
#|
(let ((sorted (sort (all-trials)
		    (lambda (x y)
		      (< (count-if #'valid (first x))
			 (count-if #'valid (first y))))))
      (counter 0))
  (defun next-option ()
    (prog1 
	(elt sorted counter)
      (incf counter))))
|#
