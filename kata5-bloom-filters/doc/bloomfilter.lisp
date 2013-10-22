(defun sum-chars  (charseq)
  "Sum up the chars of a given string"
  ; this is essentially K&R "lose-lose" pretty bad hashing algorithm
  (reduce #'+ (map 'list #'char-code charseq)))

(defun djb-string-hash (charseq)
  "Use djb's method for hashing a string"
  (reduce #'(lambda (curhash charval)
	      (+ (ash curhash 5) curhash charval))
          (cons 5381 (map 'list #'char-code charseq))))

(defun comp-hash-val (curhash charval)
  (- 
   (+ charval
      (+ (ash curhash 16) 
	 (ash curhash 6)))
     curhash))

(defun sdbm-string-hash (charseq)
  "Use the method from sdbm for hashing a string"
  (reduce #'comp-hash-val
          (cons 0 (map 'list #'char-code charseq))))

; (defn sdbm-hash-recur 
;   "Recursive version of the sdbm hash"
;   ;; Compute hash(i) = hash(i - 1) * 65599 + str[i]; 
;   ([charseq] (sdbm-hash-recur charseq 0))
;   ([charseq ^long acc]
;      (if (empty? charseq)
;        acc
;        (recur (rest charseq) 
;               (unchecked-add (unchecked-multiply acc 65599) 
;                              (int (first charseq)))))))
  
; cf. http://www.isthe.com/chongo/tech/comp/fnv/
(defun fnv-hash (charseq)
  (let ((fnv-prime #x811C9DC5))
    (reduce #'(lambda (curhash charval)
              (logxor (* curhash fnv-prime) charval))
            (cons 0 (map 'list #'char-code charseq)))))

(defvar *hash-functions*
  (list #'sum-chars #'djb-string-hash 
        #'sdbm-string-hash #'fnv-hash))

(defun bloom-add (bloom charseq &key (hashfns *hash-functions*))
  (let ((size (length bloom)))
    (reduce #'(lambda (bloom value)
		(setf (aref bloom (mod value size)) 1)
		bloom)
	    (append (list bloom) 
		    (mapcar #'(lambda (func) 
				(funcall func charseq))
			    hashfns)))))

(defun bloom-contains-p (bloom charseq &key (hashfns *hash-functions*))
  (let ((size (length bloom)))
    (every #'(lambda (value)
	      (= 1 (aref bloom (mod value size))))
	  (mapcar #'(lambda (func) 
		   (funcall func charseq))
		  hashfns))))

(defun build-bloom (wordfile &key 
		    bloom (size 1024) 
		    (hashfns *hash-functions*))
  (let (lines)
    (with-open-file (wf wordfile :direction :input)
      (do ((line (read-line wf) (read-line wf nil 'eof)))
	  ((eq line 'eof) t)
	(setq lines (append (list line) lines))))
    (reduce #'(lambda (bloom charseq)
		(bloom-add bloom charseq :hashfns hashfns))
	    (cons (or bloom
		      (make-array size :element-type 'bit))
		  (reverse lines)))))

(defun optimal-size (capacity fault-rate)
  ;(int) Math.ceil(-1 * n * Math.log(p) / Math.pow(Math.log(2), 2))
  ;(ceiling (/ (* -1 capacity-inserts (log fault-rate)) (expt (log 2) 2))))
  (ceiling (* (log (/ 1 fault-rate)) (log (exp 1)) capacity)))