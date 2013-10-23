While trying to figure out the bit vector implementation, I quickly threw together a Common Lisp implementation, which is more or less a verbatim translation of the Clojure code I had into CL. The CL code is actually somewhat more verbose and, of course, uses mutable data structures and assignments. E.g., this is what the `bloom-add` function looks like:

    (defun bloom-add (bloom charseq &key (hashfns \*hash-functions\*))
      (let ((size (length bloom)))
        (reduce #'(lambda (bloom value)
	  	            (setf (aref bloom (mod value size)) 1)
		        bloom)
	     (append (list bloom) (hash-string charseq :hashfns hashfns)))))


    (defun build-bloom (wordfile &key bloom (size 1024) 
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

This is not as polished as the final Clojure code I ended up with (from a style point of view), but it does the job. And it does it's job particular well, in particular when compared with the Clojure version:

	CL-USER> (time (progn (build-bloom "/usr/share/dict/words" :size 1000000) t))
	Evaluation took:
		0.511 seconds of real time
		0.516032 seconds of total run time (0.480030 user, 0.036002 system)
		[ Run times consist of 0.072 seconds GC time, and 0.445 seconds non-GC time. ]
		100.98% CPU
		1,446,984,809 processor cycles
		283,097,152 bytes consed
	T

	kata5-bloom-filters.core> (time (do (build-bloom-synced "/usr/share/dict/words"
		                                                    :bloom-filter (make-bloom-vector 1000000))
	                                    true))
	"Elapsed time: 4954.553481 msecs"
	true

That's an entire magnitude the still non-optimized CL code is faster. Let's dig in a little further: profiling the CL code shows where the time is spent:

       seconds  |     gc     |    consed   |  calls  |  sec/call  |  name  
    -----------------------------------------------------------
          0.995 |      0.108 | 544,287,056 | 198,342 |   0.000005 | BLOOM-ADD
          0.399 |      0.040 | 249,718,688 |  99,171 |   0.000004 | HASH-STRING
          0.061 |      0.004 |  18,500,560 | 297,518 |   0.000000 | READ-LINE
    -----------------------------------------------------------
          1.456 |      0.152 | 812,506,304 | 595,031 |            | Total

    estimated total profiling overhead: 1.11 seconds
    overhead estimation parameters:
    0.0s/call, 1.872e-6s total profiling, 8.96e-7s internal profiling
    T

Most time is spent in `bloom-add`. Here's the same analysis for the Clojure code:

    kata5-bloom-filters.core> (binding [p/*enable-profiling* true]
				   (p/profile (do (build-bloom-synced "/usr/share/dict/words"
								      :bloom-filter (make-bloom-vector 1000000))
						  true)))
      Name      mean       min       max     count       sum
       add     16751      9409  44385793     99171  1661214496
      hash       440       375    158421     99171    43728669
     slurp  17352028  17352028  17352028         1    17352028
     nil

We see similar behavior in general, but the time spent in `bloom-add` is considerably greater.

