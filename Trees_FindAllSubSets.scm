(define range (lambda (start end)
		(if (= start end) (cons start '())
		    (cons start (range (+ 1 start) end)))))

(define where (lambda (f xs)
		(if (null? xs) xs
		    (if (f (car xs))
			(cons (car xs) (where f (cdr xs)))
			(where f (cdr xs))))))

(define take (lambda (wanted-elements xs)
	       (if (null? xs) '()
	      (if (= wanted-elements 0) '()
		  (cons (car xs) (take (- wanted-elements 1) (cdr xs)))))))

(define skip (lambda (unwanted-elements xs)
	       (if (= unwanted-elements 0) xs
		   (skip (- unwanted-elements 1) (cdr xs)))))

(define select (lambda (f xs)
		 (if (null? xs) xs
		     (cons (f (car xs)) (select f (cdr xs))))))
			 
(define take-while (lambda (f xs)
		     (if (null? xs)
			 xs
			 (if (f (car xs))
			     (cons (car xs) (take-while f (cdr xs)))
			     '()))))

(define skip-while (lambda (f xs)
		     (if (null? xs)
			 xs
			 (if (f (car xs))
			     (skip-while f (cdr xs))
			     xs))))

(define zip (lambda (xs ys)
		   (if (or (null? xs) (null? ys)) '()
		      (cons (cons (car xs) (cons (car ys) '())) (zip (cdr xs) (cdr ys))))))

(define fold (lambda (f start xs)
	       (if (null? xs) start
		   (f start (fold f (car xs) (cdr xs))))))

(define skip (lambda (n xs)
	       (if (or (= n 0) (null? xs)) xs
		   (skip (- n 1) (cdr xs)))))

(define (any? f lst) (not (null? (filter f lst))))


(define wait (lambda (current-seconds target-seconds)
	       (if (< current-seconds (* 1000 target-seconds))
		   (wait (+ current-seconds  1) target-seconds))))

(define square (lambda (x) (* x x)))

(define even? (lambda (x) (= 0 (modulo x 2))))

(define odd? (lambda (x) (= 1 (modulo x 2))))

(define add (lambda (x y) (+ x y)))



;####################################################################
; Find all possible sub-sets in a list of integers:
;####################################################################

;Function:
; Get the head, then the tail. Recurse on the tail:
;Should look like: ( 1 (2 3 4) ) 

(define test-list '(1 2 3 4 5 6 7))

(define get-permutations (lambda (n xs)
			   (if  (or (= 1 (length xs)) (< (length xs) n)) '()
				(if (= n 1) (car xs)
			    (let ( 
			      (next-sequence (cons (car xs) (skip 1 (cdr xs))))
			      )
;			      (display "### NEXT ###")
;			      (newline)
;			      (display "full list: ")
;			      (display xs)
;			      (newline)
;			      (display "next sequence: ")
;			      (display next-sequence)
;			      (newline)
;			      (display "rest: ")
;			      (display rest)
			      (cons (take n xs) (get-permutations n next-sequence))
				  ))))
  )

(get-permutations 7 test-list)


(define build-tuples (lambda (xs)
		       (letrec (
			     (tuples (lambda (n xs)
				       (if (= n 0) '()
					   (cons (get-permutations n xs) (tuples (- n 1) xs))
					   ))))
			 (tuples (length xs) xs))))


;(append (skip 6 test-list) (take 6 test-list))

;(build-tuples (append (skip 1 test-list) (take 1 test-list)))

(define get-subsets (lambda (xs)
		      (letrec (
			       (subsets (lambda (n ys)
					  (display xs)
					  (newline)
					  (display n)
					  (newline)
					  (if (= n 0) '()
					     (cons (build-tuples (append (skip n ys) (take n ys))) (subsets (- n 1) ys))
					     )
					  )
					)
			       )
			       (subsets (length xs) xs)
			)
		      )
  )

(get-subsets test-list)
					     
  

		     
