;1# Recursive Problems:

;1.1) 1.1 Write a 'range' function accepting two integers a and b, returning
;a list of all integers between a and b.  Include both a and be in
;the range.

(define range (lambda (start end)
		(if (= start end) (cons start '())
		    (cons start (range (+ 1 start) end)))))
(range 1 5)
	

;1.2 Write a function called "take" that returns the first n elements
;of a list.

(define take (lambda (wanted-elements xs)
	      (if (= wanted-elements 0) '()
		  (cons (car xs) (take (- wanted-elements 1) (cdr xs))))))

(take 3 '(1 2 3 4 5))




(define skip (lambda (unwanted-elements xs)
	       (if (= unwanted-elements 0) xs
		   (skip (- unwanted-elements 1) (cdr xs)))))

(skip 3 '(1 2 3 4 5))
		 


(define prime? (lambda (x)
		 (letrec (
			  (f (lambda (n)
			       (if (= n x) #t
				   (if (= (modulo x n) 0) #f
				       (f (+ n 1)))))))
		   (f 1))))
(prime? 5)
				      
				      
		 
		  
