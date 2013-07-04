(define range (lambda (start end)
		(if (= start end) (cons start '())
		    (cons start (range (+ 1 start) end)))))

(define where (lambda (f xs)
		(if (null? xs) xs
		    (if (f (car xs))
			(cons (car xs) (where f (cdr xs)))
			(where f (cdr xs))))))

(define take (lambda (wanted-elements xs)
	      (if (= wanted-elements 0) '()
		  (cons (car xs) (take (- wanted-elements 1) (cdr xs))))))

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

;###########################################################################
;1.1) 1.1 Write a 'range' function accepting two integers a and b, returning
;a list of all integers between a and b.  Include both a and be in
;the range.
;###########################################################################

(range 1 1000)
	
;###########################################################################
;1.2 Write a function called "take" that returns the first n elements
;of a list.
;###########################################################################

(take 3 '(1 2 3 4 5))

;###########################################################################
;1.4) Get a Prime:
;###########################################################################

(define prime? (lambda (x)
		 (letrec ((f (lambda (n)
			       (if (= n 1) #t
				   (if (= (modulo x n) 0) #f
				       (f (- n 1)))))))
		   (f (- x 1)))))

(prime? 5)
(prime? 4)

;###########################################################################
;1.5) Write a function called "multiply-tree" that 
;traverses a tree of integers, and returns the same tree with all values by two:
;###########################################################################

(define multiply-tree (lambda (xs)			
			(if (null? xs) '()
			    (if (pair? (car xs)) (cons (multiply-tree (car xs)) (multiply-tree (cdr xs)))
			        (cons (* 2 (car xs)) (multiply-tree (cdr xs)))))))

(map multiply-tree test-tree)

;###########################################################################
;2.1 Write a function called "select" that accepts a function and a
;list, applies the function to each element of the list, and returns
;all the results as a list.

;Example: 
;(define square (lambda (x) (* x x)))
;(select 'square '(1 2 3 4))
;=> (1 4 9 16)
;###########################################################################

(define select (lambda (f xs)
		 (if (null? xs) xs
		     (cons (f (car xs)) (select f (cdr xs))))))

(select square '(1 2 3 4))

;###########################################################################
;2.2 A predicate is a function that accepts one argument and returns
;true or false depending on the value of its argument.  An example would be
;a function to test the parity of an integer:

;(define even? (lambda (x) (= 0 (modulo x 2))))

;Write a function called "where" that accepts a predicate and a
;list and returns a list of all elements matched by the predicate.

;Example: (where 'even? '(1 2 3 4))
;       => (2 4)
;###########################################################################

(where even? '(1 2 3 4 5 6))

;###########################################################################
;2.3 Write a function called "take-while" that accepts a predicate and
;a list, and returns elements from this list as long as the predicate
;is true.
;###########################################################################

(let ((list '(1 3 5 7 8 10 11 12)))
  take-while odd? test-list)

;###########################################################################
;2.4 Write a function called "skip-while" that accepts a predicate and
;a list, and bypasses elements in this list as long as the predicate is
;true.

;Example: (skip-while 'even? '(1 3 5 7 8 10 11 12))
;       => (8 10 11 12)
;###########################################################################

(define skip-while (lambda (f xs)
		     (if (null? xs)
			 xs
			 (if (f (car xs))
			     (skip-while f (cdr xs))
			     xs))))

(let ((list '(1 3 5 7 8 10 11 12)))
  (skip-while even? test-list))

(let ((list '(1 3 5 7 8 10 11 12)))
  (skip-while odd? test-list))

;###########################################################################
;2.5 Write a function called "zip" that accepts two lists and merges
;them into one list.

;Example: (zip '(1 2 3) '(4 5 6 7 8))
;       => '((1 4) (2 5) (3 6))
;###########################################################################

(define zip (lambda (xs ys)
		   (if (or (null? xs) (null? ys)) '()
		      (cons (cons (car xs) (cons (car ys) '())) (zip (cdr xs) (cdr ys))))))

(zip '(1 2 3) '(4 5 6 7 8))

;###########################################################################
;2.6 Write a function called "fold" that recursively applies an
;operation to all elements in a list using a determined starting value.

;Example: (fold 'add 1 '(2 3 4 5))
;       => 15    ;; or 1 + 2 + 3 + 4 + 5
;###########################################################################

(define fold (lambda (f start xs)
	       (if (null? xs) start
		   (f start (fold f (car xs) (cdr xs))))))

(fold add 1 '(2 3 4 5))

;###########################################################################
;3.1 Solve Project Euler #1:
;"If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
;Find the sum of all the multiples of 3 or 5 below 1000.
;###########################################################################

(debug-set! stack 100000)

(let ((predicate (lambda (x) (or (= (modulo x 3) 0) (= (modulo x 5) 0)))))
  (where Divisible-by-3or5? (range 1 1000)))

;###########################################################################
;3.2 Write a function called "generate-tuples" that accepts a list of integers, and returns a list of all 2-tuples in this list:
; Example: (generate-tuples '(1 2 3))
; => ((1 2) (1 3) (2 3))

;###########################################################################

(define get-2-tuple (lambda (list)
	      (letrec ((head (car list))
		       (tail (cdr list)))
		       (map (lambda (n) (cons head (cons n '()))) tail))))

(define generate-tuples (lambda (list)
		 (if (null? list) '()
		     (append (get-2-tuple list) (generate-tuples (cdr list))))))

(generate-tuples '(1 2 3))

;###########################################################################
;3.3 Write a function called "generate-n-tuples" that accepts 
;a list of integers, and return a list of all n-tuples in this list.
;###########################################################################

(letrec ((first-tuple (lambda (n head tail)
			(cons head (take (- n 1) tail))))
	 (first-row (lambda (n head tail)
		      (let ((rest (skip (- n 1) tail)))
			(if (null? rest) (cons (cons head tail) '())
			    (cons (cons head (take (- n 1) tail)) (first-row n head (cdr tail)))))))
	 (n-tuples (lambda (n list)
		     (if (null? (skip n list)) (cons list '())
			 (cons (first-row n (car list) (cdr list)) (n-tuples n (cdr list)))))))
  (n-tuples 3 '(1 2 3 4 5 6)))

;###########################################################################
;3.4) Write a function that accepts two lists of integers and generates the cartesian product of these two lists:
;###########################################################################

(define cartesian (lambda (xs ys)
		    (letrec ((row-tuples (lambda (head tail)
					   (if (null? tail) '()
					       (cons (cons head (cons (car tail)'())) 
						     (row-tuples head (cdr tail)))))))
		      (if (null? xs) '()
			  (cons (row-tuples (car xs) ys) (cartesian (cdr xs) ys))))))

(cartesian test-list test-list)

;###########################################################################
;3.5) Solve Project Euler #6:

;The sum of the squares of the first ten natural numbers is,
;12 + 22 + ... + 102 = 385
;The square of the sum of the first ten natural numbers is,
;(1 + 2 + ... + 10)2 = 552 = 3025
;Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025  385 = 2640.
;Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
;###########################################################################

(define sum-squares (lambda (x y)
		      (let ((naturalNumbers (range x y)))
			(let ((squaredNaturalNumbers (map (lambda (x) (square x)) naturalNumbers)))
			  (let ((firstSum (fold add (car squaredNaturalNumbers) (cdr squaredNaturalNumbers)))
				(secondSum (square (fold add (car naturalNumbers) (cdr naturalNumbers)))))
			    (- secondSum firstSum))))))

(sum-squares 1 10)

;###########################################################################
;3.6 Write a function that transposes an integer matrix.

;Example: (transpose '((1 2)
;                  (3 4)
;                  (5 6)))
;=> ((1 3 5)
;    (2 4 6))
;###########################################################################

(define transpose (lambda (xs)
		    (if (any? null? xs) '()
			(letrec ((head (map (lambda (x) (car x)) xs))
				 (tail (map (lambda (y) (cdr y) ) xs)))
			  (cons head (transpose tail))))))

(transpose '((1 2) (3 4) (5 6)))

;###########################################################################
;3.6 Write a function that approximates the surface area under the
;curve of a numerical function for a given interval, by summing the
;areas of rectangles that fit under the curve.

;Example: 
;(define f (lambda x) (x))
;area 'f 0 1)
;=> .5

;https://www.khanacademy.org/math/calculus/integral-calculus/riemann-sums/v/simple-riemann-approximation-using-rectangles
;###########################################################################

;Right Hand Sum:
(define area (lambda (f x y)
	       (let ((width (/ (- y x) (+ y 1))))
		 (let ((list (map (lambda (n) (f n)) (range x y))))
		   (* (fold add (car list) (cdr list)) width)))))

(area (lambda (x) (* x x)) 1 5)
