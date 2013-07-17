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
					     ))))
			       (subsets (length xs) xs))))

(get-subsets test-list)
		


;####################################################################
; Example from Stanford:
;####################################################################

(define test-list '(1 2 3 4 5 6 7))
			     
 ;First Example:
(define powerset (lambda (set)
  (if (null? set) '(())
      (append (powerset (cdr set))
	      (map (lambda (subset)
		     (cons (car set) subset))
		   (powerset (cdr set)))))))

(powerset '(1 2 3))

;Example w/ the Use of Let:		     
(define let-powerset (lambda (set)
		       (display "set in root lambda: ")
		       (display set)
		       (newline)
		       (if (null? set) '(())
			   (let ((ps-rest (let-powerset (cdr set))))
			     (append ps-rest (map (lambda (subset)
						    (cons (car set) subset))
						  ps-rest))
			     ))))

(let-powerset '(1 2 3))


;####################################################################
; Does the node with a certain value exist?:
;####################################################################

;Helped via this Stack Overflow Question:
;http://stackoverflow.com/questions/17600460/scheme-tree-puzzle-expressing-an-empty-node-in-a-final-pair

(define value-exist? (lambda (n xs)
			(if (null? xs) #f
			    (if (not (pair? xs))
				(= n xs)
				(or (value-exist? n (car xs)) (value-exist? n (cdr xs)))
				))))


(value-exist? 4 '(1 2 3 (3 2 4)))

;####################################################################
; Verify that a Binary Search Tree is in order:
;-- The left subtree of a node contains only nodes with keys less than the nod's key.
;-- The right subtree of a node contains only nodes with keys greater than the nod's key.
;-- Both the left and right subtrees must also be binary search trees.
;####################################################################

;Research: 
;http://www.geeksforgeeks.org/a-program-to-check-if-a-binary-tree-is-bst-or-not/


(define leaf '())
(define leaf? null?)
(define (tree value left right) (list left right value))

(define test-tree
  (tree 5
	(tree 3
	      (tree 1 leaf leaf)
	      (tree 7 leaf leaf)
	      )
	(tree 8
	      (tree 6 leaf leaf)
	      (tree 10 leaf leaf)
	      )))


(define node-value (lambda (node) (car (skip 2 node))))
(define left-child (lambda (node) (car (take 1 node))))
(define right-child (lambda (node) (car (skip 1 node))))


(define valid-bst? (lambda (root f xs)
		     (if (null? xs) #t
			 (letrec (
				  (node-value (car (skip 2 xs)))
				  (left-child (car (take 1 xs)))
				  (right-child (car (skip 1 xs)))
				  )
			   (if (and (f root node-value) (valid-bst? root f left-child) (valid-bst? root f right-child))
			   #t
			   #f)
			 ))))


;(valid-bst? 10 > tree1)


(define is-bst-valid? (lambda (xs)
  (if (null? xs) #t
      (letrec (
	       (node-value (car (skip 2 xs)))
	       (left-child (car (take 1 xs)))
	       (right-child (car (skip 1 xs)))
	       )
	(if (and (valid-bst? node-value > left-child) (valid-bst? node-value < right-child))
	    (if (and (is-bst-valid? left-child) (is-bst-valid? right-child))
		#t
		#f)
	    #f)
	)
      )
  )
  )
  

(is-bst-valid? test-tree)
 
			     

		     
