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

(define all? (lambda (f xs) (= (length xs) (length (filter f  xs)))))


(define wait (lambda (current-seconds target-seconds)
	       (if (< current-seconds (* 1000 target-seconds))
		   (wait (+ current-seconds  1) target-seconds))))

(define square (lambda (x) (* x x)))

(define even? (lambda (x) (= 0 (modulo x 2))))

(define odd? (lambda (x) (= 1 (modulo x 2))))

(define add (lambda (x y) (+ x y)))

(define assert-true (lambda (f x) (if (f x) #t (error "assert-true failed")))) 

(define assert-false (lambda (f x) (if (not (f x)) #t (error "assert-false failed")))) 

(define count (lambda (f xs)
		(if (null? xs) 0
		    (if (f (car xs))
			(+ 1 (count f (cdr xs)))
			(+ 0 (count f (cdr xs)))))))


(define return (lambda (n xs) 
		 (if (null? xs) (error "Out of Bounds of List")
		 (if (= n 0) (car xs)
		     (return (- n 1) (cdr xs))))))


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

(define valid-bst
  (tree 4
	(tree 2
	      (tree 1 leaf leaf)
	      (tree 3 leaf leaf)
	      )
	(tree 6
	      (tree 5 leaf leaf)
	      (tree 7 leaf leaf)
	      )))


(define invalid-bst
  (tree 4
	(tree 2
	      (tree 1 leaf leaf)
	      (tree 3 leaf leaf)
	      )
	(tree 6
	      (tree 1 leaf leaf)
	      (tree 7 leaf leaf)
	      )))


(define node-value (lambda (node) (car (skip 2 node))))
(define left-child (lambda (node) (car (take 1 node))))
(define right-child (lambda (node) (car (skip 1 node))))


(define valid-bst? (lambda (root f xs)
		     (if (null? xs) #t
			   (and (f root (node-value xs)) (valid-bst? root f (left-child xs)) (valid-bst? root f (right-child xs))))
			 ))


(valid-bst? 10 > tree1)


(define is-bst-valid? (lambda (xs)
			(if (null? xs) #t
			    (if (and (valid-bst? (node-value xs) > (left-child xs)) (valid-bst? (node-value xs) < (right-child xs)))
				(and (is-bst-valid? (left-child xs)) (is-bst-valid? (right-child xs))) 
				#f
				))))
  

(assert-true is-bst-valid? valid-bst)

(assert-false is-bst-valid? invalid-bst)

;####################################################################
;2-3-4 Tree:
;Providing a List to Perform a Top Down Insertion to Build a 2-3-4 Tree
;####################################################################


;Lets make a data structure:

(define value '())


(define leaf '())
(define leaf? null?)
(define (234-tree value1 value2 value3 leaf1 leaf2 leaf3 leaf4) (list value1 value2 value3 leaf1 leaf2 leaf3 leaf4))




(define 0-tree
  (234-tree value 
	    value
	    value
	leaf
        leaf
	leaf
	leaf
        )
  )


(define 1-tree
  (234-tree 2 
	    value
	    value
	    (234-tree 1 value value
		      leaf
		      leaf
		      leaf
		      leaf
		      )
            (234-tree 3 44 value
		      leaf
		      leaf
		      leaf
		      leaf
		      )	
	    leaf
	    leaf
	    )
  )


(define 2-tree
  (234-tree 2 
	    4 
	    value
	(234-tree 0 value value
		  leaf
		  leaf
		  leaf
		  leaf
		  )
        (234-tree 3 value value
		  leaf
		  leaf
		  leaf
		  leaf
		  )
	(234-tree 5 6 value
		  leaf
		  leaf
		  leaf
		  leaf
		  )
	leaf
        )
  )


(define 3-tree
  (234-tree 2 
	    4 
	    6
	(234-tree 0 value value
		  leaf
		  leaf
		  leaf
		  leaf
		  )
        (234-tree 3 value value
		  leaf
		  leaf
		  leaf
		  leaf
		  )
	(234-tree 5 value value
		  leaf
		  leaf
		  leaf
		  leaf
		  )
	(234-tree 7 8 value
		  leaf
		  leaf
		  leaf
		  leaf
		  )
        )
  )

;Bad 234 Trees 


(define bad-0-tree
  (234-tree value 
	    value
	    value
	leaf
        leaf
	leaf
	leaf
        )
  )


(define bad-1-tree
  (234-tree 2 
	    value
	    value
	    (234-tree 12 value value
		      leaf
		      leaf
		      leaf
		      leaf
		      )
            (234-tree 3 44 value
		      leaf
		      leaf
		      leaf
		      leaf
		      )	
	    leaf
	    leaf
	    )
  )


(define bad-2-tree
  (234-tree 2 
	    4 
	    value
	(234-tree 0 value value
		  leaf
		  leaf
		  leaf
		  leaf
		  )
        (234-tree 3 value value
		  leaf
		  leaf
		  leaf
		  leaf
		  )
	(234-tree 5 68 value
		  leaf
		  leaf
		  leaf
		  leaf
		  )
	leaf
        )
  )


(define bad-3-tree
  (234-tree 2 
	    4 
	    6
	(234-tree 0 value value
		  leaf
		  leaf
		  leaf
		  leaf
		  )
        (234-tree 3 value value
		  leaf
		  leaf
		  leaf
		  leaf
		  )
	(234-tree 20 value value
		  leaf
		  leaf
		  leaf
		  leaf
		  )
	(234-tree 7 8 value
		  leaf
		  leaf
		  leaf
		  leaf
		  )
        )
  )

;TODO: Create a bad depth tree:

(define bad-depth-tree
  (234-tree 2 
	    value
	    value
	    (234-tree 12 
		      value 
		      value
		      (234-tree 12 
				value 
				value
				(234-tree 12 
					  value 
					  value
					  leaf
					  leaf
					  leaf
					  leaf
					  )
				leaf
				leaf
				leaf
				)
		      leaf
		      leaf
		      leaf
		      )
            (234-tree 3 
		      44
		      value
		      leaf
		      leaf
		      leaf
		      leaf
		      )	
	    leaf
	    leaf
	    )
  )




;Now lets verify the 2-3-4 Tree:
;What to verify:
;-<DONE>leaves are (n + 1) the values of the node
;-<DONE> Has upmost 3 keys
;-All the lengths are the same
;-<DONE>The nodes are ordered in the same manner as a bst

(define get-values (lambda (xs) (take 3 xs)))
(define get-leaves (lambda (xs) (skip 3 xs)))

(define value-count  (lambda (xs)  (count (lambda (x) (not (null? x))) (get-values xs))))
(define leaf-count  (lambda (xs)  (count (lambda (x) (not (null? x))) (get-leaves xs))))



(define check-234-tree (lambda (xs)
			 (if (null? xs) #t
			     (if (check-234-sub-tree xs)
				 (and (check-234-sub-tree (return 0 (get-leaves xs))) 
				      (check-234-sub-tree (return 1 (get-leaves xs))) 
				      (check-234-sub-tree (return 2 (get-leaves xs))) 
				      (check-234-sub-tree (return 3 (get-leaves xs))))
				 #f))))


(check-234-tree bad-0-tree)
(check-234-tree bad-1-tree)
(check-234-tree bad-2-tree)
(check-234-tree bad-3-tree)

;//TODO the max length is one ssless than it should:
(define get-tree-length (lambda (f xs)
			  (if (all? (lambda (x) (= 0 (length x))) (get-leaves xs))
			      (if (all? (lambda (x) (null? x)) (get-values xs)) 0 1)
			      (letrec (				       
				       (first-tree-length (get-tree-length f (return 0 (get-leaves xs))))
				       (second-tree-length (get-tree-length f (return 1 (get-leaves xs))))
				       (third-tree-length (get-tree-length f (return 2 (get-leaves xs))))
				       (fourth-tree-length (get-tree-length f (return 3 (get-leaves xs))))
				       )
				(+ 1 (f first-tree-length second-tree-length third-tree-length fourth-tree-length))
				))))


(get-tree-lengths 1-tree)

(return 3 (get-leaves 1-tree))
		        				  	  
(define tree-length-delta (lambda (a b c d)
			    (- (max a b c d) (min a b c d))))

;(page 3)
;http://www.dcs.gla.ac.uk/~pat/52233/slides/234_RB_trees1x1.pdf
(define check-234-sub-tree (lambda (xs)
			     (and (> 2 (- (get-tree-length min xs) (get-tree-length max xs)))
				  (case (value-count xs) 
				    ((0) #t)
				    ((1) (and (valid-234? (return 0 (get-values xs)) < (return 0 (get-leaves xs))) 
					      (valid-234? (return 0 (get-values xs)) > (return 1 (get-leaves xs))) 
					      )
				     )
				    ((2) (and (valid-234? (return 0 (get-values xs)) < (return 0 (get-leaves xs)))
					      
					      (valid-234? (return 0 (get-values xs)) > (return 1 (get-leaves xs)))       
					      (valid-234? (return 1 (get-values xs)) < (return 1 (get-leaves xs)))
					      
					      (valid-234? (return 1 (get-values xs)) > (return 2 (get-leaves xs)))
					      )
				     )
				    ((3) (and (valid-234? (return 0 (get-values xs)) < (return 0 (get-leaves xs)))
					      
					      (valid-234? (return 0 (get-values xs)) > (return 1 (get-leaves xs)))       
					      (valid-234? (return 1 (get-values xs)) < (return 1 (get-leaves xs)))
					      
					      (valid-234? (return 1 (get-values xs)) > (return 2 (get-leaves xs)))
					      (valid-234? (return 2 (get-values xs)) < (return 2 (get-leaves xs)))
					      
					      (valid-234? (return 2 (get-values xs)) > (return 3 (get-leaves xs)))
					      )
				     )
			       (else (error "Error in check-tree-type"))
			       ))))


(define valid-234? (lambda (root f xs)
		     (if (null? xs) #t
			   (and (or (= (value-count xs) (- (leaf-count xs) 1)) (= (leaf-count xs) 0))
				(all? (lambda (x) (f x root)) (filter (lambda (x) (not (null? x))) (get-values xs)))
				(valid-234? root f (return 0 (get-leaves xs))) 
				(valid-234? root f (return 1 (get-leaves xs))) 
				(valid-234? root f (return 2 (get-leaves xs))) 
				(valid-234? root f (return 3 (get-leaves xs)))
				))))

				
(check-234-sub-tree 0-tree)
(check-234-sub-tree 1-tree)
(check-234-sub-tree 2-tree)
(check-234-sub-tree 3-tree)

(check-234-sub-tree bad-depth-tree)
