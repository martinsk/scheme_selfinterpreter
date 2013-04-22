;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The desugar precompiler,
;;
;;implemented using alot of the same methods that were used
;;in the interpreter. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define desugar
  (letrec* ([list-length-check
	   (lambda (ls len)
	     (if (zero? len)
		 (if (null? ls)
		     #t
		     #f)
		 (if (pair? ls)
		     (list-length-check (cdr ls) (1- len))
		     #f)))]
	  [get-arg
	   (lambda (ls idx)
	     (if  (zero? idx)
		 (car ls)
		 (get-arg (cdr ls) (1- idx))))]
	  [expr-boolean?
	   boolean?]
	  [expr-number?
	   number?]
	  [expr-identifier?
	   (lambda  (e)
	     (symbol? e) )]
	  [expr-lambda-abstr?
	   (lambda (e)
	     (lambda-lambda-abstr? e))]
	  [expr-quote?
	   (lambda (e)
	     (and (list-length-check e 2)
		  (equal? 'quote (get-arg e 0))))]
	  [expr-let?
	   (lambda (e)
	     (and (list-length-check e 3)
		  (equal? 'let (get-arg e 0))
		  (list-length-check (get-arg e 1) 1)
		  (list-length-check (get-arg (get-arg e 1) 0) 2)))]
	  [expr-let*?
	   (lambda  (e)
	     (and (list-length-check e 3)
		  (equal? 'let* (get-arg e 0))
		  (let ([visit
			 (lambda (ls)
			   (if (null? ls)
			       #t
			       (and (list-length-check (car ls) 2)
				    (visit (cdr ls)))))])
		    (lambda (ls) (visit ls))) (get-arg e 1)))]
	  [expr-letrec?
	   (lambda (e)
	     (and (list-length-check e 3)
		  (equal? 'letrec (get-arg e 0))
		  (list-length-check (get-arg e 1) 1)
		  (list-length-check (get-arg (get-arg e 1) 0) 2)))]
	  [expr-letrec*?
	   (lambda  (e)
	     (and (list-length-check e 3)
		  (equal? 'letrec* (get-arg e 0))
		  (let ([visit
			 (lambda (ls)
			   (if (null? ls)
			       #t
			       (and (list-length-check (car ls) 2)
				    (visit (cdr ls)))))])
		    (lambda (ls) (visit ls))) (get-arg e 1)))]
	  [expr-cond?
	   (lambda  (e)
	     (equal? 'cond (get-arg e 0)))]
	  [expr-if?
	   (lambda (e)
	     (and (list-length-check e 4)
		  (equal? 'if (get-arg e 0))))]
	  [expr-list?
	   (lambda (e)
	     (equal? 'list (get-arg e 0)))]
	  [expr-and?
	   (lambda (e)
	     (equal? 'and (get-arg e 0)))]
	  [expr-equal?
	   (lambda (e)
	     (equal? 'equal? (get-arg e 0)))] 
	  [expr-appl?
	   (lambda (e)
	     #t)]
	  [lambda-lambda-abstr?
	   (lambda (e)
	     (and (list-length-check e 3)
		  (equal? 'lambda (get-arg e 0))))]
	  [quotation?
	   (lambda (q)
	     (cond
	       [(expr-identifier? q) #t]
	       [(null? q) #t]
	       [else (errorf 'quotation? "not a proper quotation: ~s" q)]))]
	  [expr-lambda-get-fp
	   (lambda (e)
	     (get-arg (get-arg e 1) 0))]
	  [expr-lambda-get-body
	   (lambda (e)
	     (get-arg e 2))]
	  [expr-appl-get-opr
	   (lambda (e)
	     (get-arg e 0))]
	  [expr-appl-get-opnd
	   (lambda (e)
	     (get-arg e 1))]
	  [expr-quote-get-quotation
	   (lambda (e)
	     (get-arg e 1))]
	  [expr-let-get-dvar
	   (lambda (e)
	     (get-arg (get-arg (get-arg e 1) 0) 0))]
	  [expr-let-get-dexp
	   (lambda (e)
	     (get-arg (get-arg (get-arg e 1) 0) 1))]
	  [expr-let-get-body
	   (lambda (e)
	     (get-arg e 2))]
	  [expr-letrec-get-dvar
	   (lambda (e)
	     (get-arg (get-arg (get-arg e 1) 0) 0))]
	  [expr-letrec-get-dexp
	   (lambda (e)
	     (get-arg (get-arg (get-arg e 1) 0) 1))]
	  [expr-letrec-get-body
	   (lambda (e)
	     (get-arg e 2))]
	  [expr-if-get-cond
	   (lambda (e)
	     (get-arg e 1))]
	  [expr-if-get-conc
	   (lambda (e)
	     (get-arg e 2))]
	  [expr-if-get-altr
	   (lambda (e)
	     (get-arg e 3))]
	  [desugar
	   (lambda  (e)
	     (cond
	       [(null? e) e]
	       [(expr-number? e) e]
 	       [(expr-boolean? e) e]
 	       [(expr-identifier? e) e]
 	       [(expr-quote? e) e]
	       [(expr-let? e) (list 'let
				    (list (list (get-arg (get-arg (get-arg e 1) 0) 0)
						(desugar (get-arg (get-arg (get-arg e 1) 0) 1))))
				    (desugar (get-arg e 2)))]
 	       [(expr-let*? e)
		((lambda (xs body)
		   (letrec ((visit
			      (lambda (xs body)
				(if (null? xs)
				    body
				    (list 'let
					  (list (list (car (car xs))
						      (desugar (car (cdr (car xs))))))
					  (visit (cdr xs) body))))))
		     (visit xs body)))
		 (get-arg e 1) (get-arg e 2))]
 	       [(expr-letrec? e) (list 'letrec
				       (list
					 (list (get-arg (get-arg (get-arg e 1) 0) 0)
					       (desugar (get-arg (get-arg (get-arg e 1) 0) 1))))
				       (desugar (get-arg e 2)))]
 	       [(expr-letrec*? e)
		((lambda (xs body)
		   (letrec ((visit
			      (lambda (xs body)
				(if (null? xs)
				    body
				    (list 'letrec
					  (list (list (car (car xs))
						      (desugar (car (cdr (car xs))))))
					  (visit (cdr xs) body))))))
		     (visit xs body)))
		 (get-arg e 1) (desugar (get-arg e 2)))]
	       [(expr-lambda-abstr? e) ((lambda (xs body)
					  (letrec ([visit
						    (lambda (xs)
						      (if (null? xs)
							  (desugar body)
							  (list 'lambda (list (car xs))
								(visit (cdr xs)))))])
					    (visit xs)))
					(get-arg e 1)
					(desugar (get-arg e 2)))]
	       
	       [(expr-cond? e)
		((letrec ([visit
			   (lambda (xs)
			     (cond
			       [(null? xs) (errorf 'desugar "missing-else-case: ~s" e)]
			       [(equal? 'else (car (car xs)) )
				(desugar (car (cdr (car xs))))]
			       [else (list 'if
					   (desugar (car (car xs)))
					   (desugar (car (cdr (car xs))))
					   (visit (cdr xs)))]))])
		   (lambda (xs)(visit xs))) (cdr e))]
	       [(expr-if? e)
		(list 'if
		      (desugar (expr-if-get-cond e))
		      (desugar (expr-if-get-conc e))
		      (desugar (expr-if-get-altr e)))]
	       [(expr-list? e)
		((lambda (xs)
		   (letrec ([visit
			     (lambda(xs)
			       (if (null? xs)
				   '()
				   (list (list 'curried-cons
					       (desugar (car xs)))
					 (visit (cdr xs)))))])
		     (visit xs))) (cdr e))]
	       [(expr-and? e)
		((lambda (xs)
		   (letrec ([visit
			     (lambda(xs)
			       (if (null? xs)
				   #t
				   (list 'if (desugar (car xs))
					 (visit (cdr xs)) #f)))])
		     (visit xs))) (cdr e))]
	       [(expr-equal? e)
		(list (list 'curried-equal?
			    (desugar (get-arg e 1)))
		      (desugar (get-arg e 2)))]
	       [(expr-appl? e)
		((lambda (args fun)
		   (letrec ([visit
			     (lambda  (args acc)
			       (if (null? args)
				   acc
				   (visit (cdr args) (list acc (desugar (car args))))))])
		     (visit args fun))) (cdr e) (desugar (car e)))]
	       [else
 		(errorf 'desugar "invalid sugar-scm expression: ~s" e)]
	       ))])
	 (lambda (val) (desugar val))))

;; some tests make to tetst the indiviual desugar cases:
	   ;(list 
	   ;(desugar '(let* ([x 1][y 2][z 3]) (* x y z)))
	   ;(desugar '(letrec* ([x (lambda(x y z) x)][y 2][z 3]) (* x y z)))
	   ;(desugar '(letrec ([k (lambda(l m z) (x y z))]) (a s d)))
	   ;(desugar '(cond [#t 3][#t 3][else #f]))
	   ;(desugar '(and (and #t lala) #f 'a))
	   ;(desugar '(lala a s d f g h))
	   ;(desugar interpreter)
	   ;(desugar '(list (list la la) lala lala))
	   

