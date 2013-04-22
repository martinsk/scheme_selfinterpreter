;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The self interpreter (with sugar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define meta-interpreter
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
	   (lambda (b)
	   (boolean? b))]
	  [expr-number?
	   (lambda (n)
	     (number? n))]
	  [expr-identifier?
	   (lambda  (e)
	     (symbol? e))]
	  [expr-lambda-abstr?
	   (lambda (e)
	     (lambda-lambda-abstr? e))]
	  [expr-quote?
	   (lambda (e)
	     (and (list-length-check e 2)
		  (equal? 'quote (get-arg e 0))))]
	  [expr-let?
	   (lambda  (e)
	     (and (list-length-check e 3)
		  (equal? 'let (get-arg e 0))
		  (list-length-check (get-arg e 1) 1)
		  (list-length-check (get-arg (get-arg e 1) 0) 2)))]
	  [expr-letrec?
	   (lambda (e)
	     (and (list-length-check e 3)
		  (equal? 'letrec (get-arg e 0))
		  (list-length-check (get-arg e 1) 1)
		  (list-length-check (get-arg (get-arg e 1) 0) 2)))]
	  [expr-if?
	   (lambda (e)
	     (and (list-length-check e 4)
		  (equal? 'if (get-arg e 0))))]
	  [expr-appl?
	   (lambda (e)
	     (list-length-check e 2))]
	  [lambda-lambda-abstr?
	   (lambda (e)
	     (and (list-length-check e 3)
		  (equal? 'lambda (get-arg e 0))
		  (list-length-check (get-arg e 1) 1)))]
	  [lambda-abstr?
	   (lambda (lam)
	     (if (lambda-lambda-abstr? lam)
		 (and (expr-identifier? (get-arg (get-arg lam 1) 0))
		      (expr? (get-arg lam 2)))
		 (errorf 'lambda-abstr? "not a proper lambda-abstr: ~s" lam)))]
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
	  [expr?
	   (lambda  (e)
	     (cond
	       [(null? e) #t]
	       [(expr-boolean? e) #t]
	       [(expr-number? e) #t]
	       [(expr-identifier? e) #t]
	       [(expr-lambda-abstr? e)
		(lambda-abstr? e)]
	       [(expr-quote? e)
		(quotation? (expr-quote-get-quotation e))]
	       [(expr-let? e)
		(and (expr-identifier? (expr-let-get-dvar e))
		     (expr? (expr-let-get-dexp e))
		     (expr? (expr-let-get-body e)))]
	       [(expr-letrec? e)
		(and (expr-identifier? (expr-letrec-get-dvar e))
		     (lambda-abstr? (expr-letrec-get-dexp e))
		     (expr? (expr-letrec-get-body e)))]
	       [(expr-if? e)
		(and (expr? (expr-if-get-cond e))
		     (expr? (expr-if-get-conc e))
		     (expr? (expr-if-get-altr e)))]
	       [(expr-appl? e)
		(and (expr? (expr-appl-get-opr e))
		     (expr? (expr-appl-get-opnd e)))]
	       [else
		(errorf 'expr? "not a proper expr: ~s" e)]))]
	  [eval
	   (lambda (r)
	     (letrec* ([eval_lambda
			(lambda  (r env)
			  (lambda (a)
			    (visit (expr-lambda-get-body r)
				   (lambda (v)
				     (if (equal? v (expr-lambda-get-fp r))
					 a
					 (env v))))))]
		       [visit 
			(trace-lambda eval-visit (r env)
			  (cond
			    [(procedure? r) r]
			    [(expr-boolean? r)  r]
			    [(expr-number? r) r]
			    [(expr-identifier? r)
			     (visit (env r) env)]
			    [(expr-if? r)
			     (if (visit (expr-if-get-cond r) env)
				 (visit (expr-if-get-conc r) env)
				 (visit (expr-if-get-altr r) env))]
			    [(expr-lambda-abstr? r)
			     (eval_lambda r env)]
			    [(expr-let? r)
			     (let ([env_p (lambda (v)
					    (if (equal? v (expr-let-get-dvar r))
						(expr-let-get-dexp r)
						(env v)))])
			       (visit (expr-let-get-body r) env_p))]
			    [(expr-letrec? r)
			     (letrec ([env_p (lambda (v)
					       (if (equal? v (expr-letrec-get-dvar r))
						   (eval_lambda (expr-letrec-get-dexp r) env_p)
						   (env v)))])
			       (visit (expr-letrec-get-body r) env_p))]
			    [(expr-appl? r)
			     ((visit (expr-appl-get-opr r) env)
			      (visit (expr-appl-get-opnd r) env))]
			    [else (errorf 'eval? "error evaluating: ~s" r)]
			    ))]
		       [init_env
			(lambda (v)
			  (cond
			    [(equal? v 'curried-equal?)  curried-equal?]
			    [(equal? v 'curried-*) curried-*]
			    [(equal? v '1-) 1-]
			    [(equal? v 'car) car]
			    [(equal? v 'cdr) cdr]
			    [(equal? v 'null?) null?]
			    [(equal? v 'zero?) zero?]
			    [(equal? v 'pair?) pair?]
			    [(equal? v 'boolean?) boolean?]
			    [(equal? v 'number?) number?]
			    [(equal? v 'symbol?) symbol?]
			    [(equal? v 'procedure?) procedure?]
			    [else
			     (errorf 'eval? "unkown identifier: ~s" v)]))])
		      (visit r init_env)))])
	   
	 (lambda (state)
	   (cond
	     [(equal? state 'interpreter)
	      (lambda (r)
		(eval r))]
	     [(equal? state 'syntax-check)
	      (lambda (r)
		(expr? r))]))))