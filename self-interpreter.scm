;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implemtation of a self-interpreter for a subset of
;; scheme, and a desugar scheme, to make life alittle sweeter
;;
;; Made by Martin Kristiansen (2010),
;; aarhkortnummer 20011579
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; predefined number? symbol? boolean? pair? null? equal? errorf
(define curried-equal?
  (lambda (x)
    (lambda (y)
      (equal? x y))))

(define curried-*
  (lambda (x)
    (lambda (y)
      (* x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The self interpreter (with sugar)
;;
;; Defines the process 'meta-interpreter'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "meta-interpreter.scm")
;; cases for using the interpreter

(define interpreter (meta-interpreter 'interpreter))
(define syntax-checker (meta-interpreter 'syntax-check))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The desugar precompiler,
;;
;; defines the procedure 'desugar'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "desugar.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  THE TEST CODE, A copy of the interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "define-expression-interpreter.scm")

;(define expression '((lambda (x y) (and x y)) #t #f))

(desugar '((lambda (x y z)
	     (let ([visit
		    (lambda (x y z)
		      (cond
			[(equal? x 0) #t]
			[(equal? y 0) #f]
			[else (visit (1- z) (1- (1- x)) y)]))])
	       (visit x y z))) 1 2 3))
			    

; test the desugar method
(interpreter (desugar '(letrec* ([countdown 
				  (lambda (from togo)
				    (if (zero? togo )
					from
					(countdown (1- from) (1- togo))))])
				(countdown 100 20))))



;; this generates ALOT of code in the terminal
;(desugar expression-interpreter)
;'(DESUGAR IS DONE)

;(syntax-checker (desugar expression-interpreter))
;'(SYNTAX_CHECKER IS DONE)

;(interpreter (desugar expression-interpreter))
;'(INTERPRETER IS DONE)


