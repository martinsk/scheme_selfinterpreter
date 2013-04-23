Scheme Self-interpreter
===

The interpreter is able to interpret a simple subset of scheme:

          <expression> ::= <number>
                         | (quote <quotation>)
                         | <identifier>
                         | <lambda-abstraction>
                         | (let ([<identifier> <expression>]) <expression>)
                         | (letrec ([<identifier> <lambda-abstraction>])
                             <expression>)
                         | (if <expression> <expression> <expression>)
                         | (<expression> <expression>)

           <quotation> ::= ()
                         | <identifier>

           <lambda-abstraction> ::= (lambda (<identifier>) <expression>)

This is done using an interpreter and a compiler written in a super-language, and bootstrapping down to the actual language that the compiler does understand.

There is an incoherent blogpost about the work here http://highspeederlang.wordpress.com/2013/04/22/writing-a-compiler-its-not-as-hard-as-you-might-think/

The interpreter and test code is located in the self-interpreter.scm file.
