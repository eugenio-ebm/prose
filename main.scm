(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (string-join del . ss)
  (cond
   [(null? ss)
    ""]
   [(null? (cdr ss))
    (car ss)]
   [else
    (string-append (car ss)
		   del
		   (apply string-join del (cdr ss)))]))

(define (Prose.Grammar.Symbol name)
  (assert (string? name))
  `(Prose.Grammar.Symbol ,name))
(define (Prose.Grammar.Symbol? v)
  (and (pair? v)
       (equal? (car v) 'Prose.Grammar.Symbol)))
(define Prose.Grammar.Symbol.getName cadr)
(define Prose.Grammar.Symbol->string Prose.Grammar.Symbol.getName)

(define (Prose.Grammar.Production lhs rhs)
  (assert (Prose.Grammar.Symbol? lhs))
  (assert (and (list? rhs)
	       (for-all Prose.Grammar.Symbol? rhs)))
  `(Prose.Grammar.Production ,lhs ,rhs))
(define (Prose.Grammar.Production? v)
  (and (pair? v)
       (equal? (car v) 'Prose.Grammar.Production)))
(define Prose.Grammar.Production.getLhs cadr)
(define Prose.Grammar.Production.getRhs caddr)
(define (Prose.Grammar.Production->string p)
  (let ([lhs (Prose.Grammar.Production.getLhs p)]
	[rhs (Prose.Grammar.Production.getRhs p)])
    (string-append (Prose.Grammar.Symbol->string lhs)
		   " -> "
		   (apply string-append
			  (map Prose.Grammar.Symbol->string rhs)))))

(define (Prose.Grammar variables terminals productions start)
  (assert (and (list? variables)
	       (for-all Prose.Grammar.Symbol? variables)))
  (assert (and (list? terminals)
	       (for-all Prose.Grammar.Symbol? terminals)))
  (assert (and (list? productions)
	       (for-all Prose.Grammar.Production? productions)))
  (assert (Prose.Grammar.Symbol? start))
  `(Prose.Grammar ,variables ,terminals ,productions ,start))
(define Prose.Grammar.getVariables cadr)
(define Prose.Grammar.getTerminals caddr)
(define Prose.Grammar.getProductions cadddr)
(define Prose.Grammar.getStart (compose car cddddr))
(define (Prose.Grammar->string g)
  (let ([variables (Prose.Grammar.getVariables g)]
	[terminals (Prose.Grammar.getTerminals g)]
	[productions (Prose.Grammar.getProductions g)]
	[start (Prose.Grammar.getStart g)])
    (string-append "Variables: "
		   (apply string-join ", "
			  (map Prose.Grammar.Symbol->string variables))
		   "\n"
		   "Terminals: "
		   (apply string-join ", "
			  (map Prose.Grammar.Symbol->string terminals))
		   "\n"
		   "Rules:\n  "
		   (apply string-join "\n  "
			  (map Prose.Grammar.Production->string productions))
		   "\n"
		   "Start: "
		   (Prose.Grammar.Symbol->string start))))


(define palindromes
  (Prose.Grammar (list (Prose.Grammar.Symbol "S")
		       (Prose.Grammar.Symbol "A")
		       (Prose.Grammar.Symbol "B"))
		 (list (Prose.Grammar.Symbol "a")
		       (Prose.Grammar.Symbol "b")
		       (Prose.Grammar.Symbol "c"))
		 (list (Prose.Grammar.Production (Prose.Grammar.Symbol "S")
						 (list (Prose.Grammar.Symbol "A")))
		       (Prose.Grammar.Production (Prose.Grammar.Symbol "S")
						 (list (Prose.Grammar.Symbol "B")))
		       (Prose.Grammar.Production (Prose.Grammar.Symbol "S")
						 (list (Prose.Grammar.Symbol "c")))
		       (Prose.Grammar.Production (Prose.Grammar.Symbol "A")
						 (list (Prose.Grammar.Symbol "a")
						       (Prose.Grammar.Symbol "S")
						       (Prose.Grammar.Symbol "a")))
		       (Prose.Grammar.Production (Prose.Grammar.Symbol "B")
						 (list (Prose.Grammar.Symbol "b")
						       (Prose.Grammar.Symbol "S")
						       (Prose.Grammar.Symbol "b"))))
		 (Prose.Grammar.Symbol "S")))

(printf (Prose.Grammar->string palindromes))
