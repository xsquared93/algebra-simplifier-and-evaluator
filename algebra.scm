;; an Exp is one of:
;;  - Term
;;  - (make-addtion Exp Exp)
;;  - (make-subtraction Exp Exp)
;;  - (make-multiplication Exp Exp)

;; A Term is one of:
;;  - number
;;  - (make-monomial number variable)

;; exp -> exp
;; this procedure simplifies Exps and performs arithmetic on them.
(define (compute exp)
  (cond ((term? exp) exp)
	((binomial? exp) exp)
	((addition? exp) (make-addition (compute (addend exp))
				        (compute (augend exp))))
	((subtraction? exp) (make-sub (compute (subtrahend exp))
				      (compute (minuend exp))))
	((multiplication? exp) (make-product (compute (multiplicand exp))
					     (compute (multiplier exp))))
	(else (error "Unknown Expression Type -- COMPUTE" exp))))

;; data representation for addition

(define (addition? exp) (and (pair? exp) (eq? (car exp) '+)))

(define (make-addition exp1 exp2)
  (cond ((eq? exp1 0) exp2)
	((eq? exp2 0) exp1)
	((&number? exp1 exp2) (+ exp1 exp2))
	((&monomial? exp1 exp2)
	 (add-monomials exp1 exp2))
	(else (list '+ exp1 exp2))))

(define (addend exp) (cadr exp))
(define (augend exp) (caddr exp))

(define (&number? exp1 exp2)
  (and (number? exp1)
       (number? exp2)))

(define (add-monomials exp1 exp2)
  (if (same-variable? exp1 exp2)
      (make-monomial (+ (coeff exp1) (coeff exp2))
		     (variable exp1)
		     (exponent exp1)) ; assumes same exponent ie (+ (2 x 3) (2 x 3))
      (list '+ exp1 exp2)))

(define (make-monomial coefficient variable exponent)
  (list coefficient variable exponent))
(define (coeff exp) (car exp))
(define (variable exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define (same-variable? exp1 exp2)
  (eq? (variable exp1)
       (variable exp2)))

;; terms data representation
(define (term? exp) (or  (number? exp)
			 (monomial? exp)))
(define (monomial? exp)
  (and (pair? exp)
       (number? (car exp))
       (symbol? (cadr exp))
       (number? (caddr exp))))

(define (&monomial? exp1 exp2)
  (and (monomial? exp1)
       (monomial? exp2)))

;; data representation for subtraction
(define (subtraction? exp)
  (and (pair? exp)
       (eq? (car exp) '-)))

(define (make-sub exp1 exp2)
  (cond ((eq? exp1 0) exp2)
	((eq? exp2 0) exp1)
	((&number? exp1 exp2) (- exp1 exp2))
	((&monomial? exp1 exp2)
	 (sub-monomials exp1 exp2))
	(else (list '- exp1 exp2))))

(define (subtrahend exp) (cadr exp))
(define (minuend exp) (caddr exp))

(define (sub-monomials exp1 exp2)
  (if (same-variable? exp1 exp2)
      (make-monomial (+ (coeff exp1) (coeff exp2))
		     (variable exp1)
		     (exponent exp1))
      (list '- exp1 exp2)))

;; representation for products
(define (multiplication? exp)
  (and (pair? exp)
       (eq? (car exp) '*)))

(define (make-product exp1 exp2)
  (cond ((eq? exp1 0) 0)
	((eq? exp2 0) 0)
	((eq? exp1 1) exp2)
	((eq? exp2 1) exp1)
	((&number? exp1 exp2) (* exp1 exp2))
	((&monomial? exp1 exp2)
	 (multiply-monomials exp1 exp2))
	(else (list '* exp1 exp2))))

(define (multiplicand exp) (cadr exp))
(define (multiplier exp) (caddr exp))

(define (multiply-monomials exp1 exp2)
  (if (same-variable? exp1 exp2)
      (make-monomial (* (coeff exp1) (coeff exp2))
		     (variable exp1)
		     (+ (exponent exp1) (exponent exp2)))
      (list '* exp1 exp2)))

;;; examples/tests

;; #1
(define exp1 '(+ (2 x 3) (2 x 3)))
(compute exp1)
;;value: (4 x 3)

;; #2
(define exp2 '(* (2 x 3) (2 x 3)))
(compute exp2)
;;value: (4 x 6)
