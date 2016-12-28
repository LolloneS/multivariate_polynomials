# multivariate_polynomials_cl
Progetto Common Lisp sui polinomi multivariati

TEST

1)
	(defparameter POLY1 (as-polynomial '(+ (* y (expt s 3) (expt t 3)) -4 (* x y))))
	(defparameter POLY2 (as-polynomial '(0)))

	(polytimes POLY1 POLY2) ----> 0

2)
	(defparameter POLY1 (as-polynomial '(+ (* y (expt s 3) (expt t 3)) -4 (* x y))))
	(defparameter POLY2 (as-polynomial '(+ (* -1 y (expt s 3) (expt t 3)) -4 (* -1 x y))))
	
	(polyplus POLY1 POLY2) -----> 0
	
3)
	(defparameter POLY1 (as-polynomial '(+ (* y (expt s 3) (expt t 3)) -4 (* x y))))
	
	(polyval POLY1 '(0 0 0 0)) ------> -4
	(polyval POLY1 '(0 0 0)) ------> ERRORE
	(polyval POLY1 '(0 0 0 0 0 0 0)) ------> -4


