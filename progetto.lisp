;;;; Mode: -*- Lisp -*-


;;;; progetto.lisp


;;;; Multivariate-Polynomials



;;; Returns the exponent from a variable (v Exp VarSymbol)
(defun varpower-power (vp)
  (let ((pow (second vp)))
    (if (numberp pow) pow (error "L'esponente non e' un numero"))))

;;; Returns the varsymbol from a variable (v Exp VarSymbol)
(defun varpower-symbol (vp)
  (let ((vs (third vp)))
    (cond ((and
	    (atom vs)
	    (not (numberp vs))) vs)
	  (T (error "La variabile non e' un carattere")))))

;;; Returns the monomial's Vars and Powers
(defun varpowers (mono)
  (and (= (length mono) 4)
       (let ((vps (fourth mono)))
	 (if (null vps) nil (vps)))))

;;; Returns the monomial's Vars and Powers
(defun monomial-vars-and-powers (mono)
  (and (= (length mono) 4)
       (let ((vps (fourth mono)))
         (if (listp vps) vps (error "VPs non e' una lista")))))

;;; Returns the monomial's TD
(defun monomial-total-degree (mono)
  (and (= (length mono) 4)
       (let ((mtd (third mono)))
	 (if (>= mtd 0) mtd (error "Grado minore di 0")))))

;;; Returns the monomial's TD
(defun monomial-degree (mono)
  (and (= (length mono) 4)
       (let ((mtd (third mono)))
         (if (>= mtd 0) mtd (error "Grado minore di 0")))))

;;; Returns the monomial's coefficient
(defun monomial-coefficient (mono)
  (and (= (length mono) 4)
       (let ((coeff (second mono)))
         (if (numberp coeff) coeff (error "Il coeff non e' un numero")))))

#|
TRUE if m is a monomial
Checks:
-whether the first element in the list equals "m"
-whether the total degree is an integer >= 0
-whether vps is a list and every element of it is a VarPower
|#
(defun is-monomial (m)
  (and (listp m)
       (eq 'm (first m))
       (let ((mtd (monomial-total-degree m))
             (vps (monomial-vars-and-powers m)))
	 (and (integerp mtd)
	      (>= mtd 0)
	      (listp vps)
	      (every #'is-varpower vps)))))

;;; T if vp is a list of varpowers
(defun is-varpower (vp)
  (and (listp vp)
       (eq 'v (first vp))
       (let ((p (varpower-power vp))
             (v (varpower-symbol vp))
	     )
         (and (integerp p)
              (>= p 0)
              (symbolp v)))))

;;; Returns the list of all the monomials in a poly
(defun poly-monomials (p)
  (rest p)
  )


;;; T if p is a polynomial
(defun is-polynomial (p)
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (poly-monomials p)))
         (and (listp ms)
              (every #'is-monomial ms)))))

;;; Returns the list of all the coefficients in a poly
(defun coefficients (p)
  (let ((monomials (poly-monomials p)))
    (mapcar 'monomial-coefficient monomials)))


;;; Returns the list of all the VPs in a poly
(defun poly-variables (p)
  (apply #'append
	 (let ((monomials (poly-monomials p)))
	   (mapcar 'monomial-vars-and-powers monomials))))


;;; Returns the list of all the variables in a poly
(defun variables (p)
  (mapcar #'varpower-symbol
	  (apply #'append
		 (mapcar #'monomial-vars-and-powers
			 (poly-monomials p)))))

;;; Returns the max degree in the poly
(defun maxdegree (p)
  (if (is-polynomial p)
      (maximum-in-list (mapcar #'monomial-total-degree
		       (poly-monomials p)))
      (error "P is not a polynomial")))

;;; Returns the maximum in a list
(defun maximum-in-list (l)
  (if (= (length l) 1)
      (car l)
      (if (> (car l) (maximum-in-list (cdr l)))
          (car l)
          (maximum-in-list (cdr l)))))

;;; Returns the min degree in the poly
(defun mindegree (p)
  (if (is-polynomial p)
      (minimum-in-list (mapcar #'monomial-total-degree
		       (poly-monomials p)))
      (error "P is not a polynomial")))

;;; Returns the minimum in a list
(defun minimum-in-list (l)
  (if (= (length l) 1)
      (car l)
      (if (< (car l) (minimum-in-list (cdr l)))
	  (car l)
	  (minimum-in-list (cdr l)))))

;;; Checks whether the list contains only numbers and lists (recursively)
(defun check-only-numbers-lists-in-list (expr)
  (let ((head (first expr)) (tail (rest expr)))
    (cond ((null expr) t)
	  ((numberp head) (check-only-numbers-lists-in-list tail))
	  ((listp head) (and (check-expression-contains-no-variables head) (check-expression-contains-no-variables tail)))
	  (t (error "C'e' un problema con l'input...")))))


;;; Checks whether the expression contains variables
;;; in case it begins with /
(defun check-expression-contains-no-variables (expr)
  (let ((head (first expr)) (tail (rest expr)))
    (cond ((null expr) t)
	  ((listp head) (and (check-expression-contains-no-variables head) (check-expression-contains-no-variables tail)))
	  ((numberp head) (error "La struttura non e' corretta"))
					; se il primo della lista e' un operatore aritmetico, controllo che il resto della lista sia fatto di numeri e liste
	  ((numberp (position head '(+ - * /))) (check-only-numbers-lists-in-list tail))
	  (t (error "Ci sono variabili in una posizione sbagliata!"))
	  )))


;;; as-monomial
(defun as-monomial (expr)
  (let ((head (first expr)))
    (cond ((and (or (eql head '-) (eql head '+) (eql head '/)) (check-expression-contains-no-variables expr))
	   (let ((coeff (coerce (eval expr) 'float))) (list 'm coeff 0 ()))) ;; solo numeri TODO
	  ((eql head '*) 6) ; primo simbolo * e' un prodotto di piu' variabili || TODO
	  ((numberp head) (list 'm head 0 ())) ;coefficiente intero
	  ((symbolp head) (list 'm 1 1 (list 'v 1 head))) ; una sola variabile
	  (t (error "The expression can't be parsed as a monomial")) ; errore
	  )))

;;; end of file -- progetto.lisp
