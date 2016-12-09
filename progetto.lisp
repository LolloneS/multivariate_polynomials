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
	 (if (listp vps) vps (error "le variabili non sono scritte correttamente")))))

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
       (let ((mtd (monomial-degree m))
             (vps (varpowers m)))
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
	   (mapcar 'varpowers monomials))))


;;; Returns the list of all the variables in a poly
(defun variables (p)
  (mapcar #'varpower-symbol
	  (apply #'append
		 (mapcar #'varpowers
			 (poly-monomials p)))))

;;; Returns the max degree in the poly
(defun maxdegree (p)
  (if (is-polynomial p)
      (maximum-in-list (mapcar #'monomial-degree
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
      (minimum-in-list (mapcar #'monomial-degree
		       (poly-monomials p)))
      (error "P is not a polynomial")))

;;; Returns the minimum in a list
(defun minimum-in-list (l)
  (if (= (length l) 1)
      (car l)
      (if (< (car l) (minimum-in-list (cdr l)))
	  (car l)
	  (minimum-in-list (cdr l)))))

(defun eval-as-number(expression)
  (let ((result (handler-case (eval expression)
		  (error () nil)
		  (warning () nil))))
    (if (numberp result) result nil)))

(defun is-power-not-parsed(expr)
  (if (not (listp expr)) nil
    (if (equal (first expr) 'expt) T NIL)))

(defun parse-power(expr)
  (if (is-power-not-parsed expr) 
      (list 'm 1 (third expr) (second expr)) nil))

(defun parse-power-negative-coeff(expr)
  (if (is-power-not-parsed expr) 
      (list 'm -1 (third expr) (second expr)) nil))

(defun is-operator(expr)
  (if (or (eql expr '*) (eql expr '/) (eql expr '-) (eql expr '+)) 
      T NIL))

(defun build-coefficient(expr)
  (if (eval-as-number (first expr)) 
      (* 1 (eval (first expr)) (build-coefficient (rest expr))) 1))


(defun build-varpowers(expr td)
  (let ((head (first expr)) (tail (rest expr)))
    (cond ((and (listp head) (not (null head)) (equal (first head) 'expt))
           (append (build-varpowers tail (+ (eval td) (eval (third head)))) (list (list 'v (third head) (second head)))))
          ((and (symbolp head) (not (null head))) 
           (append (build-varpowers tail (+ 1 (eval td))) (list (list 'v 1 head)) ))
          ((numberp (eval head)) (build-varpowers tail td))
          ((null head) (list td))
          )))



(defun as-monomial(expr)
  (if (eval-as-number expr)
      ;; se e' solo un numero, calcola il coefficiente e ritorna il monomio
      (list 'm (eval expr) 0 nil)
      (let ((head (first expr)) (tail (rest expr)))
	(if (is-operator head) ;;caso serio
	    (cond ((equal head '-)
		   (if (listp (second expr)) 
		       (parse-power-negative-coeff (second expr)) (list 'm -1 1 (second expr))))
		  ((equal head '*)
		   (if (eql (build-coefficient tail) 0) (list 'm 0 0 nil)
		       (append (list 'm) (list (build-coefficient tail)) (build-varpowers tail 0)))))
	    (if (is-power-not-parsed head) (parse-power head) (list 'm 1 1 head))))))


#|
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

|#




;;; end of file -- progetto.lisp
