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
	  (T (error "La variabile non e'  un carattere")))))

(defun varpowers (mono)
  (and (= (length mono) 4)
       (let ((vps (fourth mono)))
          (if (null vps) nil (vps)))))

;;; Returns the monomial's TD
(defun monomial-total-degree (mono)
  (and (= (length mono) 4)
        (let ((mtd (third mono)))
          (if (>= mtd 0) mtd (error "Grado minore di 0")))))

;;; Uguale al metodo sopra ma il prof usa due nomi diversi
(defun monomial-degree (mono)
  (and (= (length mono) 4)
       (let ((mtd (third mono)))
         (if (>= mtd 0) mtd (error "Grado minore di 0")))))

;;; Returns the monomial's coefficient
(defun monomial-coefficient (mono)
  (and (= (length mono) 4)
       (let ((coeff (second mono)))
         (if (numberp coeff) coeff (error "Il coeff non � un numero")))))

(defun monomial-vars-and-powers (mono)
  (and (= (length mono) 4)
       (let ((vps (fourth mono)))
         (if (listp vps) vps (error "VPs non � una lista")))))

;; T if m is a monomial
(defun is-monomial (m)
  (and (listp m)
       (eq 'm (first m))
       (let ((mtd (monomial-total-degree m))
             (vps (monomial-vars-and-powers m))
	     )
	 (and (integerp mtd)
	      (>= mtd 0)
	      (listp vps)
	      (every #'is-varpower vps)))))

;; T if vp is a list of varpower
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


;; T if p is a polynomial
(defun is-polynomial (p)
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (poly-monomials p)))
         (and (listp ms)
              (every #'is-monomial ms)))))

;; Returns the list of all the coefficients in a poly
(defun coefficients (p)
  (let ((monomials (poly-monomials p)))
    (mapcar 'monomial-coefficient monomials)))


;; Returns the list of all the VPs in a poly
(defun poly-variables (p)
  (apply #'append
    (let ((monomials (poly-monomials p)))
             (mapcar 'monomial-vars-and-powers monomials))))


;; Returns the list of all the variables in a poly
(defun variables (p)
      (mapcar #'varpower-symbol
        (apply #'append
         (mapcar #'monomial-vars-and-powers
                  (poly-monomials p)))))


;;; end of file -- progetto.lisp
