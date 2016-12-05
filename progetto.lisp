;;; Returns the exponent from a variable (v Exp VarSymbol)
(defun varpower-power (vp)
  (let ((pow (second vp)))
    (cond ((numberp pow) pow)
	  (t (error "L'esponente non è un numero")))))

;;; Returns the varsymbol from a variable (v Exp VarSymbol)  
(defun varpower-symbol (vp)
  (let ((vs (third vp)))
    (cond ((and
	    (atom vs)
	    (not (numberp vs))) vs)
	  (t (error "La variabile non è un carattere")))))

;;; Returns the monomial's VPs
(defun monomial-vars-and-powers (mono)
  (and (= (length mono) 4)
       (let ((vps (fourth mono)))
         (cond ((null vps) nil)
               (T vps)))))

;;; Uguale al metodo sopra ma il prof usa due nomi diversi
(defun varpowers (mono)
  (and (= (length mono) 4)
       (let ((vps (fourth mono)))
         (cond ((null vps) nil)
               (T vps)))))

;;; Returns the monomial's TD
(defun monomial-total-degree (mono)
  (and (= (length mono) 4)
       (let ((mtd (third mono)))
         (cond ((>= mtd 0) mtd)
               (T (error "Grado minore di 0"))))))

;;; Uguale al metodo sopra ma il prof usa due nomi diversi
(defun monomial-degree (mono)
  (and (= (length mono) 4)
       (let ((mtd (third mono)))
         (cond ((>= mtd 0) mtd)
               (T (error "Grado minore di 0"))))))

;;; Returns the monomial's coefficient
(defun monomial-coefficient (mono)
  (and (= (length mono) 4)
       (let ((coeff (second mono)))
         (if (numberp coeff) coeff (error "Il coeff non è un numero")))))


;;; (m coefficient total-degree vars-n-powers)
;;; controlla totaldegree
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

;;; (v power var-symbol)
(defun is-varpower(vp)
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


;;; (poly monomials)
(defun is-polynomial (p)
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (poly-monomials p)))
         (and (listp ms)
              (every #'is-monomial ms)))))

(defun coefficients (p)
  (let ((monomials (poly-monomials p)))
  (mapcar 'monomial-coefficient monomials)))
