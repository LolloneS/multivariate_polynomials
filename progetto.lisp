;;;; Mode: -*- Lisp -*-


;;;; progetto.lisp


;;;; Multivariate-Polynomials


;;; varpower-power/1
;; Returns the exponent from a variable (v Exp VarSymbol)

(defun varpower-power (vp)
  (let ((pow (second vp)))
    (if (numberp pow) pow (error "L'esponente non e' un numero"))))


;;; varpower-symbol/1
;; Returns the varsymbol from a variable (v Exp VarSymbol)

(defun varpower-symbol (vp)
  (let ((vs (third vp)))
    (cond ((and
	    (atom vs)
	    (not (numberp vs))) vs)
	  (T (error "La variabile non e' un carattere")))))


;;; varpowers/1
;; Returns the monomial's Vars and Powers

(defun varpowers (mono)
  (if (and (= (length mono) 4) (eq 'm (first mono)))
       (let ((vps (fourth mono)))
         (if (listp vps)
             vps
           (error "le variabili non sono scritte correttamente")))
      (let* ((parsed-mono (as-monomial mono)) (vps (fourth parsed-mono)))
         (if (listp vps)
             vps
           (error "le variabili non sono scritte correttamente")))))


;;; monomial-degree/1
;; Returns the monomial's TD

(defun monomial-degree (mono)
  (if (and (= (length mono) 4) (eq 'm (first mono)))
      (let ((mtd (third mono)))
        (if (>= mtd 0) mtd (error "Grado minore di 0")))
    (let* ((parsed-mono (as-monomial mono)) (mtd (third parsed-mono)))
         (if (>= mtd 0) mtd (error "Grado minore di 0")))))


;;; monomial-coefficient/1
;; Returns the monomial's coefficient

(defun monomial-coefficient (mono)
  (if (and (= (length mono) 4) (eq 'm (first mono)))
      (let ((coeff (second mono)))
         (if (numberp coeff) coeff (error "Il coeff non e' un numero")))
    (let* ((parsed-mono (as-monomial mono)) (coeff (second parsed-mono)))
         (if (numberp coeff) coeff (error "Il coeff non e' un numero")))))


#|
TRUE if m is a monomial
Checks:
-whether the first element in the list equals "m"
-whether the total degree is an integer >= 0
-whether vps is a list and every element of it is a VarPower
|#

;;; is-monomial/1
;; Returns true if m is a monomial

(defun is-monomial (m)
  (and (listp m)
       (eq 'm (first m))
       (let ((mtd (monomial-degree m))
             (vps (varpowers m)))
	 (and (integerp mtd)
	      (>= mtd 0)
	      (listp vps)
	      (every #'is-varpower vps)))))


;;; is-varpower/1
;; T if vp is a list of varpowers

(defun is-varpower (vp)
  (and (listp vp)
       (eq 'v (first vp))
       (let ((p (varpower-power vp))
             (v (varpower-symbol vp))
	     )
         (and (integerp p)
              (>= p 0)
              (symbolp v)))))


;;; poly-monomials/1
;; Returns the list of all the monomials in a poly

(defun poly-monomials (p)
  (first (rest p)))


;;; is-polynomial/1
;; T if p is a polynomial

(defun is-polynomial (p)
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (poly-monomials p)))
         (and (listp ms)
              (every #'is-monomial ms)))))


;;; coefficients/1
;; Returns the list of all the coefficients in a poly

(defun coefficients (p)
  (let* ((parsed-p (to-polynomial p)) (monomials (poly-monomials parsed-p)))
        (mapcar 'monomial-coefficient monomials)))


;;; variables/1
;; Returns the list of all the variables in a poly

(defun variables (p)
  (let ((parsed-p (to-polynomial p)))
      (remove-duplicates (mapcar #'varpower-symbol
                                 (apply #'append
                                        (mapcar #'varpowers
                                                (poly-monomials parsed-p)))))))


;;; vars-of/1
;; Gets the VarSymbols from a monomial

(defun vars-of (mono)
  (if (not (and (equal (first mono) 'm) (= (length mono) 4)))
      (vars-of (as-monomial mono))
    (apply #'append
           (let ((vps (varpowers mono)))
             (append (list (mapcar (lambda (x) (third x)) vps)))))))


;;; maxdegree/1
;; Returns the max degree in the poly

(defun maxdegree (p)
  (let* ((parsed-p (to-polynomial p)))
      (monomial-degree (first (last (poly-monomials parsed-p))))))



;;; mindegree/1
;; Returns the min degree in the poly

(defun mindegree (p)
  (let* ((parsed-p (to-polynomial p)))
      (monomial-degree (first (poly-monomials parsed-p)))))


;;; eval-as-number/1
;; If (eval expression) is a number, returns it. Else NIL

(defun eval-as-number (expression)
  (let ((result (handler-case (eval expression)
		  (error () nil)
		  (warning () nil))))
    (if (numberp result) result nil)))


;;; is-power-not-parsed/1
;; True if expr is an expression to be parsed

(defun is-power-not-parsed (expr)
  (if (not (listp expr)) nil
      (if (and (equal (first expr) 'expt) (symbolp (second expr))
	       (numberp (third expr)))
	  T NIL)))


;;; parse-power/1
;; Parses an expression (expt VAR EXP) into the form (v EXP VAR)

(defun parse-power (expr)
  (if (is-power-not-parsed expr)
      (if (not (eq (third expr) 0)) (list 'm 1 (third expr) (list 'v (third expr) (second expr)))
        (list 'm 1 '0 nil)) nil))


;;; parse-power-negative-coeff/1
;; Parser an expression (expt VAR EXP) into the form (v EXP VAR) manages a negative coefficient

(defun parse-power-negative-coeff (expr)
  (if (is-power-not-parsed expr)
      (list 'm -1 (third expr) (list 'v (third expr) (second expr))) nil))


;;; is-operator/1
;; True if expr is + or - or * or /

(defun is-operator (expr)
  (if (or (eql expr '*) (eql expr '/) (eql expr '-) (eql expr '+))
      T NIL))


;;; sort-monomial/1
;; Sorts the variables in a monomial by lexicographical order

(defun sort-monomial (mono)
  (let ((new-varpowers (copy-list (varpowers mono))))
    (append (list (first mono) (second mono) (third mono))
	    (list (stable-sort new-varpowers 'string< :key 'third)))))


;;; compare-varpowers/2
;; Compares the variables in monomials with the same TD

(defun compare-varpowers (vars1 vars2)
  (cond ((null vars1) (not (null vars2)))
	((null vars2) nil)
	(t
         (let ((v1 (first vars1)) (v2 (first vars2)))
           (cond
	     ((string< (third v1) (third v2)) t)
	     ((string> (third v1) (third v2)) nil)
	     ((and (equal (third v1) (third v2)) (= (second v1) (second v2)))
	      (compare-varpowers (rest vars1) (rest vars2)))
	     (t (< (second (first vars1)) (second (first vars2)))))))))


;;; compare-degrees/2
;; Compares the degrees of the monomials in a poly

(defun compare-degrees (first-mono rest-monos)
  (when (not (null first-mono))
    (let ((degrees
	   (list (monomial-degree first-mono)
		 (monomial-degree rest-monos))))
      (cond ((null first-mono) (not (null rest-monos)))
            ((null rest-monos) nil)
            ((= (first degrees) (second degrees))
	     (compare-varpowers (varpowers first-mono) (varpowers rest-monos)))
            (t (< (first degrees) (second degrees)))))))


;;; sort-poly/2
;; Sorts a polynomial by degree and lexicographical order

(defun sort-poly (monos)
  (let ((poly-copied (copy-list monos)))
    (stable-sort poly-copied #'compare-degrees)))


;;; build-coefficient/1
;; Evaluates the coefficient of a monomial

(defun build-coefficient (expr)
  (if (null expr) 1
      (if (eval-as-number (first expr))
	  (* 1 (eval (first expr)) (build-coefficient (rest expr)))
	  (* 1 (build-coefficient (rest expr))))))


;;; build-varpowers/2
;; Builds the VPs of a monomial

(defun build-varpowers (expr td)
  (let ((head (first expr)) (tail (rest expr)))
    (cond ((and (listp head) (not (null head)) (not (eq (third head) 0)) (equal (first head) 'expt))
           (append (build-varpowers tail (+ (eval td) (eval (third head))))
		   (list (list 'v (third head) (second head)))))
          ((and (listp head) (not (null head)) (eq (third head) 0) (equal (first head) 'expt))
           (append (build-varpowers tail (+ (eval td) (eval (third head)))) nil))
          ((and (symbolp head) (not (null head)))
           (append (build-varpowers tail (+ 1 (eval td)))
		   (list (list 'v 1 head))))
          ((numberp (eval head)) (build-varpowers tail td))
          ((null head) (list td)))))


;;; Parses a monomial
;; NB: per il concetto di atomo in CL, (as-monomial '-x)
;; prende -x come simbolo di variabile

(defun as-monomial (expr)
  (compress-vars-in-monomial (sort-monomial (as-monomial-unordered expr))))


(defun as-monomial-unordered (expr)
  (cond ((eval-as-number expr) (list 'm (eval expr) 0 nil))
        ((atom expr) (list 'm 1 1 (list (list 'v 1 expr))))
        (t (let ((head (first expr)) (tail (rest expr)))
             (if (is-operator head) ;;caso serio
                 (cond ((equal head '-)
                        (if (listp (second expr))
                            (parse-power-negative-coeff (second expr))
			    (list 'm -1 1 (list 'v 1(second expr)))))
                       ((equal head '*)
                        (if (eql (build-coefficient tail) 0) (list 'm 0 0 nil)
			    (let ((vps (build-varpowers tail 0)))
			      (append (list 'm) (list (build-coefficient tail))
				      (list (first vps)) (list (rest vps)))))))
		 (if (is-power-not-parsed head)
		     (parse-power head)
		     (list 'm 1 1 (list 'v 1 head))))))))


;;; as-polynomial/1
;; Parses the input as a polynomial

(defun as-polynomial (expr)
  (if (is-monomial expr) (to-polynomial expr)
      (append (list 'poly)
	      (list
	       (sum-similar-monos-in-poly
		(sort-poly (as-polynomial-call expr)))))))


;;; as-polynomial-call/1
;; Parses a polynomial

(defun as-polynomial-call (expr)
  (when (not (null expr))
    (if (atom expr) (as-monomial expr)
	(let ((head (first expr)) (tail (rest expr)))
	  (if (is-operator head)
	      (if (equal head '+) (as-polynomial-call tail) (list (as-monomial expr)))
	      (if (and (listp expr) (not (null tail)))
		  (append (list (as-monomial head)) (as-polynomial-call tail))
		  (list (as-monomial head))))))))


;;; check-equal-variables/1
;; This function checks if the variables in the monomial are equal

(defun check-equal-variables (expr)
  (let ((variables1 (fourth (first expr))) (variables2 (fourth (second expr))))
    (if (equal variables1 variables2) T NIL)))


;;; sum-similar-monos-in-poly/1
;; This function sums the similar monomials in a polynomial

(defun sum-similar-monos-in-poly (monos)
  (cond ((null monos) nil)
        ((null (second monos)) monos)
        (t
         (let* ((mono1 (first monos))
		(mono2 (second monos))
		(c1 (monomial-coefficient mono1))
		(c2 (monomial-coefficient mono2))
		(td (monomial-degree mono1))
		(vps1 (varpowers mono1))
		(vps2 (varpowers mono2)))
           (if (not (equal vps1 vps2))
	       (append (list mono1)
		       (sum-similar-monos-in-poly (rest monos)))
	       (sum-similar-monos-in-poly
		(append (list (list 'm (+ c1 c2) td vps1))
			(rest (rest monos)))))))))


;;; compress-vars-in-monomial/1
;; This predicate sums the exponents of similiar VPs in a monomial

(defun compress-vars-in-monomial (mono)
  (if (null (varpowers mono)) mono
      (let ((vps (varpowers mono))
	    (c (monomial-coefficient mono))
	    (td (monomial-degree mono)))
	(append (list 'm c td) (list (compress-vps vps))))))


;;; compress-vps/1
;; This predicate sums the exponents of of similiar VPs

(defun compress-vps (vps)
  (if (null vps) nil
      (if (null (second vps)) vps
	  (let* ((vp1 (first vps))
		 (vp2 (second vps))
		 (expt1 (varpower-power vp1))
		 (expt2 (varpower-power vp2))
		 (var1 (varpower-symbol vp1))
		 (var2 (varpower-symbol vp2))
		 (tail (rest (rest vps))))
	    (if (not (null tail))
		(if (not (null vp2))
		    (if (equal var1 var2)
			(compress-vps (append
				       (list (list 'v (+ (eval expt1)
							 (eval expt2))
						   var1))
				       tail))
			(append (list (list 'v expt1 var1))
				(compress-vps (rest vps)))))
		(if (equal var1 var2) (list (list 'v (+ (eval expt1)
							(eval expt2))
						  var1))
		    (append (list (list 'v expt1 var1))
			    (list (list 'v expt2 var2)))))))))


;;; new-pairlis/2
;; This function creates a list with elements from list1 and list2 alternated

(defun new-pairlis (list1 list2)
  (cond ((null list1) list2)
	((null list2) list1)
	(t (append (list (list (first list1) (first list2)))
		   (new-pairlis (rest list1) (rest list2))))))


;;; change-sign/1
;; This predicate changes the sign of the coefficients

(defun change-sign (monos)
  (let* ((mono1 (first monos))
	 (c1 (second mono1))
	 (td (third mono1))
	 (var-powers (fourth mono1)))
    (if (equal (rest monos) nil)
        (append (list (list 'm (- 0 c1) td var-powers)))
	(change-sign (rest monos)))))


;;; to-polynomial/1
;; Checks whether the input is a Poly. If not, the function parses the input

(defun to-polynomial (poly)
  (cond ((is-polynomial poly) (append (list 'poly) (sort-poly (list (poly-monomials poly)))))
	((is-monomial poly) (append (list 'poly) (list (list poly))))
	((if (or (atom poly) (equal '* (first poly))) (to-polynomial (as-monomial poly))
	     (as-polynomial poly)))
        (t (error "Not a valid input! EXPECTED: [Poly] or [Monos]"))))


;;; polyval/2
;; Evaluates a poly in a certain point in space

(defun polyval (poly value)
  (if (listp value)
      (let* ((polyParsed (to-polynomial poly)) (vars (variables polyParsed))
	     (alternate (new-pairlis vars value)) (monos (poly-monomials polyParsed))
	     (monos-with-value (substitute-vars-in-mono monos alternate)))
	(evaluate-monos monos-with-value))

      (error "I valori non sono in una lista")))


;;; substitute-var-in-vp/2
;; This predicate substitutes variable in a vp with a number
;; (evaluation point of the poly)

(defun substitute-var-in-vp (vp alternate)
  (let* ((var (third vp))
	 (var-a (first alternate))
	 (value (second alternate))
	 (tail (rest (rest alternate)))
	 (expt (second vp)))
    (if (and (null var) (null expt))
	(list 'v 0 0)
	(if (eq var var-a)
	    (list 'v expt value)
	    (substitute-var-in-vp vp tail)))))


;;; substitute-vars-in-vps/2
;; This predicate calls the method that performs variable substitution

(defun substitute-vars-in-vps (vps alternate)
  (let* ((head (first vps)) (tail (rest vps)))
    (if (not (null tail))
	(append (list (substitute-var-in-vp head alternate))
		(substitute-vars-in-vps tail alternate))
	(list (substitute-var-in-vp head alternate)))))


;;; substitute-vars-in-mono\2
;; This predicate creates a new monomial by substituing the variables with
;; the corresponding values appearing in the list "alternate"

(defun substitute-vars-in-mono (monos alternate)
  (let* ((head (first monos))
	 (tail (rest monos))
	 (vps (varpowers head))
	 (coef (second head))
         (td (third head))
	 (vps-a (substitute-vars-in-vps vps alternate)))
    (if (not (null tail))
	(append (list (list 'm coef td vps-a)) (substitute-vars-in-mono tail alternate))
	(list (list 'm coef td vps-a)))))


;;; evaluate-monos/1, evaluate-vps/1
;; They caluculate the value of the monomials that compose a polynomial
;; in a certain point

(defun evaluate-monos (monos)
  (let* ((head (first monos))
	 (tail (rest monos))
	 (coef (second head))
	 (vps (varpowers head))
	 (vps-a (evaluate-vps vps)))
    (if (not (null tail))
	(+ (* coef vps-a) (evaluate-monos tail))
	(* coef vps-a))))

(defun evaluate-vps (vps)
  (let* ((head (first vps))
	 (tail (rest vps))
	 (exp (second head))
	 (base (third head)))
    (if (not (null tail))
	(* (expt base exp) (evaluate-vps tail))
	(expt base exp))))


;;; polyplus/2
;; This predicate calculates calculates the sum of two polynomials

(defun polyplus (poly1 poly2)
  (let ((p1 (to-polynomial poly1)) (p2 (to-polynomial poly2)))
    (append (list 'poly)
            (list (sort-poly
                   (sum-similar-monos-in-poly
                    (sort-poly (append (poly-monomials p1)
                                       (poly-monomials p2)))))))))


;;; polyminus/2
;; This predicate calculates the difference of two polynomials

(defun polyminus (poly1 poly2)
  (let ((p1 (to-polynomial poly1)) (p2 (to-polynomial poly2)))
    (append (list 'poly)
            (list (sort-poly
                   (sum-similar-monos-in-poly
                    (sort-poly (append (poly-monomials p1)
                                       (change-sign
                                        (poly-monomials p2))))))))))


;;; polytimes/2
;; This predicate calclulates the product of two polynomials

(defun polytimes (poly1 poly2)
  (append (list 'poly)
          (list (sort-poly (sum-similar-monos-in-poly
			    (polytimes-call
			     (poly-monomials (to-polynomial poly1))
			     (poly-monomials (to-polynomial poly2))))))))


;;; polytimes-call/2
;;

(defun polytimes-call (monos1 monos2)
  (if (or (null monos1) (null monos2)) nil
      (let* ((head1 (first monos1))
	     (head2 (first monos2))
	     (tail1 (rest monos1))
	     (tail2 (rest monos2)))
	(append (list (mono-times head1 head2))
		(polytimes-call (list head1) tail2)
		(polytimes-call tail1 monos2)))))


;;; mono-times/2
;;

(defun mono-times (mono1 mono2)
  (cond ((null mono1) mono2)
        ((null mono2) mono1)
        (t (let ((c1 (monomial-coefficient mono1))
                 (c2 (monomial-coefficient mono2))
                 (td1 (monomial-degree mono1))
                 (td2 (monomial-degree mono2))
                 (vps1 (varpowers mono1))
                 (vps2 (varpowers mono2)))
             (if (or (= 0 c1) (= 0 c2)) (list 'm 0 0 nil)
		 (append (list 'm
			       (* c1 c2)
			       (+ td1 td2)
			       (multiply-variables vps1 vps2))))))))


;;; multiply-variables/2
;;

(defun multiply-variables (vps1 vps2)
  (cond ((null vps1) vps2)
        ((null vps2) vps1)
        (t (let* ((vp1 (first vps1))
                  (vp2 (first vps2))
                  (exp1 (varpower-power vp1))
                  (exp2 (varpower-power vp2))
                  (var1 (varpower-symbol vp1))
                  (var2 (varpower-symbol vp2)))
             (if (equal var1 var2)
                 (append (list (list 'v (+ exp1 exp2) var1))
                         (multiply-variables (rest vps1) (rest vps2)))
		 (if (string>= var1 var2)
		     (append (list (list 'v exp2 var2))
			     (multiply-variables vps1 (rest vps2)))
		     (append (list (list 'v exp1 var1))
			     (multiply-variables (rest vps1) vps2))))))))


;;; pprint-polynomial/1
;; This predicate prints a polynomial in our "traditional" form

(defun pprint-polynomial (poly)
  (pprint-polynomial-call (second (to-polynomial poly))))


;;; pprint-polynomial-call/1
;; This predicate prints a traditional form of poly

(defun pprint-polynomial-call (mono)
  (let ((m1 (first mono)) (c2 (second (second mono))))
    (if (not (null c2))
        (if (> c2 0)
            (append (pprint-polynomial-call-coefficients m1)
		    (list '+)
		    (pprint-polynomial-call (rest mono)))
	    (append (pprint-polynomial-call-coefficients m1)
		    (pprint-polynomial-call (rest mono))))
	(append (pprint-polynomial-call-coefficients m1)))))


;;; pprint-polynomial-call-coefficients/1
;; This predicate prints the coefficient of a mono

(defun pprint-polynomial-call-coefficients (m1)
  (let ((c1 (second m1)) (v&p (fourth m1)))
    (if (equal v&p nil)
        (append (list c1))
	(append (list c1) (list '*) (pprint-polynomial-call-variables v&p)))))


;;; pprint-polynomial-call-variables/1
;; This predicate prints variables and powers of a mono

(defun pprint-polynomial-call-variables (var-power)
  (if (null var-power) nil
      (let ((exp (second (first var-power))) (var (third (first var-power))))
	(if (equal (rest var-power) nil)
	    (if (= exp 1)
		(append (list var)) (append (list var '^ exp)))
	    (if (= exp 1)
		(append (list var '*)
			(pprint-polynomial-call-variables (rest var-power)))
		(append (list var '^ exp '*)
			(pprint-polynomial-call-variables
			 (rest var-power))))))))


;;; end of file -- progetto.lisp
