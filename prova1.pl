% -*- Mode: Prolog -*-

%%% sum_degrees_variables/2
% TRUE if TotalDegree is the sum of all VP's exponent
sum_degrees_variables([], 0) :- !.
sum_degrees_variables([v(Exp, _Var) | Vs], TotalDegree) :-
    sum_degrees_variables(Vs, TotalDegree2), !,
    TotalDegree is Exp + TotalDegree2.

%%% is_monomial/1
% TRUE if the argument is a Monomial
is_monomial(m(_C, TD, VPs)) :-
    integer(TD),
    TD >= 0,
    is_list(VPs),
    foreach(member(V, VPs), is_varpower(V)),
    sum_degrees_variables(VPs, TD).

%%% is_varpower/1
% TRUE if the argument is a VP
is_varpower(v(Power, VarSymbol)) :-
    integer(Power),
    Power >= 0,
    atom(VarSymbol).

%%% is_polynomial/1
% TRUE if the argument is a list of Momomials
is_polynomial(poly(Monomials)) :-
    is_list(Monomials),
    foreach(member(M, Monomials), is_monomial(M)).


%%%% First of all some "getters", because readability counts

%%% get_power_from_variable/2
% TRUE if Power unifies with the variable's power
get_power_from_variable(v(Power, _VarSymbol), Power) :- !.

%%% get_varsymbol_from_variable/2
% TRUE if VarSymbol unifies with the variable's VarSymbol
get_varsymbol_from_variable(v(_Power, VarSymbol), VarSymbol) :- !.

%%% get_varsymbols_from_variables/2
% TRUE if the second argument is the list of all the VarSymbols
% that appear in the first argument
get_varsymbols_from_variables([], []) :- !.
get_varsymbols_from_variables([v(_Power, VarSymbol) | VPs],
			      [VarSymbol | VSs]) :-
    !,
    get_varsymbols_from_variables(VPs, VSs).

%%% get_powers_from_variables/2
% TRUE if the second argument is the list of all the Powers
% that appear in the first argument
get_powers_from_variables([], []) :- !.
get_powers_from_variables([v(Power, _VarSymbol) | Xs], [Power | RestPower]) :-
    !,
    get_powers_from_variables(Xs, RestPower).

%%% get_coefficient_from_monomial/2
% TRUE if the second argument is the coefficient of the first argument
% (a monomial)
get_coefficient_from_monomial(m(C, _TD, _VPs), C) :- !.

%%% get_total_degree_from_monomial/2
% TRUE if the second argument is the total degree of the first argument
% (a monomial)
get_total_degree_from_monomial(m(_C, TD, _VPs), TD) :- !.

%%% get_variables_from_monomial/2
% TRUE if the second argument is the list of variables of the first argument
% (a monomial)
get_variables_from_monomial(m(_C, _TD, VPs), VPs) :- !.

%%% get_coefficients_from_polynomial/2
% Used in coefficients/2
get_coefficients_from_polynomial(poly([]), []) :- !.
get_coefficients_from_polynomial(poly(Monomials), Coefficients) :-
    get_coefficients_from_polynomialCall(poly(Monomials), Coefficients).
get_coefficients_from_polynomialCall(poly([]), []) :- !.
get_coefficients_from_polynomialCall(poly([HeadMono | RestMono]),
				     [R | RestCoeffList]) :-
    get_coefficient_from_monomial(HeadMono, R),
    get_coefficients_from_polynomialCall(poly(RestMono), RestCoeffList).

%%% coefficients/2
% TRUE if the second arg is the list of the coefficients of the
% monomials composing the polynomial passed as the first arg
coefficients(poly([]), []) :- !.
coefficients(Poly, Coefficients) :-
    to_polynomial(Poly, PolyParsed),
    !,
    get_coefficients_from_polynomial(PolyParsed, Coefficients).

%%% get_variables_from_polynomial/2
% TRUE if the second arg unifies with the list of the variables of the
% monomials composing the polynomial passed as the first arg, sorted by
% lexicographical order
get_variables_from_polynomial(poly([]), []) :- !.
get_variables_from_polynomial(poly(Monomials), Variables) :-
    get_variables_from_polynomialCall(poly(Monomials), Variables2),
    append(Variables2, Variables3),
    sort(2, @=<, Variables3, Variables).
get_variables_from_polynomialCall(poly([]), []) :- !.
get_variables_from_polynomialCall(poly([HeadMono | RestMono]),
				  [R | ListaVar]) :-
    get_variables_from_monomial(HeadMono, R),
    get_variables_from_polynomialCall(poly(RestMono), ListaVar).

%%% variables/2
% Returns the list of all variables in a poly
variables(poly([]), []) :- !.
variables(Poly, Variables) :-
    to_polynomial(Poly, PolyParsed),
    !,
    get_variables_from_polynomial(PolyParsed, VarsList),
    get_varsymbols_from_variables(VarsList, SVList),
    !,
    sort(SVList, Variables).

%%% mindegree/2
% Gets the minimum degree of all the monomials in a polynomial
mindegree(poly([]), 0) :- !.
mindegree(Poly, Degree) :-
    to_polynomial(Poly, PolyParsed), !,
    get_variables_from_polynomial(PolyParsed, VPs),
    get_powers_from_variables(VPs, FinalList),
    minInList(FinalList, Degree2), !,
    Degree2 >= 0,
    Degree is Degree2.

%%% minInList/2
% Gets the minimum element in a list
minInList([], 0) :- !.
minInList([X], X) :- !.
minInList([X | Xs], M):-
    minInList(Xs, M),
    M =< X.
minInList([X | Xs], X):-
    minInList(Xs, M),
    X =< M.

%%% maxdegree/2
% Gets the maximum degree of all the monomials in a polynomial
maxdegree(poly([]), 0) :- !.
maxdegree(Poly, Degree) :-
    to_polynomial(Poly, PolyParsed), !,
    get_variables_from_polynomial(PolyParsed, VPs),
    get_powers_from_variables(VPs, FinalList),
    maxInList(FinalList, Degree2), !,
    Degree2 >= 0,
    Degree is Degree2.

%%% maxInList/2
% Gets the maxiumum element in a list
maxInList([], 0) :- !.
maxInList([X], X) :- !.
maxInList([X | Xs], M):-
    maxInList(Xs, M),
    M >= X.
maxInList([X | Xs], X):-
    maxInList(Xs, M),
    X >= M.

%%% reduce_monomial/2
% TRUE if ReducedMono is the Mono with all similar variables multipied
% f.i. x * x -> x^2
reduce_monomial(Mono, ReducedMono) :-
    !,
    reduce_monomial_call(Mono, ReducedMono).

reduce_monomial_call(m(0, _, _), m(0, 0, [])) :- !.
reduce_monomial_call(m(C, 0, []), m(C, 0, [])) :- !.
reduce_monomial_call(m(C, TD, [v(Exp, Var)]), m(C, TD, [v(Exp, Var)])) :- !.
reduce_monomial_call(m(C, TD, [v(Degree1, Var), v(Degree2, Var) | VPs]),
		     m(C, TD, VPsReduced)) :-
    !,
    Z is Degree1 + Degree2, !,
    reduce_monomial(m(C, TD, [v(Z, Var) | VPs]), m(C, TD, VPsReduced)).
reduce_monomial_call(m(C, TD, [v(Degree1, Var), v(Degree2, DiffVar) | VPs]),
		     m(C, TD, [v(Degree1, Var) | VPsReduced])) :-
    !,
    reduce_monomial(m(C, TD, [v(Degree2, DiffVar) | VPs]),
		    m(C, TD, VPsReduced)).

%%% sum_similar_monomials_in_poly/2
% TRUE if the 2nd arg is the Poly passed as the first arg
% with all of the similar monomials summed
% f.i. x + x -> 2 * x
sum_similar_monomials_in_poly(poly([]), poly([])) :- !.
sum_similar_monomials_in_poly(poly([X]), poly([X])) :- !.
sum_similar_monomials_in_poly(poly([A, B | Tail1]), poly(Tail2)) :-
    get_total_degree_from_monomial(A, TD),
    get_total_degree_from_monomial(B, TD),
    get_variables_from_monomial(A, VPs),
    get_variables_from_monomial(B, VPs),
    get_coefficient_from_monomial(A, C1),
    get_coefficient_from_monomial(B, C2),
    Z is C1+C2, !,
    sum_similar_monomials_in_poly(poly([m(Z, TD, VPs) | Tail1]),
				  poly(Tail2)).
sum_similar_monomials_in_poly(poly([A, B | Tail1]),
			      poly([A | Tail2])) :-
    !,
    sum_similar_monomials_in_poly(poly([B | Tail1]), poly(Tail2)).

%%% pairlis/3
% Appends the first and the second list alternatively into the third one.
pairlis([], [], []) :- !.
pairlis([], M, M) :- !.
pairlis([X | Xs], [Y | Ys], [X, Y | Zs]) :-
    pairlis(Xs, Ys, Zs).

%%% as_monomial/2
% TRUE if Monomial is the term which represents the resulting monomial
% of Expression parsed
as_monomial(Expression, Monomial) :-
    as_monomialCall(Expression, MonomialUnreduced),
    reduce_monomial(MonomialUnreduced, Monomial).

%%% as_monomialCall/2
% TRUE if the 2nd argument is a Monomial parsed and sorted starting from an
% Expression passed as the 1st argument
as_monomialCall(Expression, m(C, TD, VPs)) :-
    as_monomial_unordered(Expression, m(C, TD, VPs2)),
    sort(2, @=<, VPs2, VPs).

%%% as_monomial_unordered/2
% This predicate pareses the 1st arg without sorting the resulting monomial
as_monomial_unordered(0, m(0, 0, [])) :- !.
as_monomial_unordered(-Mono, m(NC, TD, VPs)) :-
    !,
    as_monomial_unordered(Mono, m(C, TD, VPs)), !,
    NC is -C.
as_monomial_unordered(SingleVar, m(1, 1, [v(1, SingleVar)])) :-
    atom(SingleVar),
    !.
as_monomial_unordered(SingleVar ^ Exp, m(1, 0, [])) :-
    atom(SingleVar),
    integer(Exp),
    Exp == 0.
as_monomial_unordered(SingleVar ^ Exp, m(1, Exp, [v(Exp, SingleVar)])) :-
    atom(SingleVar), !,
    integer(Exp), !.
as_monomial_unordered(Head * Tail, m(C, TD, [v(1, Tail) | VPs])) :-
    atom(Tail),	!,
    as_monomial_unordered(Head, m(C, TD1, VPs)),
    TD is TD1 + 1.
as_monomial_unordered(Head * A ^ 0, m(C, TD, VPs)) :-
    atom(A), !,
    as_monomial_unordered(Head, m(C, TD1, VPs)),
    TD is TD1.
as_monomial_unordered(Head * A ^ B, m(C, TD, [v(B, A) | VPs])) :-
    number(B), !,
    atom(A), !,
    as_monomial_unordered(Head, m(C, TD1, VPs)),
    TD is TD1 + B.
as_monomial_unordered(UglyCoeff, m(Z, 0, [])) :-
    UglyCoeff \= 0, !,
    arithmetic_expression_value(UglyCoeff, Z).

%%% compare_variables/3
% This predicate -combined with compare_monomials/3- is used
% to sort the monomials in a poly
compare_variables(>, [], _) :- !.
compare_variables(<, _, []) :- !.
compare_variables(<, v(_, Var1), v(_, Var2)) :-
    Var1 @< Var2,
    !.
compare_variables(>, v(_, Var1), v(_, Var2)) :-
    Var1 @> Var2,
    !.
compare_variables(<, v(Exp1, Var), v(Exp2, Var)) :-
    Exp1 < Exp2,
    !.
compare_variables(>, v(Exp1, Var), v(Exp2, Var)) :-
    Exp1 > Exp2,
    !.
compare_variables(<, [v(Exp, Var) | Vs1] , [v(Exp, Var) | Vs2]) :-
    !,
    compare_variables(<, Vs1, Vs2).
compare_variables(<, [v(Exp1, Var) | _] , [v(Exp2, Var) | _]) :-
    Exp1 < Exp2,
    !.
compare_variables(>, [v(Exp, Var) | Vs1] , [v(Exp, Var) | Vs2]) :-
    !,
    compare_variables(>, Vs1, Vs2).
compare_variables(>, [v(Exp1, Var) | _] , [v(Exp2, Var) | _]) :-
    Exp1 > Exp2,
    !.
compare_variables(<, [v(_, Var1) | _Vs1] , [v(_, Var2) | _Vs2]) :-
    Var1 @< Var2,
    !.
compare_variables(>, [v(_, Var1) | _Vs1] , [v(_, Var2) | _Vs2]) :-
    Var1 @> Var2,
    !.

%%% compare_monomials/3
compare_monomials(<, m(_C1, TD, VPs1), m(_C2, TD, VPs2)) :-
    compare_variables(<, VPs1, VPs2),
    !.
compare_monomials(>, m(_C1, TD1, _VPs1), m(_C2, TD2, _VPs2)) :-
    TD1 > TD2, !.
compare_monomials(>, m(_C1, TD, VPs1), m(_C2, TD, VPs2)) :-
    compare_variables(>, VPs1, VPs2),
    !.
compare_monomials(<, m(_C1, TD1, _VPs1), m(_C2, TD2, _VPs2)) :-
    TD1 < TD2, !.

%%% sort_monomials_in_polynomial/2
% TRUE if the second argument is a polynomial which unifies
% with the first argument sorted by degree and lexicographical order.
sort_monomials_in_polynomial(poly(Monomials), poly(SortedMonomials)) :-
    remove_coeff_zero(poly(Monomials), poly(MonoWithout0s)),
    predsort(compare_monomials, MonoWithout0s, SortedMonomials).


%%% as_polynomial/2
% TRUE if Polynomial is the Expression parsed and without the monomials
% with coefficient = 0.
as_polynomial(Expression, Polynomial) :-
    as_polynomialCall(Expression, PolynomialWith0s),
    remove_coeff_zero(PolynomialWith0s, Polynomial).

%%% as_polynomialCall/2
% Parses and sorts a polynomial, then it sums the similar monomials in it.
as_polynomialCall(m(0, _, _), poly([])) :- !.
as_polynomialCall(m(C, TD, VPs2), poly([m(C, TD, VPs)])) :-
    is_monomial(m(C, TD, VPs2)), !,
    sort(2, @=<, VPs2, VPs).
as_polynomialCall(Expression, poly(Monomials)) :-
    as_polynomial_unordered(Expression, poly(Monomials2)),
    sort_monomials_in_polynomial(poly(Monomials2), poly(SortedMonomials)),
    sum_similar_monomials_in_poly(poly(SortedMonomials), poly(Monomials)).
as_polynomial_unordered(MonoHead + MonoTail, poly(Parsed)) :-
    as_monomial(MonoTail, ParsedTail),
    as_polynomial_unordered(MonoHead, poly(ParsedHead)), !,
    append([ParsedTail], ParsedHead, Parsed).
as_polynomial_unordered(MonoHead - MonoTail, poly(Parsed)) :-
    as_monomial(-MonoTail, ParsedTail),
    as_polynomial_unordered(MonoHead, poly(ParsedHead)), !,
    append([ParsedTail], ParsedHead, Parsed).
as_polynomial_unordered(Mono, poly([ParsedMono])) :-
    !,
    as_monomial(Mono, ParsedMono).


%%% to_polynomial/2
% TRUE if ParsedPoly is the polynomial Poly parsed,
% reduced and sorted by grade and lexicographical order.

to_polynomial(Poly, Poly) :- is_polynomial(Poly), !.
to_polynomial(Poly, ParsedPoly) :- as_polynomial(Poly, ParsedPoly).

%%% polyval/3
% Evaluates a poly in a certain point in space.
% TRUE if Value is the value of Polynomial in the point P = VariableValues
% f.i.: Polynomial = x^2 + y, VariableValues = [1, 2], Value = 1^2 + 2 = 3.
polyval(Polynomial, VariableValues, Value) :-
    polyvalCall(Polynomial, VariableValues, Value).

%%% polyvalCall/3
% Executes polyval/3
polyvalCall(Polynomial, VariableValues, Value) :-
    to_polynomial(Polynomial, poly(Monomials2)), !,
    is_list(VariableValues), !,
    sort_monomials_in_polynomial(poly(Monomials2), poly(Monomials)),
    variables(poly(Monomials), VSs),
    length(VariableValues, L1),
    length(VSs, L2),
    L1 >= L2,
    pairlis(VSs, VariableValues, Alternated),
    evaluate_poly(poly(Monomials), Alternated, Value).

%%% evaluate_poly/3
% Evaluates a polynomial by summing the values of the single monomials
evaluate_poly(poly([]), _, 0) :- !.
evaluate_poly(poly([m(C, TD, VPs) | OtherMonos]), Alternated, Value) :-
    evaluate_mono(m(C, TD, VPs), Alternated, ValueMono),
    evaluate_poly(poly(OtherMonos), Alternated, ValueRestPoly),
    Value is ValueMono + ValueRestPoly.

%%% substitute_vars/3
% The vars in the poly are substituted by their actual values
substitute_vars([], _, []) :- !.
substitute_vars([v(Exp, Var)], [Var, NewValue], [v(Exp, NewValue)]) :- !.
substitute_vars([v(Exp, Var)], [DifferentVar, _NewValue | Rest], Others) :-
    Var \= DifferentVar, !,
    substitute_vars([v(Exp, Var)], Rest, Others).
substitute_vars([v(Exp, Var) | Vs], [Var, NewValue | Rest],
		[v(Exp, NewValue) | Others]) :-
    !,
    substitute_vars(Vs, Rest, Others).
substitute_vars([v(Exp, Var) | Vs], [Var2, _NewValue | Rest], Others) :-
    Var \= Var2, !,
    substitute_vars([v(Exp, Var) | Vs], Rest, Others).

%%% substitute_mono/3
% This predicate creates a new monomial by substituing the VarSymbols with
% the corresponding values
substitute_mono(m(C, TD, VPs), Alternated, m(C, TD, ListOfVarsOk)) :-
    get_variables_from_monomial(m(C, TD, VPs), ListOfVars),
    substitute_vars(ListOfVars, Alternated, ListOfVarsOk).

%%% evaluate_vars/2, evaluate_mono/2
% They caluculate the value of the monomials that compose the polynomial
% in a certain point
evaluate_vars([], 1) :- !.
evaluate_vars([v(Exp, Base) | RestVs], ValueOfTheVars) :-
    Value is Base ** Exp,
    evaluate_vars(RestVs, ValueRest),
    ValueOfTheVars is ValueRest * Value.

evaluate_mono(m(C, TD, VPs), Alternated, Value) :-
    substitute_mono(m(C, TD, VPs), Alternated, m(C, TD, VPSubstituted)),
    evaluate_vars(VPSubstituted, ValueOfTheVars),
    Value is C * ValueOfTheVars.

%%% monomials/2
% TRUE if Monomials is a list that contains all monomials in Poly
% reduced and sorted by grade and lexicographical order
monomials(poly([]), []) :- !.
monomials(Poly, Monomials):-
    to_polynomial(Poly, poly(Parsed)), !,
    sort_monomials_in_polynomial(poly(Parsed), Monomials).

%%% opposite_polynomial/2
% This predicate changes the coefficient of every Monomial that compose the Poly
% to its opposite
opposite_polynomial(poly([]), poly([])) :- !.
opposite_polynomial(poly([m(C, TD, VPs) | Monos]),
		                poly([m(NC, TD, VPs) | OppMonos])) :-
    NC is -C,
    !,
    opposite_polynomial(poly(Monos), poly(OppMonos)).

%%% reduce_all_monos/2
%
reduce_all_monos(poly([]), poly([])) :- !.
reduce_all_monos(poly([HeadMono | TailMono]),
                 poly([HeadReduced | TailReduced])) :-
    reduce_monomial(HeadMono, HeadReduced),
    reduce_all_monos(poly(TailMono), poly(TailReduced)).

%%% remove_coeff_zero/2
% Removes all the monomials with C = 0 from a Poly
remove_coeff_zero(poly([]), poly([])) :- !.
remove_coeff_zero(poly([m(0, _, _) | Tail]), poly(Tail2)) :-
    !,
    remove_coeff_zero(poly(Tail), poly(Tail2)).
remove_coeff_zero(poly([m(C, TD, VPs) | Tail]),
		  poly([m(C, TD, VPs) | Tail2])) :-
    !,
    remove_coeff_zero(poly(Tail), poly(Tail2)).

%%% polyplus/3
% TRUE if Result is the sum of Poly1 and Poly2
polyplus(Poly1, Poly2, Result):-
    to_polynomial(Poly1, Poly1Parsed),
    to_polynomial(Poly2, Poly2Parsed),
    polyplus_call(Poly1Parsed, Poly2Parsed, Result).

%%% polyplus_call/3
% TRUE if the 3rd arg is the sum of the first and the second polynomial
polyplus_call(poly([]), poly([]), poly([])) :- !.
polyplus_call(poly([]), poly(Monos), poly(SortedMonos)) :-
    sort_monomials_in_polynomial(poly(Monos), poly(SortedMonos)), !.
polyplus_call(poly(Monos), poly([]), poly(SortedMonos)) :-
    sort_monomials_in_polynomial(poly(Monos), poly(SortedMonos)), !.
polyplus_call(poly(M1), poly(M2), poly(Z)) :-
    append(M1, M2, Z1),
    sort_monomials_in_polynomial(poly(Z1), poly(Z2)),
    sum_similar_monomials_in_poly(poly(Z2), poly(Z)).

%%% polyminus/3
% TRUE if Result is the difference between Poly1 and Poly2
polyminus(Poly1, Poly2, Result):-
    to_polynomial(Poly1, Poly1Parsed),
    to_polynomial(Poly2, Poly2Parsed),
    opposite_polynomial(Poly2Parsed, OppPoly2Parsed),
    polyminus_call(Poly1Parsed, OppPoly2Parsed, Result).

%%% polyminus_call/3
% TRUE if the third arg is the difference between the first and the
% second polynomial
polyminus_call(poly([]), poly([]), poly([])) :- !.
polyminus_call(poly([]), poly(Monos), poly(SortedMonos)) :-
    sort_monomials_in_polynomial(poly(Monos), poly(SortedMonos)), !.
polyminus_call(poly(Monos), poly([]), poly(SortedMonos)) :-
    sort_monomials_in_polynomial(poly(Monos), poly(SortedMonos)), !.
polyminus_call(poly(M1), poly(M2), poly(Z)) :-
    append(M1, M2, Z1),
    sort_monomials_in_polynomial(poly(Z1), poly(Z2)),
    sum_similar_monomials_in_poly(poly(Z2), poly(Z3)),
    reduce_all_monos(poly(Z3), poly(Z4)),
    remove_coeff_zero(poly(Z4), poly(Z)).

%%% polytimes/3
% TRUE if Result is Poly1 * Poly2
polytimes(Poly1, Poly2, Result) :-
    to_polynomial(Poly1, Poly1Parsed), !,
    to_polynomial(Poly2, Poly2Parsed), !,
    polytimes_call(Poly1Parsed, Poly2Parsed, PolyResult),
    sum_similar_monomials_in_poly(PolyResult, Result).

%%% polytimes_call/3
% TRUE if 3rd argument is the product between the first and the
% second polynomial passed as 1st and 2nd argument
polytimes_call(poly([]), poly([]), poly([])) :- !.
polytimes_call(poly([]), poly(_Monos), poly([])) :- !.
polytimes_call(poly(_Monos), poly([]), poly([])) :- !.

polytimes_call(poly([Head1 | Tail1]), poly([Head2 | Tail2]),
	       poly([Z | Tail3])) :-
    mono_times(Head1, Head2, Z),
    polytimes_call(poly([Head1]), poly(Tail2), poly(Tail4)),
    polytimes_call(poly(Tail1), poly([Head2 | Tail2]), poly(Tail5)),
    append(Tail4, Tail5, Tail3).


%%% multiply_variables/3
% This predicate sums the similar variables in a Monomial
multiply_variables([], [], []) :- !.
multiply_variables(V, [], V) :- !.
multiply_variables([], V, V) :- !.
multiply_variables([v(Exp1, Var) | Vs1], [v(Exp2, Var) | Vs2],
		               [v(Exp, Var) | Vs]) :-
    Exp is Exp1 + Exp2,
    !,
    multiply_variables(Vs1, Vs2, Vs).
multiply_variables([v(Exp1, Var1) | Vs1], [v(Exp2, Var2) | Vs2],
		               [v(Exp2, Var2) | Vs]) :-
    Var1 @>= Var2,
    !,
    multiply_variables([v(Exp1, Var1) | Vs1], Vs2, Vs).
multiply_variables([v(Exp1, Var1) | Vs1], [v(Exp2, Var2) | Vs2],
		               [v(Exp1, Var1) | Vs]) :-
    Var2 @>= Var1,
    !,
    multiply_variables(Vs1, [v(Exp2, Var2) | Vs2], Vs).

%%% mono_times/3
% This predicate multiplies the first Monomial by the second one
mono_times(m(0, _, _), m(_, _, _), m(0, 0, [])) :- !.
mono_times(m(_, _, _), m(0, _, _), m(0, 0, [])) :- !.
mono_times(M, [], M) :- !.
mono_times([], M, M) :- !.
mono_times(m(C1, TD1, VPs1), m(C2, TD2, VPs2), m(C, TD, VPs)) :-
    C is C1 * C2, !,
    TD is TD1 + TD2, !,
    multiply_variables(VPs1, VPs2, VPs).

%%% print_vps/1
% This predicate prints VarSymbol and Power of the VPs passed as argument
print_vps([]) :- !.
print_vps([v(1, Var)]) :-
    !,
    write(Var).
print_vps([v(Exp, Var)]) :-
    Exp \= 1,
    !,
    write(Var),
    write(^),
    write(Exp).
print_vps([v(Exp, Var) | VPs]) :-
    print_vps([v(Exp, Var)]),
    write(" * "),
    print_vps(VPs).

%%% pprint_polynomial/1
% This predicate deals with Poly printing on console after sorting it
pprint_polynomial(Poly) :-
    to_polynomial(Poly, PolyParsed), !,
    pprint_polynomial_call(PolyParsed).

%%% pprint_polynomial_call/1
% This predicate prints the Poly on the standard output
pprint_polynomial_call(poly([])) :-
    write("Polinomio vuoto!").
pprint_polynomial_call(poly([m(C, 0, [])])) :-
    write(C),
    !.
pprint_polynomial_call(poly([m(1, _TD, VPs)])) :-
    !,
    print_vps(VPs).
pprint_polynomial_call(poly([m(C, _TD, VPs)])) :-
    !,
    write(C), !,
    write(" * "), !,
    print_vps(VPs).
pprint_polynomial_call(poly([HeadMono | TailMono])) :-
    pprint_polynomial_call(poly([HeadMono])),
    write(" + "), !,
    pprint_polynomial_call(poly(TailMono)).
