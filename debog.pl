/*----------------------------------------------*
*	DEBOG.PRO				*
*-----------------------------------------------*/


/*----------------------------------------------*
*	quelques outils de debogage		*
*						*
*-----------------------------------------------*/
write_regle(Regle) :- spyre,
	regle_num(Regle,Num),!,
	(
	spyre(Num),write(Regle), nl, !, get_str1(S), 
	traite_debog(S)
	;
	true
	).
	
write_regle(Regle) :-
	debog, nl, write(Regle), nl, !,
	get_str1(S),traite_debog(S).
write_regle(Regle) :- step_regle, nl, write(Regle), nl, getrc, !.
write_regle(Regle) :- bavard, nl, write(Regle), nl, !.
write_regle(Regle) :- echo, regle_num(Regle,N), nl, write(N), !.
write_regle(_).

sreg(L) :- assert_spyl(L),(spyre,!; assert(spyre)).
bav :- bavard, ! ; assert(bavard).
r2r :- step_regle, !; assert(step_regle).
deb :- debog, ! ; assert(debog).
ech :- echo, !; assert(echo).
cool :- abolishl([echo, bavard,step_regle, debog]).
pa :- retract((point_arret :- !)),
	retract((point_arret(_) :- !)), fail.
pa.
spa :- asserta((point_arret :- !)),
	asserta((point_arret(_) :- !)).

assert_spyl([]).
assert_spyl([P|S]) :- assert_spy(P), assert_spyl(S).

assert_spy(NumR) :- spyre(NumR), !; assert(spyre(NumR)).

traite_debog("a") :- abort, !.
traite_debog("n") :- cool, !.
traite_debog("t") :- trace, !.
/* TODO check si c est une typo
taite_debog("c") :- !.
taite_debog("f") :- !, fail.
*/
traite_debog("b") :- nl,
		write('sos break> '),
		read(Ordre),
		( Ordre == fin, !
		;
		  Ordre == fail, !,
		  fail
		;
		  (call(Ordre), !, write(Ordre), nl ; write(' rate mec ...'), nl),
		  traite_debog("b")
		).

traite_debog("s") :- shell, !,nl, write('point d''arret : '),
		get_str1(S),traite_debog(S).

traite_debog("h") :- nl,
		write(' a : abort'), nl,
		write(' n : fin trace debog'), nl,
		write(' t : trace Prolog'), nl,
		write(' b : sos break'), nl,
		write(' s : geom_shell'), nl,
		write(' c : continue'), nl, !,
		nl, write('point d''arret : '),
		get_str1(S),traite_debog(S).


point_arret(_) :- batchmode, !.
point_arret(Mess) :-
	nl, write('point d''arret ( '), write(Mess),write(' )'),
	get_str1(S),traite_debog(S).

point_arret :- batchmode, !.
point_arret :-
	nl, write('point d''arret ( '), write(anonyme),write(' )'),
	get_str1(S),traite_debog(S).
