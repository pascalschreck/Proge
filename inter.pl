/*------------------------------*
*	INTER			*
* Interprete de PGC pour Proge  *
*				*
*-------------------------------*/

/*--------------------------------------*
*	ptype/2				*
*---------------------------------------*/
ptype(OG,T) :- decl(T, OG).

/*--------------------------------------*
*	appartient_circ/2		*
*---------------------------------------*/	
appartient_circ(_,[]) :- !, fail.
appartient_circ(V,[V]) :- !.
appartient_circ(Val, Lv) :- appartient(Val, Lv).

/*--------------------------------------*
*	pcg_bdf_l/2			*
* Transformation du programme de constr.*
* geometrique contenu dans la base de   *
* faits en l'objet (liste) programme de *
* construction geometrique.		*
*---------------------------------------*/
pcg_bdf_l(Num, L) :-
	pcg_bdf_l1(Num, L1),
	pcg_bdf_l2(L2),
	concat(L1, L2, L).

pcg_bdf_l1(Num, [Inst | Lsuiv]) :-
	retract(pcg(Num, Terme)), !,
	(
	 Terme = echec, !, Inst = echec
        ;
         Terme = cas_contradictoire, !, Inst = cas_contradictoire
	;
	  Terme = (_ := _), !, Inst = Terme
	;
	  Terme = (pour I dans L faire Pgm), !,
	  pcg_bdf_l1(Pgm, Lfaire),
	  Inst = (pour I dans L faire Lfaire)
	;
	  Terme = (si Cond alors Pgm1 sinon Pgm2), !,
	  pcg_bdf_l1(Pgm1, Lpgm1),
	  pcg_bdf_l1(Pgm2, Lpgm2),
	  Inst = (si Cond alors Lpgm1 sinon Lpgm2)
	;
	  Terme = (si Cond alors Pgm1), !,
	  pcg_bdf_l1(Pgm1, Lpgm1),
	  Inst = (si Cond alors Lpgm1)
	),
	pcg_bdf_l1(Num, Lsuiv).

pcg_bdf_l1(_,[]).

pcg_bdf_l2([verifier(Cond) | Suite]) :-
	retract(lu(_ 'cont:' Cond)), !,
	pcg_bdf_l2(Suite).

pcg_bdf_l2([]).
