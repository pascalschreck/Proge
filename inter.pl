/*------------------------------*
*	INTER						*
* Interprete de PGC pour Proge  *
*								*
*-------------------------------*/

/*--------------------------------------*
*	ptype/2								*
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


/*--------------------------------------------------------------------------*
*                     projet mars 2021 :									*
*         nettoyage d'un programme de construction							*
*																			*
*																			*
*___________________________________________________________________________*/





/*-------------------------------------------*
*  cherche_def/3							*
*  cherche_def(+Pgm, -Liste_var, -Liste_def)*
*  retourne la liste des variables			*
* et des définitions dans Pgm				*
*___________________________________________*/
cherche_def([],[],[]) :- !.

cherche_def([Obj := Def| Suite], [Obj|So],[Obj := Def|Autres] ) :-
	!, cherche_def(Suite,So, Autres).

cherche_def([si _ alors Pgm1 sinon Pgm2 | Suite], Lo, L) :-
	!, cherche_def(Pgm1, Lo1, L1),
	cherche_def(Pgm2, Lo2, L2),
	cherche_def(Suite, Los, Autres), /* normalement, après une alternative, il n y que les vérif */
	concat(L1,L2,L3), concat(Lo1,Lo2,Lo3),
	concat(L3, Autres, L), concat(Lo3,Los,Lo). 

cherche_def([pour Obj dans Liste faire Pgm | Suite], Lo, L) :-
	!, 
	cherche_def(Pgm, Lo1, L1),
	cherche_def(Suite, Lo2, L2),
	concat([Obj := Liste | L1], L2, L), 
	concat([Obj | Lo1],Lo2,Lo).

cherche_def([verifier(_) | Suite], Lo, L) :-
	!, cherche_def(Suite, Lo, L).
/* traitement si on ne sait pas faire ....*/
cherche_def(Pgm, [],[] ) :-
	!, write("cherche_def/3 : "),
	write(Pgm), write("\n structure non reconnue \n").


/*
enleve_inutil(+Ldef,+Lvar, -Linut, -Lutil)
appel : 
    - Ldef provient du programme de constr.
	- Lvar provient du programme de constr.
	- Linut est une variable
	- Lutil est instanciée au départ avec les objets cherchés ou mentionnés
sortie :
	- Linut : 
    - Lutil :
*/

enleve_inutil(_,[],[]) :- !.
enleve_inutil(Ldef, Lvar, Li) :-
	enleve_aux(Ldef,Lvar,Linut,[],Lut),
	(
		Linut  == [],!, Li = []
		;
		net_defs(Ldef, Linut, Ldefp),
		enleve_inutil(Ldefp, Lut, Lip),
		concat(Linut, Lip, Li)
	),!.

/*
    enleve_aux/5
	enleve_aux(+Ldef, +Lvar, -Linut,+Accu, -Lut)
	les variables de Lvar qui sont utiles vont dans Lut
	et celles qui sont inutiles dans Linut,
	Accu sert à construire Lut
	Linut est construite à la remontée
*/
enleve_aux(_, [], [], Lut, Lut) :- !.
enleve_aux(Ldef,[Fo | S], Ls, Accu, Lut) :- 
	util(Fo, Ldef),!,
	enleve_aux(Ldef, S, Ls, [Fo|Accu], Lut).
enleve_aux(Ldef, [Fo | S], [Fo | Ls], Accu, Lut) :-
	enleve_aux(Ldef, S, Ls, Accu, Lut), !.

/*
net_defs(Ldef, Linut, Ldefp)
*/
net_defs(Ldef,[], Ldef):- !.
net_defs(Ldef,[Fo|S], Ldefo) :-
	net_defs_aux(Ldef, Fo, Ldefp),!,
	net_defs(Ldefp, S, Ldefo).

net_defs_aux([],_, []) :- !.
net_defs_aux([Obj := _ | S], Obj, Sp) :- !, net_defs_aux(S, Obj, Sp).
net_defs_aux([Def | S], Obj, [Def | Sp]) :- net_defs_aux(S, Obj, Sp).

/* util/2 */
util(Obj, _) :-
		  lu(_ 'dec:' Declaration),
          ddecl(Declaration,Obj,_,Motif),
		  (Motif = cherche ; Motif = mentionne),
		  !.

util(Obj, [_ := T | S]) :- 
	T =.. [_|Largs], member(Obj,Largs), !
	;
	atom(T), Obj = T, !
	;
	util(Obj,S).




/*-------------------------
*     Nettoyage du pgm de construction
*		net_pcg(Pgm, Pgmp)
*_______________________________________*/

net_pcg(Pgm,Pgmp) :-
	cherche_def(Pgm, Lvars, Ldefs),
	write("liste des vars : "), write(Lvars), nl,
	write("liste des defs : "), write(Ldefs), nl,
	enleve_inutil(Ldefs, Lvars, Li),
	write("liste des vars inutiles : "), write(Li), nl,
	net_pcg_l(Pgm, Li, Pgmp).

/*
			net_pcg_l/3
*/
net_pcg_l(Pgm, [], Pgm) :- !.

net_pcg_l(Pgm, [Fo | Svar], Pgmp) :-
	net_pcg_aux(Pgm, Fo, Pgm1),!,
	net_pcg_l(Pgm1, Svar, Pgmp).

/*
		net_pcg_aux/3
*/
net_pcg_aux([],_,[]) :- !.
net_pcg_aux([Instr|Spgm], Fo, Pgmp) :-
		supprimable(Instr, Fo), !,
		write(Instr), write("supprimée avec "), write(Fo), nl,
		net_pcg_aux(Spgm, Fo, Pgmp).

net_pcg_aux([Instr|Spgm], Fo, [Instr | Pgmp]) :-
	net_pcg_aux(Spgm, Fo, Pgmp), !.

supprimable(Fo := _, Fo) :- !.
supprimable(si Cond alors _ sinon _, Fo) :-
	Cond =.. [_|Largs], member(Fo, Largs), !.

supprimable(pour C dans Fo faire _, Fo) :- util(C,[]),!,fail.