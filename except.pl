/*---------------------------------*
*        except.pro                *
*  Gestion des exceptions.         *
*----------------------------------*/


/*------------------------------------------------------------------*
*	gen_except/2			
* Gestion des exceptions. Le premier argument est la figure qui
* pose probleme, le deuxieme, en retour, indique le resultat du
* traitement en suivant le code :
*	- 'ok' on n'est pas dans un cas d'exception, la definition
*	  est valable.
*	- 'maldef' la definition n'est pas valable, 
*	- 'exceptions', le cas general est envisageable ainsi que
*	  les exceptions possibles.
*------------------------------------------------------------------*/
gen_except(fe(Nom,Ty,Deg,Def,Lr,Lp), Except) :-
	Nom eg Def 'except:' [Kg|Kps],
	(
	 mal_defini(Nom,Def), Except = maldef, !
	 ;
	 write(prouve_cas_gen), write(' '),write(Kg), nl, prouve_kg(Kg), Except = ok, !,
     /* LIGNE SUIVANTE AJOUTÉE PS2012 ; 
	     en principe les hypothèses du cas général on éte prouvées et ajoutés.*/
         Kg = (_ >> Lcons), assert_rel_part(Lcons)
	 ;
	 write(prouve_cas_NON_gen), nl,prouve_non_kg(Kg), Except = maldef, !
	 ;
	 Except = exceptions,!, write(alternative), declenche_except(fe(Nom,Ty,Deg,Def,Lr,Lp),[Kg|Kps])
	 )
	;
	 Except = ok.
	 
/*--------------------------------------------------------------*
* declenche_except/2											*
* Declencher une exception (lorsque Proge n'a pu ni				*
* 				demontrer, ni infirmer le cas general).			*
* Ce predicat agit par effet de bord sur la base de 			*
* faits.														*
* Les arguments sont la figure elementaire qui pose				*
* le probleme et la liste des cas associes a la def. le			*
* cas general en tete de liste.									*
*---------------------------------------------------------------*/

declenche_except(fe(Nom, Type, Deg, Def, Lr, Lpart), [Cg|Lp]) :-
	Cg = Lhyp >> Lcons,
	num_prog_cur(Num),
	gnum(num_pcg, Pgm1),
	gnum(num_pcg, Pgm2),
	abolishe num_prog_cur,
	assert(num_prog_cur(Pgm1)),
	assert(pcg(Num, si Lhyp alors Pgm1 sinon Pgm2)),
	(
	Def =.. [F|_],
	rmult(F), !,		
	gname(list,NL),
	gnum(num_pcg, Pgm),
	assert(pcg(Pgm1, NL := Def)),
	assert(pcg(Pgm1, pour Nom dans NL faire Pgm)),
	abolishe num_prog_cur,
	assert(num_prog_cur(Pgm))
	;
	assert(pcg(Pgm1, Nom := Def))
	),
	empile_config(Pgm2, Lp, fe(Nom, Type, Deg, Def, Lr, Lpart)),/* assure par la suite mal_def */
	assert_rel_part(Lhyp), 				/* pour les cas particuliers   */
	assert_rel_part(Lcons).

/*----------------------------------------------*
* manipulation des configuration :		*
* 	-recuperation d'une configuration	*
*	dans la db				*
*	- empilement, depilement,		*
*	- effacement de la db			*
*	 ...					*
*-----------------------------------------------*/

exist_config :- config(_,_,_,_,_,_,_,_,_,_), !.

/*----------------------------------------------*
* recuperations dans la base de faits		*
*-----------------------------------------------*/
recup_fe(Fl) :-		/* recuperation des faits fe	*/
	bagof(fe(N,T,Dl,Df,Lp,Lr),fe(N,T,Dl,Df,Lp,Lr),Fl), ! ; Fl = [].
recup_alias(Al) :-	/* recuperations des faits alias*/
	bagof(T1 alias T2, T1 alias T2, Al), ! ; Al = [].
recup_dc(Ldc) :-	/* recuperation des faits deja_construit*/
	bagof(deja_construit(N), deja_construit(N), Ldc), ! ; Ldc = [].
recup_som(Sl) :-	/* recuperation des faits sommets	*/
	bagof(sommet(Num, Prop, Nba, Nbp), sommet(Num, Prop, Nba, Nbp), Sl), !
	; Sl = [].
recup_der(Dl) :-	/* recuperation des faits deriv		*/
	bagof(deriv(Num,Clause, Nr), deriv(Num,Clause, Nr), Dl), ! ; Dl = [].
recup_rccx(Rl) :-	/* recuperation des faits repccx	*/
	bagof(R1 repccx R2, R1 repccx R2, Rl), ! ; Rl = [].
recup_cap(Cpl) :-	/* recuperation des faits rel_part	*/
	bagof(rel_part(Terme), rel_part(Terme), Cpl), ! ; Cpl = [].
recup_mad(Mdl) :-	/* recuperation des faits mal_def	*/
	bagof(mal_def(N,T), mal_def(N,T), Mdl), ! ; Mdl = [].


empile_config(Num_prog, Liste_des_cas, fe(Nom, Type, _, Def, Lr, Lp)) :-
	nl, write('--------> '), write(Num_prog), nl,
	point_arret(empile_config),
	recup_fe(Fl),
	dmax(Type,M),
	Fll = [fe(Nom, Type, M, [], Lr, Lp)|Fl],
	recup_alias(Al),
	recup_dc(Ldc),
	recup_som(Sl),
	recup_der(Dl),
	recup_rccx(Rl),
	recup_cap(Cpl),
	recup_mad(Mdl),
	Mdll = [mal_def(Nom, Def) | Mdl],
	asserta(config(Num_prog, Liste_des_cas, Fll, Al, Ldc,Sl,Dl,Rl,Cpl,Mdll)),!.


efface_config :- abolishl [fe,alias,deja_construit,sommet,deriv,repccxs,rel_part,mal_def].
/* 
la ligne suivante a été ajoutée pour "stimuler" un backtracking profond"
en SWI-Prolog. Il me semble en effet que Delphia Prolog, recommençait ce but
permettant à moteur de ne pas échouer ....
*/
efface_config :- fe(_,_,_,_,_,_), efface_config.


depile_config :- 
	retract(config(Num_prog, Lcas, Fl, Al, Ldc, Sl, Dl, Rl, Cpl, Mdl)),
	retablis_config(Num_prog, Fl, Al, Ldc, Sl, Dl, Rl, Cpl, Mdl), !,
/*
	nl, write('(depile_config) nouvelle configuration depilee '),
	write(Lcas),
	point_arret('depile_config'),
*/
	(
	 Num_prog = [_|Autres_num], !,
	 Lcas = cas([[] >> Concl1 | Autres_cas], Clausepos,Numr),
		(
		 Autres_num = [], !
		;
		 asserta(config(Autres_num, cas(Autres_cas, Clausepos, Numr), Fl, Al, Ldc, Sl, Dl, Rl, Cpl, Mdl))
		),
	 applique_concl(Concl1, Clausepos,Numr)
	;
	Lcas = cas(Restant,Clausepos,Numr), !,
	reexamine_cas(cas(Restant, Clausepos,Numr), Lhyp >> Conc, RRest),
	    (RRest = [] , !
	     ; 
	     gnum(num_pcg, Pgm1),
	     gnum(num_pcg, Pgm2),
	     abolishe num_prog_cur,
	     assert(num_prog_cur(Pgm1)),
	     assert(pcg(Num_prog, si Lhyp alors Pgm1 sinon Pgm2)),
	     asserta(config(Pgm2, cas(RRest, Clausepos, Numr), Fl, Al, Ldc, Sl, Dl, Rl, Cpl, Mdl))
	    ),
	applique_concl(Conc, Clausepos,Numr)
	;
	examine_lc(Lcas, Lhyp, Cas_restants),
	    (
	     Cas_restants = [], !
	     ;
	     gnum(num_pcg, Pgm1),
	     gnum(num_pcg, Pgm2),
	     abolishe num_prog_cur,
	     assert(num_prog_cur(Pgm1)),
	     assert(pcg(Num_prog, si Lhyp alors Pgm1 sinon Pgm2)),
	     asserta(config(Pgm2, Cas_restants, Fl, Al, Ldc, Sl, Dl, Rl, Cpl, Mdl))
	     )
	).


retablis_config(Num_prog, Fl, Al, Ldc, Sl, Dl, Rl, Cpl, Mdl) :-
	abolishe num_prog_cur,
	(
	Num_prog = [Num1 | _], !,
	assert(num_prog_cur(Num1))
	;
	assert(num_prog_cur(Num_prog))
	),
	listodb(Fl),		/* remarque : la premiere figure est peut etre a restaurer */
	listodb(Al),		/* ceci doit etre assure dans les cas d exception	   */
	listodb(Ldc),
	listodb(Sl),
	listodb(Dl),
	listodb(Rl),
	listodb(Cpl),
	listodb(Mdl), !.

listodb([P|S]) :- assert(P), !, listodb(S).
listodb([]).


/*--------------------------------------*
*	mal_defini/2			*
*---------------------------------------*/
mal_defini(Nom, Def) :-
	mal_def(N,D),
	synonymes(Nom,N),
	Def equiv D.

/*--------------------------------------------------------------*
*	assert_rel_part/1					*
*	assert_rp/1						*
*	assert_rp doit non seulement assurer la mise en place	*
*      des faits rel_part (sans les doubler), mais traiter l	*
*      incidence de ces relations particulieres sur la figure	*
*      (fusion en cas d egalite, nouvelle definition, nouvelles	*
*      relations d incidences ....				*
*---------------------------------------------------------------*/
assert_rel_part([P|S]) :- assert_rp(P), !, assert_rel_part(S).
assert_rel_part([]).

assert_rp(Rel) :- 
    Rel pequiv Relp,			/* remplacement de equiv (fonctionnel) par pequiv */
	rel_part(Relp), ! /* , write(Rel), nl, nl */
	; 
	traite_rp(Rel).

/*--------------------------------------------------------------*
*	traite_rp/1						*
*  Traitement des relations particulieres au moment du 		*
* depilement d'une configuration				*
*---------------------------------------------------------------*/

traite_rp(Arg1 eg Arg2) :- 
        (
         (atom(Arg1) ; construit(Arg1)), !,
         atomise(Arg1, Nom1),
         atomise(Arg2, Nom1)
         ;
         (atom(Arg2) ; construit(Arg2)),!,
         atomise(Arg2, Nom1),
         atomise(Arg1, Nom1)
         ;
         atomise(Arg1, Nom1),
         atomise(Arg2, Nom1)
         ),
         (
          atom(Arg1), construit(Arg1), not(atom(Arg2)),	/* rustine !!!!! */
         'maj:' Arg1 eg Arg2 ==> Lmaj,
         complete_figl(Lmaj,_)
         ;
         true
         ),
	assert(rel_part(Arg1 eg Arg2)).

traite_rp(A est_sur D) :-
	complete_fig(A est_sur D,Propa,_),
	assert(rel_part(Propa)).

traite_rp(def(O,NDeg, NDef)) :-
	retract(fe(O,T,_,[],Lr,Lp)),
	assert(fe(O,T,NDeg, NDef, Lr, Lp)).	/* bricolage !!!!! */
	
traite_rp(Rel) :- 			/* traitement par defaut	*/
	assert(rel_part(Rel)).          /* il faut peut-être atomiser exple A diff centre(C) */
                                        /* ou c'est fait après                               */


/*--------------------------------------------------------------*
*	examine_lc/2						*
* Examen de la liste des cas particuliers (premier argument)	*
* dans la configuration courante jusqu'a l'obtention d'un cas   *
* non contradictoire (ou valide ?). Le deuxieme argument est la *
* liste des cas particuliers restants non-examines.		*
*---------------------------------------------------------------*/
examine_lc([], _, _) :-
	num_prog_cur(Np),
	assert(pcg(Np, cas_contradictoire)),
	assert(trappe_echec), !, fail.

examine_lc([Cas1 | Suite], Hyps, Cas_restants) :-
	Cas1 = (Hyps >> Concs),
	non_contradictl(Hyps), !,		/* dans la configuration actuelle	*/
	Cas_restants = Suite,
	assert_rel_part(Hyps),
	assert_rel_part(Concs)
	;
	examine_lc(Suite, Hyps, Cas_restants).

/*------------------------------*
* 	non_contradl/1		*
*-------------------------------*/

non_contradictl([]) :- ! .
non_contradictl([Hyp1 | Suite]) :- not(contradiction(Hyp1)), !, non_contradictl(Suite).


contradiction(Hyp) :- rel_part(Rel), contradictoire(Hyp, Rel).


contradictoire(eg(M,N),diff(N,M)).
contradictoire(eg(M,N),diff(M,N)).
contradictoire(eg(M,N), diff(MM,NN)) :-
	synonymes(M,MM), synonymes(N,NN)
	;
	synonymes(M,NN),synonymes(N,MM).
contradictoire(diff(MM,NN), eg(M,N)) :- 
	contradictoire(eg(M,N), diff(MM,NN)).


/*------------------------------------------------------*
*	examine_cas/3					*
* Examine une liste de cas de figure dans la		*
* configuration courante.				*
* examine_cas(Lcas,Restants,Rep)			*
* Lcas est la liste de cas a examiner, l'examen		*
*   consiste a prouver la validite ou la 		*
*   contradiction (ou aucun des deux) du cas 		*
*   dans la configuration courante.			*
* Restants est la liste des cas, peut etre,		*
*   valide, le premier cas est bon si Rep = ok		*
*   ou Rep = cas_part.					*
*-------------------------------------------------------*/
examine_cas([],[],echec).

examine_cas([Prem|_], [Prem], ok) :-
	write('dans examine cas : appel prouve_kg *** '), prouve_kg(Prem), !.

examine_cas([Prem|Suite], Rest, Rep) :-
	write('dans examine cas : appel prouve_non_kg *** '),prouve_non_kg(Prem), !,
	examine_cas(Suite, Rest, Rep).

examine_cas([Prem|Suite], [Prem|Suite], cas_part) :-
       write('dans examine cas : sortie cas_part avec *** '), write([Prem|Suite]), nl.

/*------------------------------------------------------*
*	reexamine_cas/2					*
*							*
*-------------------------------------------------------*/
reexamine_cas(cas(Restant, _, _), Prem >> Conc, Rest) :-
	examine_cas(Restant, Reste, Rep),
	(
	  Rep == echec,
	  num_prog_cur(Np),
	  assert(pcg(Np, echec)),
	  assert(trappe_echec), !, fail
	 ;
	  Rep == ok, !,
	  Rest = []
	 ;
	  Rep == cas_part, !,
	  Reste = [Prem >> Conc | Rest] 
	; 
	  nl, write('(reexamine_cas) *** ATTENTION : mauvais retour de examine_cas : '),
	  write(Rep), nl, point_arret(reexamine_cas)
	),
	assert_rel_part(Prem).

	 
/*------------------------------------------------------*
*	declenche_cas_part/3				*
* declenche(CasRestants, Clausepos, NumRegle)		*
* gestion de la configuration dans le cas de detection	*
* de cas particuliers dans une regle exclusive.		*
* le premier cas de CasRestants est le cas en cours (	*
* donc a ne pas empiler).				*
*-------------------------------------------------------*/
 declenche_cas_part([Cg|Lp], ClausePos, Numr) :-
	Cg = Lhyp >> _,
	num_prog_cur(Num),
	gnum(num_pcg, Pgm1),
	gnum(num_pcg, Pgm2),
	abolishe num_prog_cur,
	assert(num_prog_cur(Pgm1)),
	assert(pcg(Num, si Lhyp alors Pgm1 sinon Pgm2)),
	empile_cas_part(Pgm2, Lp, ClausePos, Numr),
	assert_rel_part(Lhyp).




/*------------------------------------------------------*
*	empile_cas_part/3				*
*   Empile la configuration actuelle dans le cas d'un 	*
* declenchement par une regle disjonctive.		*
*-------------------------------------------------------*/
empile_cas_part(_, [],_,_) :- !.
empile_cas_part(Num, Restants, ClausePos, NumR) :-
	nl, write('--------> '), write(Num), nl,
	recup_fe(Fl),
	recup_alias(Al),
	recup_dc(Ldc),
	recup_som(Sl),
	recup_der(Dl),
	recup_rccx(Rl),
	recup_cap(Cpl),
	recup_mad(Mdl),
	asserta(config(Num, cas(Restants,ClausePos,NumR), Fl, Al, Ldc,Sl,Dl,Rl,Cpl,Mdl)),!,
	point_arret(empile_cas).

/*------------------------------------------------------*
*      prouve_kg/1					*
* Essaie de prouver le cas general.			*
* (Version assez sommaire au 4 fevrier 92)		*
* utilise prouve_ex/1 et prouve_ex_l/1			*
*-------------------------------------------------------*/
prouve_kg(Lhyps >> _) :-
	point_arret(prouve_kg), prouve_ex_l(Lhyps).
prouve_kg(Lhyps >> _) :- write('echec prouve_kg avec : '), write(Lhyps), nl, fail. 

prouve_ex_l([]) :- !.
prouve_ex_l([Deb| Suite]) :- prouve_ex(Deb),!, prouve_ex_l(Suite).

prouve_ex(X diff X) :- !, fail.		/* rustine ? */

/* différences explicitées (énoncé par exemple) */
prouve_ex(Obj1 diff Obj2) :- 		
	(rel_part(Obj1 diff Obj2 si Lcond) ; rel_part(Obj2 diff Obj1 si Lcond)),
	prouve_ex_l(Lcond),
	(Lcond = []; asserta(rel_part(Obj1 diff Obj2))),!.

prouve_ex(Obj1 diff Obj2) :-
	(rel_part(Obj1 diff Obj2); rel_part(Obj2 diff Obj1)),!,
	write(Obj1 diff Obj2),write(' a été utilisé'),nl.

/* application des règles (basiques) plus bas */
prouve_ex(Obj1 diff Obj2) :-
	(Obj1 diff Obj2 si Lcond; Obj2 diff Obj1 si Lcond),
	/* write('essai de preuve de '), write(Obj1 diff Obj2 si Lcond),nl,*/
	prouve_ex_l(Lcond),
	write('preuve de :'), write(Obj1 diff Obj2 si Lcond),nl, 
	!, write(Obj1 diff Obj2), write(' a été asserté'), nl,
	asserta(rel_part(Obj1 diff Obj2)). 


/* prouve_ex suite */

prouve_ex(N de type Ty) :-
	atom(N), xtype(N, Ty), !
	;
	ttype(N, Ty), !.	

prouve_ex(existe Var :: Type tel_que [P|S]) :-
	/* on peut chercher dans les représentants si c'est une égalité u tester 
	   tous les objet géométriques possibles
	   ou travailler globalement, c'est plus lourd, mais plus général
	*/
	not(var(Var)), !, write('dans prouve_ex (except) :'),
	                  write(Var),
					  write(' devrait être une variable'),nl
	;
	fe(Var,Type,_,_,_,_),
	assert(no_spam),
	prouve_ex_l([P|S]),
	retractall(no_spam)
	.

prouve_ex(existe Var :: Type tel_que Terme) :-
	/* on peut chercher dans les représentants si c'est une égalité ou tester 
	   tous les objet géométriques possibles
	   ou travailler globalement, c'est plus lourd, mais plus général
	*/
	not(var(Var)), !, write('dans prouve_ex (except) :'),
	                  write(Var),
					  write(' devrait être une variable'),nl
	;
	not(Terme = [_|_]), !
	;
	fe(Var,Type,_,_,_,_),
	assert(no_spam),
	prouve_ex(Terme),
	retractall(no_spam)
	.


prouve_ex(Rel) :-
	Rel =.. [F, T1, T2],
	ptitre(F, egalite),
	atomise(T1,N1),
	atomise(T2,N2),
	synonymes(N1, N2),!.
	
prouve_ex(Rel) :-
	Rel =.. [F, T1, T2],
	ptitre(F, incid),
	atomise(T1, N1),
	atomise(T2, N2),
	nom_fe(N2, Fig),
	fe_parts(Fig, Lp),
	appartient(N1/_/_, Lp),!.
	
	
		
prouve_ex(Rel) :-
	Rel =.. [F|Args],
	rel_part(Relb),
	Relb pequiv Relbb,
	Relbb =.. [F|Argbs],
	term_aliases(Args, Argbs), !.



prouve_ex(non_nul(_)) :- true.


prouve_ex(Rel) :-
	prouve_abs(Rel,_).


aliases([],[]) :- !.
aliases([P|S],[Pa|Sa]) :- synonymes(P,Pa), !, aliases(S,Sa).

term_aliases([],[]) :- !.
term_aliases([Pt|St], [Pta|Sta]) :- term_synonymes(Pt, Pta), !, term_aliases(St, Sta).

term_synonymes(T1, T2) :- 
    /* spy(assert), */
    atomise(T1, V1), atomise(T2, V2), synonymes(V1,V2).
    /*!, nospy(assert). */
/* term_synonymes(_, _) :- nospy(assert), fail. */
/*--------------------------------------------------------------*
*      prouve_non_kg/1						*
* Essaie de prouver que le cas general est faux			*
* (Version assez SOMMAIRE au 4 fevrier 92)			*
* utilise prouve_non_ex/1, prouve_non_exl/1 et contraire	*
*---------------------------------------------------------------*/

prouve_non_kg(Rels >> _) :- 
	prouve_non_exl(Rels).
prouve_non_kg(Lhyps >> _) :- write('echec prouve_non_kg avec : '), write(Lhyps), nl, fail.
	
prouve_non_exl([]) :- !.
prouve_non_exl([Deb|Suite]) :- prouve_non_ex(Deb), prouve_non_exl(Suite).

prouve_non_ex(Rel) :-
	contraire(Rel, Ler),		
	prouve_ex(Ler).

contraire(A eg B, A diff B).
contraire(A diff B, A eg B).

contraire(pll(A,B,C,D), pll(A,B,D,C)).
contraire(non_nul(K), nul(K)).
contraire(nul(K), non_nul(K)).
	


/*------------------------------------------------------*
*	Regles pour demontrer lors des exceptions	*
*	ou des cas particuliers.			*
*      Remarque : l'atomisation ne fonctionne pas       *
*         lorsque un terme n'est pas clos               *
*         comme dans A est_sur D plus bas               *
*         on cherche alors dans le raisonnement         *
*         qui est moins riche que la figure             *
*-------------------------------------------------------*/
/*

                ATTENTION : '=<type>=' utilise l'atomisation qui ne fonctionne pas 
				lorsque l'un des deux termes n'est pas clos.
				il faut donc revoir les règles ci-dessous !
				(work in progress)

*/

/* PS 02/2021 : */
/* on utilise deux nouveaux opérateurs : existe et tel_que */
/* qui vont permettre de chercher tous les B qui conviennent */
P1 diff G si [P1 de type point, G de type point, 
              existe B :: point tel_que P1 '=p=' symp(B,G)].

P1 diff G si [P1 de type point, G de type point, 
              existe B :: point tel_que P1 '=p=' symp(G,B)].

A diff G si [
			A de type point, 
			G de type point, 
			existe X :: point tel_que 
			existe Y :: point tel_que G '=p=' cgr(A,X,Y), X diff A, Y diff A]. 

A diff B si [A de type point, B de type point, 
             existe X :: point tel_que [A '=p=' mil(X,B),X diff B]].	/* note la liste pour pouvoir bien backtracker */
/*-----------------------------------------------------
	Les règles suivantes sont à revoir.
	Il faut mettre des 'existe' et des 'tel_que'
	Il peut y avoir beuaoucp de règles !
*------------------------------------------------------*/
A diff B si [A de type point, B de type point, A est_sur D, D de type droite, B est_sur Dp,
             Dp de type droite, dir(D) = dir(Dp), D diff Dp ].
A diff B si [A de type point, B de type point, dist(A,B) '=l=' K, non_nul(K)].
A diff B si [A de type point, B de type point, A '=p=' centre(C), B est_sur C].
A diff B si [A de type point, B de type point, A est_sur ccr(O,R1), B est_sur ccr(O,R2), R1 diff R2].
A diff B si [A de type point, B de type point, A est_sur D1, B est_sur D2, 
		D1 diff D2, dird(D1) '=di=' dird(D2)].

