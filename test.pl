/*--------------------------------------*
*	fichier test			*
* predicats de deroulement et interface *
* pour l'interrogation de la figure 	*
* numerique.				*
*  ( Version de test ...)		*
*---------------------------------------*/

/*--------------------------------------*
* test/0 : test pour l'evaluation des   *
* COG.					*
* test/2 utilise par test/0 et par les  *
* predicat d'interrogation de la figure.*
* ttraite/3 predicat d'interpretation 	*
* des commandes utilisees par test/2	*
*---------------------------------------*/
test :- test([],_).
test(Le,Lsd):- write('nom : '), read(Nom), nl,ttraite(Nom,Le,Ls), !, test(Ls,Lsd).
test(_,_).

ttraite(fin,_,_) :- !, fail.


ttraite(Nom, Le, Ls) :-
	write('def : '), read(Terme), nl,
	(
	  Terme == donne, !,
	  write('type : '), read(Type),
	  saisie(Type, Nom, Coord)
	; 
	  evalue(Terme, Le, Coord),
	  ttype(Terme,Type)
	),
	(
	 dessinable(Type), !, dessinel(Nom, Coord)
	 ;
	  true
	),
	Ls = [valeur(Nom, Coord) | Le].
 
/*--------------------------------------*
* 	vide_tp_evt/0			*
* Vide le tampon des evenements de la	*
* fenetre 0. Tous les evenements sont 	*
* lus est ignores jusqu'a la frappe du	*
* caractere espace dans la fenetre 0.	*
*---------------------------------------*/
vide_tp_evt.

/*--------------------------------------*
*	dessinel/2							*
* Dessine pour une liste (Cf graph)		*
*---------------------------------------*/
dessinel(N,[P|S]) :- dessine(N,P), !, dessinel(N,S).
dessinel(_,[]) :- !.
dessinel(N,T) :- dessine(N,T).




/*--------------------------------------*
*	roule/0				*
* Enchainement des predicats de lecture	*
* de l'enonce, de resolution, de l'	*
* interpretation et lancement de la 	*
* boucle d'attente de commande.		*
*---------------------------------------*/
roule :- lis_decl, lis_cont, lis_cnd, !, moteur(_), !,
	pcg_bdf_l(_,Pgm),
	gname(pgm_int, P),
	asserta(sauve(P, Pgm)), !,
	nl, write(' r: '), get_str1(S),!,
	traite_req(S, Pgm, _), !.

/*--------------------------------------*
*	continue/0							*
* Reprend la boucle d'interpretation 	*
* avec le PCG en cours.					*
*---------------------------------------*/
continue :- 
	sauve(_, Pgm),
	!,
	nl, write(' r: '), get_str1(S),
	traite_req(S, Pgm, _), !.

/*--------------------------------------*
*	traite_req/3			*
* Predicat gerant l'interpretation des	*
* commandes d'interpretation du PCG et	*
* d'exploration de la figure.		*
*---------------------------------------*/
traite_req("f",_,_) :- !.
traite_req("q",_,_) :- !.
traite_req("Q",_,_) :- !.
traite_req("F",_,_) :- !.

traite_req("x",Pgm,L) :-
	nl, write(' nom : '), read(Nom),
	evall(Nom, L, V), write('---> '), write(V),
	nl, write(' r: '), get_stri1(S), !,
	traite_req(S, Pgm, L).
traite_req("X", Pgm, L) :- !, traite_req("x", Pgm, L).

traite_req("E", Pgm, L) :- shell,
	nl, write(' r: '),get_str1(S), !,traite_req(S, Pgm, L).
traite_req("e", Pgm, L) :- shell, 
	nl, write(' r: '),get_str1(S), !,traite_req(S, Pgm, L).

traite_req("p", Pgm, L) :- gname(pgm_int, P),
	asserta(sauve(P, Pgm)),
	nl, write(' r: '),
	get_str1(S), !,traite_req(S, Pgm, L).

/* TODO check if typo
traite_rep("r", _, L) :-
	sauve(P, Pgm), !,
	nl, write(P), nl,
	nl, write(' r: '),
	get_str1(S), !,traite_req(S, Pgm, L).

traite_rep("r",Pgm,L) :-
	nl, write(' r: '),
	get_str1(S), !,traite_req(S, Pgm, L).
*/

traite_req("k", Pgm, L) :-
	abolishe sauve,
	nl, write(' r: '), 
	/* get1(C), !, list([C], S),*/ /* ancienne version incompatible avec swipl */
	get_str1(S), !,
	traite_req(S, Pgm, L).

traite_req("a", Pgm, L) :-
	aff_prog(Pgm, 0),
	nl, write(' r: '),
	get_str1(S), !,traite_req(S, Pgm, L).

traite_req("c", Pgm, L) :-
	net_pcg(Pgm,Pgmp),
    aff_prog(Pgmp, 0),
	nl, write(' r: '),
	get_str1(S), !,traite_req(S, Pgm, L).

traite_req(_, Pgm, L) :-
	nl, write(' Aide : '),
	nl, write('d	 : dessine '),
	nl, write('s     : solution suivante'),
	nl, write('p	 : solution precedente'),
	nl, write('c	 : nettoyer le programme courant'),
	nl, write('i	 : nouvelle interpretation (complete)'),
	nl, write('x	 : coordonnees'),
	nl, write('e	 : shell'),
	nl, write('p	 : sauver (temporairement) un programme'),
	nl, write('r	 : recuperer un programme'),
	nl, write('a	 : afficher le programme'),
	nl, write('k	 : kill programmes sauves'),
	nl, write('w	 : écrire le programme dans un fichier'),
	nl, write('f,q	 : quitter'),
	nl, write(' r: '),
	get_str1(S), !,traite_req(S, Pgm, L), !.
	
/*--------------------------------------*
*	Utilitaires :			*
*  aff_prog/2 : affichage d'un PCG.	*
*---------------------------------------*/
aff_prog([(OG := T)| Suite], N) :- 
	nl, tab(N), write(OG := T), !, aff_prog(Suite, N). 
aff_prog([(pour O dans L faire Pgm) | Suite], N) :-
	nl, tab(N), writel(['pour ', O,' dans ',L,' faire ']),
	NN is N + 3,
	aff_prog(Pgm, NN), !,
	aff_prog(Suite, N).
aff_prog([(si Cond alors P1 sinon P2) | Suite], N) :-
	nl, tab(N), writel(['si ',Cond,' alors']),
	NN is N+3,
	aff_prog(P1,NN),
	N1 is N+1,
	nl, tab(N1), write(sinon),
	aff_prog(P2,NN), !,
	aff_prog(Suite, N).

aff_prog([(si Cond alors P1 ) | Suite], N) :-
	nl, tab(N), writel(['si ',Cond,' alors']),
	NN is N+3,
	aff_prog(P1,NN),
	aff_prog(Suite,N).

aff_prog([echec|Suite], N) :-
	nl, tab(N), write(echec), !, aff_prog(Suite, N).

aff_prog([verifier(Cond)|Suite], N) :-
	nl, tab(N), write(verifier(Cond)), !, aff_prog(Suite,N).

aff_prog([],N) :-
	nl, tab(N), write(fin).

