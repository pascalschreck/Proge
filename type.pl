/*--------------------------------*
*        type.pro                 *
* description des types utilises  *
* dans proge                      *
*---------------------------------*/

/*--------------------------------*
     dmax/2
  degre de liberte 
  maximum pour le type donne
*---------------------------------*/

/* ps septembre 2022*/
dmax(triangle, 6) :- !.
/* fiin ajout sept. 2022*/

dmax(point,2):- !.
dmax(droite,2):- !.
dmax(cercle,3):- !.
dmax(long,1):- !.
dmax(reel,1):- !.
dmax(dir,1):- !.
dmax(angle,1):- !.
dmax(vecteur,2) :- !.
dmax(translation,2) :- !.
dmax(homothetie,3) :- !.
dmax(rotation,3) :- !.
dmax(T,0) :- write('(dmax) *** ERREUR 0 : '), 
             write(T), 
             write(' type non envisage'), nl.
             

/*----------------------------------*
      autom/4
 constructions automatiques
 affectees a un type.
 autom(type,nom,objets a construire,nouv. representants)
------------------------------------*/
/* ps septembre 2022*/
autom(triangle,_, [],[]) :- !.	/* dans le cas de Wernik, on pourrait automatiquement */
								/*considérer  des choses, par eexemple les cotés*/
/* fiin ajout sept. 2022*/
autom(point,_,[],[]):- !.
autom(droite,D,[dir :: Di nomme dird(D)], [dpdir(nul, Di)]) :- !.
autom(cercle,C,[point :: O nomme centre(C),long :: R nomme rayon(C)],[ccr(O,R)]):- !.
autom(homothetie, H,[point :: O nomme centrh(H), long :: K nomme rapport(H)],[hcr(O,K)]):- !.
autom(rotation, R, [point :: O nomme centrr(R), angle_vo :: A nomme angr(R)],[rca(O,A)]):- !.
autom(long,_,[],[]):- !.
autom(reel,_,[],[]):- !.
autom(dir,_,[],[]):- !.
autom(angle,_,[],[]):- !.
autom(T,_,[],[]) :- write('(autom) *** ERREUR 0 : '), 
             write(T), 
             write(' type non envisage'), nl.
            
/*----------------------------------*
     princip/2
  constructeurs principaux lies
  a un type.
------------------------------------*/
/* ps septembre 2022*/
princip(triangle, [tri]) :- !.
/* fiin ajout sept. 2022*/
princip(point,[interdd,intercd,interdc,intercc]):- !.
princip(droite,[dro,dpdir]):- !.
princip(cercle,[ccr,ccp,cr2p,ccir]):- !.
princip(long,[]):- !.
princip(reel,[]):- !.
princip(dir,[dird]):- !.
princip(angle_no,[]):- !.
princip(homothetie, [hcr, hcpp, hcdd]) :- !.
princip(T,[]) :-write('(princip) *** ERREUR 0 : '), 
             write(T), 
             write(' type non envisage'), nl.

/*--------------------------------------*
*	type_coord/2			*
* Donne un prototype du terme 		*
* "coordonnees" en fonction du type.	*
*---------------------------------------*/
/* ps septembre 2022*/
type_coord(triangle, tri(p(_,_), p(_,_),p(_,_))) :- !.  /* ? */
/* fiin ajout sept. 2022*/
type_coord(point,p(_,_)).
type_coord(droite,d(_,_,_)).
type_coord(cercle,c(p(_,_),_)).
/* .....  prevoir alors les projections (pour acceder aux coords) ? .... */
/*--------------------------------------*
*	dessinable/1			*
*---------------------------------------*/
dessinable(point).
dessinable(droite).
dessinable(cercle).
dessinable(segment).	/*----?????----*/
dessinable(triangle).	/*----ajouté sept. 2022----*/
dessinable(polygone).	/*----?????----*/

/*--------------------------------------*
*    	rep_par/2			*
*  Representation parametrique d'un	*
* type.					*
----------------------------------------*/
rep_par(point,p(_,_)).
rep_par(droite,d(_, _, _)).
rep_par(cercle,c(p(_,_), _)).
rep_par(long, l(_)).
rep_par(dir, di(_)).	/* D : reel ou +- infini */
rep_par(angle,a(_)).

dessine(Nom, p(X,Y)) :-
	coord_ec(X,Y,Xe, Ye),	/* transformation des coord. reelles en coord. ecran	*/
	HGx is Xe - 2,
	HGy is Ye - 2,
	HDx is Xe + 2,
	HDy is Ye - 2,
	BGx is Xe - 2,
	BGy is Ye + 2,
	BDx is Xe + 2,
	BDy is Ye + 2,
	Xn is HDx + 3,
	Yn is HDy - 3,
	tbw_draw_line(0,HGx,HGy,BDx,BDy,1,0),
	tbw_draw_line(0,HDx,HDy,BGx,BGy,1,0),
	tbw_write_canvas(0,Xn,Yn,Nom,2), !.

dessine(_, d(A,B,C)) :-
	point_bas(A,B,C,Xb,Yb),
	point_haut(A,B,C,Xh,Yh),
	tbw_draw_line(0,Xb,Yb,Xh,Yh,1,0).
	/* prevoir d'afficher le nom de la droite ... */

dessine(_, c(p(X,Y),R)) :-
	factK(K),
	Re is integer(R*K),
	coord_ec(X,Y,Xe,Ye),
	tbw_draw_circle(0,Xe,Ye,Re,1,0,-1).

/*----------------------------------------*
*	saisie				  *
*-----------------------------------------*/

saisie(point, Nom, p(X,Y)) :-
	nl, write('**** saisie de : '), write(Nom), write(' (point)'), nl,
	tbo_get_event(E,[tb_click_down_wdw, tb_ms_move_wdw]),
	s_traite_event(E, p(Xe,Ye)),
	coord_ec(X,Y,Xe,Ye),
	dessine(Nom,p(X,Y)).


saisie(droite, Nom, d(A,B,C)) :-
	nl, write('**** saisie de : '), write(Nom), write(' (droite)'),
        nl, write('----> premier point '), nl, 
	tbo_get_event(E1,[tb_click_down_wdw, tb_ms_move_wdw]),
	s_traite_event(E1, p(Xe1,Ye1)), 
	write(' ok'),
	coord_ec(X1,Y1,Xe1,Ye1), dessine(' ',p(X1,Y1)),
	nl, write('----> deuxieme point...'), nl,
	tbo_get_event(E2,[tb_click_down_wdw, tb_ms_move_wdw]),
	s_traite_event(E2, p(Xe2,Ye2)),
	coord_ec(X2,Y2,Xe2,Ye2), dessine(' ',p(X2,Y2)),
	write(' ok'), nl,
	A is Y1 - Y2,
	B is X2 - X1,
	C is -A*X1 - B*Y1.

saisie(cercle, Nom, c(p(X1,Y1), R)) :-
	nl, write('**** saisie de : '), write(Nom), write(' (cercle)'),
        nl, write('----> centre '), nl, 
	tbo_get_event(E1,[tb_click_down_wdw, tb_ms_move_wdw]),
	s_traite_event(E1, p(Xe1,Ye1)), 
	write(' ok'),
	coord_ec(X1,Y1,Xe1,Ye1), dessine(' ',p(X1,Y1)),
	nl, write('----> point sur cercle...'), nl,
	tbo_get_event(E2,[tb_click_down_wdw, tb_ms_move_wdw]),
	s_traite_event(E2, p(Xe2,Ye2)),
	coord_ec(X2,Y2,Xe2,Ye2),
	write(' ok'), nl,
	R is sqrt((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2)),
	dessine(Nom,c(p(X1,Y1),R)).

saisie(long, Nom, l(V)) :-
	nl, write('**** saisie de : '), write(Nom), write(' (longueur)'),
        nl, write('----> premier point '), nl, 
	tbo_get_event(E1,[tb_click_down_wdw, tb_ms_move_wdw]),
	s_traite_event(E1, p(Xe1,Ye1)), 
	write(' ok'),
	coord_ec(X1,Y1,Xe1,Ye1), dessine(' ',p(X1,Y1)),
	nl, write('----> deuxieme point...'), nl,
	tbo_get_event(E2,[tb_click_down_wdw, tb_ms_move_wdw]),
	s_traite_event(E2, p(Xe2,Ye2)),
	coord_ec(X2,Y2,Xe2,Ye2), dessine(' ',p(X2,Y2)),
	write(' ok'), nl,
	V is sqrt((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2)).


s_traite_event([tb_ms_move_wdw,_,_,_, _], p(X, Y)) :-
	tbo_get_event(E,[tb_click_down_wdw,tb_ms_move_wdw]), !,
	s_traite_event(E, p(X, Y)).
s_traite_event([tb_click_down_wdw,_,_,X,Y,_], p(X, Y)) :-
	 !.

dessine_l([valeur(N,P)|S]) :- 
	nl, write(N),nl,
	(
	  ptype(N,T),dessinable(T), !, dessine(N,P)
	 ; 
	  true
	),
	!, dessine_l(S).

dessine_l([]).
