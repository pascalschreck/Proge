/*---------------------------------*
*         COG.PRO                  *
*   description des COG            *
*----------------------------------*/

/*----------------------------------------------------------*

     Differentes "connaissances" doivent etre
  associees aux foncteurs determinant des 
  objets geometriques (COG) :
   - le profil du constructeur (en particulier son
     type),
   - sa decomposabilite en participants et, le cas
     echeant, les titres de participation des args.
   - les permutations possibles des arguments,
   - sa faculte de definir un nouveau type d'objet 
     ( p.e. vecteur) ou une construction elementaire
     (on donne alors la construction en fonction de
     constructeurs de base),
   - les exeptions liees a la definition d'un objet
     par ce foncteur,
   - les mises a jour automatiques (en particulier
     d'appartenance).

*------------------------------------------------------------*/

/*---------------------------------*
*         profil/2                 *
*                                  *
*                                  *
*----------------------------------*/
:- multifile(profil/2).

/* Wernick */

profil(cgr, point x point x point >> point) :- !.
profil(ccc, point x point x point >> point) :- !.
profil(ort, point x point x point >> point) :- !.

/* Remarque : pour le moment, (mars 2021) 
   les cog pour Wernick e sont pas complétement
   définis. Il manque :
   * les exceptions
   * les règles
   * les construction implicites (? éventuellement)
   * ... 
*/

profil(interdd, droite x droite >> point):- !.
profil(intercd, cercle x droite >> point):- !.
profil(interdc, droite x cercle >> point):- !.
profil(intercc, cercle x cercle >> point):- !.
profil(mil, point x point >> point):- !.
profil(prj, point x droite >> point):- !.
profil(centre, cercle >> point):- !.
profil(symp, point x point >> point):- !.
profil(symd, droite  x point >> point):- !.
profil(homp, homothetie x point >> point):- !.	/* image par une homothetie 	*/
profil(rotp, rotation x point >> point):- !.	/* image par une rotation   	*/
profil(centrh, homothetie >> point):- !.	/* centre d'une homothetie	*/
profil(centrr, rotation >> point):- !.		/* centre d'une rotation	*/

profil(dro, point x point >> droite):- !.
profil(dpd, droite x long >> droite):- !.
profil(dpp, droite x point >> droite):- !. /* droite parallèle passant par un point */
profil(med, point x point >> droite):- !.
profil(dpdir, point x dir >> droite):- !.
profil(dorth, droite x point >> droite):- !. /* droite perpendiculaire passant par un point */
profil(bis, droite x droite >> droite):- !.
profil(dmd, droite x droite >> droite) :- !.		/* "milieu" de deux droites */
profil(homd, homothetie x droite >> droite):- !.	/* image par une hom.	*/
profil(rotd, rotation x droite >> droite):- !.		/* image par une rot.	*/

profil(ccr, point x long >> cercle):- !.
profil(ccp, point x point >> cercle):- !.
profil(cr2p, long x point x point >> cercle):- !.
profil(cdiam, point x point >> cercle):- !.
profil(ccir, point x point x point >> cercle):- !.
profil(homc, homothetie x cercle >> cercle):- !.
profil(rotc, rotation x cercle >> cercle):- !.

profil(dist, point x point >> long):- !.
profil(did, point x droite >> long):- !.
profil(rayon, cercle >> long):- !.
profil(rapport, homothetie >> long) :- !.	/* en attendant ....	*/

profil(+, long x long >> long):- !.
profil(-, long x long >> long):- !.
profil(/, long x long >> long):- !.    /* !!!!! en attendant    */
profil(*, long x long >> long):- !.    /* !!!!! en attendant    */

profil(angdd, droite x droite >> angle_no):- !.
profil(angr, rotation >> angle_vo) :- !.	

profil(dird, droite >> dir):- !.
profil(dirpp, point x point >> dir):- !.    /* ??? utile ???    */
profil(diro, dir >> dir):- !.
profil(dira, dir x angle >> dir):- !.

profil(hcr, point x long >> homothetie):- !.
profil(hcpp, point x point x point >> homothetie):- !.		/* centre, point, image */
profil(hcdd, point x droite x droite >> homothetie) :- !.	/* centre, droite, image	*/

profil(rca, point x angle >> rotation):- !.

profil(T, inc >> inc) :- nl,
                  write(' (profil) *** COG : '),
                  write(T),
                  write(' non reconnu ***'), nl,
                  point_arret(profil).

/*---------------------------------*
*	rmult/1			   *
* Le COG en argument definit en    *
* general plusieurs objets geom    *
*----------------------------------*/
rmult(intercd) :- !.
rmult(interdc) :- !.
rmult(intercc) :- !.
rmult(dpd) :- !.
rmult(bis) :- !.
rmult(cr2p) :- !.

/*---------------------------------*
*        ttype/2                   *
*  Type d'une construction elem.   *
*----------------------------------*/
ttype(T, Ty) :-
    T =.. [F | _],
    profil(F, _ >> Ty).

xtype(Nom, Ty) :-
    synonyme(Nom, Nomb), fe(Nomb, Ty, _,_,_,_).
    
/*---------------------------------*
*         decomp/2                 *
*                                  *
*----------------------------------*/
/*--------------------------------------------------
          Note sur les COG decomposables :
     il me semble que pour un COG decomposable,
   les arguments doivent forcement etre differents,
   et il faut donc en tenir compte pour les 
   exeptions (voir plus bas) ...
----------------------------------------------------*/   
decomp(interdd,[droite/incid, droite/incid]):- !.
decomp(interdc,[droite/incid, cercle/incid]):- !.
decomp(intercd,[cercle/incid, droite/incid]):- !.
decomp(intercc,[cercle/incid, cercle/incid]):- !.
decomp(dro,[point/incid, point/incid]):- !.
decomp(dpdir,[point/incid, dir/incid]):- !.
decomp(dird, [droite/incid]) :- !.
decomp(ccr,[point/centre, long/rayon]):- !.
decomp(ccp,[point/centre, point/incid]) :- !.
decomp(cr2p,[long/rayon, point/incid, point/incid]) :- !.
decomp(ccir,[point/incid,point/incid,point/incid]):- !.
decomp(hcr,[point/centrh, long/rapport]):- !.
decomp(hcpp,[point/centrh, point/ante, point/image]) :- !.
decomp(hcdd,[point/centrh, droite/ante, droite/image]) :- !.
decomp(rca,[point/centre, angle/angle]):- !.

decomposable(Terme) :-        /* Terme est-il decomposable ?     */
     functor(Terme,F,_),
     decomp(F,_).

decomposable(Terme, L_titre) :-
    Terme =.. [F|Largs],   
    decomp(F,Lt),
    associe_arg_titre(Largs, Lt, L_titre).

associe_arg_titre([X|S], [_|St], Sti) :- 
    X == nul, !, associe_arg_titre(S,St,Sti).
associe_arg_titre([X|S], [Ty/Ti|St], [X/Ty/Ti|Sti]) :-
    !, associe_arg_titre(S,St,Sti).
associe_arg_titre([],[],[]).  
/*---------------------------------*
*         permut/1                 *
*----------------------------------*/
permut(intercc):- !.
permut(interdd):- !.
permut(mil):- !.
permut(dro):- !.
permut(med):- !.
permut(bis):- !.
permut(cdiam):- !.
permut(ccir):- !.
permut(dist):- !.
permut(angdd):- !.
permut(+):- !.
permut(*):- !.

/* Wernick */
permut(cgr) :- !.
permut(ort) :- !.
permut(ccc) :- !.


/*---------------------------------*
*        equiv/2                   *
*   Ne pas couper ...              *
*----------------------------------*/

Terme equiv Terme.
/*
interdd(D1,D2) equiv interdd(D2,D1).
intercd(D1,D2) equiv interdc(D2,D1).
interdc(D1,D2) equiv intercd(D2,D1).
intercc(D1,D2) equiv intercc(D2,D1).
*/
cgr(A, B, C) equiv cgr(A, C, B).
cgr(A, B, C) equiv cgr(B, A, C).
cgr(A, B, C) equiv cgr(B, C, A).
cgr(A, B, C) equiv cgr(C, A, B).
cgr(A, B, C) equiv cgr(C, B, A).

ort(A, B, C) equiv ort(A, C, B).
ort(A, B, C) equiv ort(B, A, C).
ort(A, B, C) equiv ort(B, C, A).
ort(A, B, C) equiv ort(C, A, B).
ort(A, B, C) equiv ort(C, B, A).

ccc(A, B, C) equiv ccc(A, C, B).
ccc(A, B, C) equiv ccc(B, A, C).
ccc(A, B, C) equiv ccc(B, C, A).
ccc(A, B, C) equiv ccc(C, A, B).
ccc(A, B, C) equiv ccc(C, B, A).

mil(A,B) equiv mil(B,A).
/*
dro(A,B) equiv dro(B,A).
*/
med(A,B) equiv med(B,A).
bis(D1,D2) equiv bis(D2,D1).
dmd(D1,D2) equiv dmd(D2,D1).
cdiam(A,B) equiv cdiam(B,A).
dist(A,B) equiv dist(B,A).
angdd(D1,D2) equiv angdd(D2,D1).  /* angles de droites non orientes */
X + Y equiv Y + X.
X * Y equiv Y * X.

/*-----------------------------------------------------*
*   equivdef/2   utilisé pour def_fe dans figure .pl
*  ajout tiré de correct.pl    --PS2012
*------------------------------------------------------*/

A equivdef B :- A equiv B.
dro(A,B) equivdef dro(B,A).
cr2p(R,A,B) equivdef cr2p(R,B,A).
interdd(D1,D2) equivdef interdd(D2,D1).
intercd(D1,D2) equivdef interdc(D2,D1).
interdc(D1,D2) equivdef intercd(D2,D1).
intercc(D1,D2) equivdef intercc(D2,D1).
/*---------------------------------*
*         constr/2                 *
*                                  *
*----------------------------------*/
constr(interdd(_,_), base(point)):- !.
constr(intercd(_,_), base(point)):- !.
constr(interdc(_,_), base(point)):- !.
constr(intercc(_,_), base(point)):- !.
constr(mil(_,_), interdc(a_completer)):- !.  /*------ a completer -----*/
constr(prj(_,_), interdc(a_completer)):- !.  /*------ a completer -----*/       
constr(dro(_,_),base(droite)):- !.

/*====================== A COMPLETER ==============================*/

/*---------------------------------*
*         except:/2                *
*                                  *
*----------------------------------*/
/*=====================================================================*
 formalisme donne a titre provisoire :
Le premier argument de 'except:' est une egalite de la forme 
	Nom eg Terme
le second argument est une liste L de termes de la forme
	Liste de Cond >> Liste des consequences
 le premier element de la liste L correspond au cas general, 
 Cond decrit les conditions pour que la definition contextuelle
correspondant a Terme soit valable, la liste de consequences indique
quelles relations particulieres on peut en deduire.
 Les elements suivants de L decrivent des cas particuliers, le formalisme 
est le meme, mais cette fois la figure elementaire de nom Nom ne peut
pas etre definie par Terme (ceci sera note une fois pour toute
dans les configurations suivantes a l'aide du predicat mal_def/2 ...).
*======================================================================*/

/*-------- essai des cas particuliers avec le COG 'dro'--------------------*/
M eg mil(A,B) 'except:' [[A diff B] >> [ M diff A, M diff B], [A eg B] >> []] :- !.
C eg cdiam(A,B) 'except:' [[A diff B] >> [A diff centre(C), B diff centre(C)], [A eg B] >> []] :- !.
H eg prj(M,D) 'except:' [[M hors_de D] >> [], [M est_sur D] >> [H eg M]] :- !.
N eg symp(M,O) 'except:' [[M diff O] >> [], [M eg O] >> [N eg M]] :- !.

D eg dro(A,B) 'except:' [ [A diff B] >> [], [A eg B] >> [def(D,1,[B/point/incid])]] :- ! .
_ eg med(A, B) 'except:' [[A diff B] >> [], [A eg B] >> []] :- !.

_ eg cpp(O,M) 'except:' [[M diff O] >> [], [M eg O]>>[]] :- !.

/*---------------------------------*
*         maj:/1                   *
*    Mises a jour automatiques     *
*                                  *
*----------------------------------*/
/*------------------------------------------
     Note sur maj:
   Les regles suivantes, prefixees par maj:
  sont des regles particulieres en ce sens
  qu'on a envie qu'elles soient appliquees
  systematiquement lors de la creation (ou
  la mise a jour) d'un objet par un repre-
  sentant. D'autre part, il est peut etre 
  souhaitable qu'elles ne laissent pas de
  trace dans le raisonnement pour ne pas 
  le compliquer inutilement.
--------------------------------------------*/
  

'maj:' M eg mil(A,B) ==> [M est_sur dro(A,B)] :- !.
'maj:' H eg prj(_,D) ==> [H est_sur D] :- !.
'maj:' M eg symp(A,B) ==> [M est_sur dro(A,B)] :- !.
'maj:' M eg homp(H,P) ==> [M est_sur dro(chom(H),P)] :- !.
'maj:' B eg bis(D1,D2) ==> [interdd(D1,D2) est_sur B] :- !.
'maj:' C eg cdiam(A,B) ==> [A est_sur C, B est_sur C,centre(C) '=p=' mil(A,B)  ] :- !.
'maj:' D eg dorth(_,P) ==> [ P est_sur D] :- !.
'maj:' X est_sur ccr(O,R) ==> [R '=l=' dist(O,X)] :- !.
'maj:' Di eg dird(D) ==> [D est_sur Di] :- !.

/* Wernick  */
'maj:' G eg cg(A,B,C) ==> [G est_sur dro(A,mil(B,C)), G est_sur dro(B,mil(A,C)), G est_sur dro(C,mil(A,B))] :- !.


'maj:' _ ==> [] :- !.		/* par defaut ... */

/*=================== A COMPLETER ================================*/

/*------------------------------------------------------------------------*
    participants
Un participant est constitue 
	d'un nom,
	d'un type,
	et d'un titre de participation
ceci dans le format : Nom/Type/Titre
	(!!!! attention !!!! l'associativite de / a ete changee !!!!!!).
Les trois predicats suivants sont les projections canoniques des 
participants.
*------------------------------------------------------------------------*/

titre(_/_/Tit, Tit).
nomp(Nom/_/_, Nom).
typp(_/Typ/_, Typ).

/*---------------------------------------------*
	recip/3
  reciprocite des titres de participation,
avec le mode d'application de la reciproque.
----------------------------------------------*/
recip(incid, incid, participant).
recip(centre, centre, representant).
recip(rayon, rayon, representant).     /* ???????? */

/*   ajout PS2012 ... pas encore exploité  */
recip_augmente(X est_sur ccr(O,R), O est_sur ccr(X,R)).

/*--------------------------------------------*
	deg_titre/2
 Degre de participation d'un titre de
 participation.
----------------------------------------------*/
deg_titre(incid, 1).
deg_titre(centre, 2).
deg_titre(rayon, 1).
deg_titre(dir, 1).


