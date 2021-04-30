/*-------------------------------------*
*          raison.pro                  *
*    gestion du graphe de raisonnement *
*                                      *
*--------------------------------------*/

/*-------------------------------------------------------------
  Notes sur le graphe de raisonnement :
     un sommet correspond a un element de relation
  ( ce que Buthion appelle une propriete),
     une hyperarete une derivation de proprietes.
  Un sommet est represente de la maniere suivante :
sommet(Numsom, Propriete atomisee, Nb util. act, Nb util. pas)
ou :
     Numsom est un numero attribue au sommet,
     Propriete est le resultat de l'atomisation,
     
  Une hyperarete :
deriv(Numar, Clause de Horn, Numero de la regle appliquee)
ou :
     Numar est un numero attribue a l'arete,
     Clause est de la forme :
       Numsom1 & Numsom2 & ... :> Numsom

  D'autre part, on calcule a chaque intervention dans le
graphe les composantes connexes de celui-ci au moyen de
la relation repccx/2 (qui est une projection) :
     Num repccx N
 signifie que Num est le representant de la composante 
connexe.
 Les composantes connexes sont utilisees pour la propagation
des nombres d'utilisations actives et passives.
-----------------------------------------------------------------*/

/*---------------------------------*
*   Projections ...                *
*----------------------------------*/
/*------ pour les sommets :        */
sommet_num(sommet(Num, _, _, _), Num).
sommet_prop(sommet(_, Prop, _, _), Prop).
sommet_nba(sommet(_, _, Nba, _), Nba).
sommet_nbp(sommet(_,_,_, Nbp), Nbp).

/*------ pour les deductions :     */
deriv_num(deriv(Num, _, _), Num).
deriv_clause(deriv(_, Clause, _), Clause).
deriv_n_regle(deriv(_, _, Nregle), Nregle).

/*---------------------------------*
*         insom/3                  *
*  inserer un sommet               *
*  le premier argument est la prop *
*  atomisee, le deuxieme, le num.  *
*  attribue au sommet dans le      *
*  graphe.
*  flux normal (i,o,o)             *
*----------------------------------*/

insom(Prop, Num, faux) :-                    /* tenter d'inserer un sommet deja      */
   Prop pequiv Propi,                  /* existant n'est pas synonyme d'       */
   sommet(Num,Propo,_,_),              /* echec de la regle...                 */
   Propi =.. [F|L],
   Propo =.. [F|Li],
   unif_synl(L,Li), !.
   
insom(Prop, Num, vrai) :- 
   gnum(som, Num),
   assert(sommet(Num,Prop,0,0)),
   assert(Num repccx Num).           /* repccx/2 composantes connexes du rais.  */

/*---------------------------------*
*        insar/4                   *
*    insertion d'une arete         *
*   flux normal (i,i,o)            *
*----------------------------------*/

insar(CHorn, Numregle, Num, faux) :-
     deriv(Num,CHorn,Numregle), !.      /* derivation deja faite (meme regle, memes premisses) */
     
insar(CHorn, Numregle, Num, vrai) :-
     gnum(har,Num),
     assert(deriv(Num,CHorn,Numregle)),
     connecte(CHorn).

/*---------------------------------*
*        connecte/1                *
*    calcul des nouvelles          *
*  composantes connexes.           *
*                                  *
*----------------------------------*/
connecte(Expr :> Numconc) :-
           (
             _ repccx Numconc,!
             ;
             assert(Numconc repccx Numconc)
           ),
             conn(Expr,Numconc).

/*---------------------------------*
*        conn/2                   *
*    organise suivant la liste    *
*  des numero de expression.      *
*  (effets de bord ...)           *
*                                 *
*---------------------------------*/
conn(N & R, NC) :-
    NP repccx N, 
    (
    NP repccx NC, !;
    remplrepccx(NP, NC)
    ),
    conn(R,NC),!.

conn(N , NC) :-
    NP repccx N, 
    (
    NP repccx NC, !;
    remplrepccx(NP, NC)
    ).

/*---------------------------------*
*        remplrepccx/2            *
*   change les representants des   *
*  composantes connexes concernees *
*----------------------------------*/
remplrepccx(NP,NC) :-
    NP repccx NC, !.

remplrepccx(NP,NC) :-
    NREP repccx NC,
    NREP \== NP,
    retract(NP repccx N),
    assert(NREP repccx N),
    fail.
remplrepccx(_,_).

/*---------------------------------*
*                                  *
*       recherche d'une propriete  *
*   dans le raisonnement.          *
*----------------------------------*/

/*---- En fonction de son foncteur principal :---*
*        rech_rais_crg/2                         *
*------------------------------------------------*/
/* rech_crg_abs ajoute PS2012  ... non present dans la version
sauvee pour une raison obscure
*/
rech_crg_abs(Fonct, sommet(Num, Prop, Nba, Nbp)) :-
    sommet(Num, Prop, Nba, Nbp),
    Prop =.. [Fonct|_].

rech_rais_crg(Fonct, sommet(Num, Prop, Nba, Nbp)) :-
        sommet(Num, Prop, Nba, Nbp),
        Prop =.. [Fonct|_],
        minact(Nbam), Nba =< Nbam,
        minpas(Nbpm), Nbp =< Nbpm.

/*-En fonction d'un nbr. max d'utilisations actives --*
*        rech_rais_act/2                              *
*-----------------------------------------------------*/
rech_rais_act(Nivmax, sommet(Num, Prop, Nba, Nbp)) :-
        sommet(Num, Prop, Nba, Nbp),
        Nba =< Nivmax.

rech_rais_act_pas(Niva, Nivp, sommet(Num, Prop, Nba, Nbp)) :-
        sommet(Num, Prop, Nba, Nbp),
        Nba =< Niva,
	Nbp =< Nivp.

/*-En fonction d'un nbr. max d'utilisations passives--*
*        rech_rais_pas/2                              *
*-----------------------------------------------------*/
rech_rais_pas(Nivmax, sommet(Num, Prop, Nba, Nbp)) :-
        sommet(Num, Prop, Nba, Nbp),
        Nbp =< Nivmax.

/*------------- general :                             */
choisir_sommet(Som) :-
    minpas(Np), minact(Na),
    rech_rais_act_pas(Na,Np,Som).


atom_sommet(At, sommet(Num, Prop, Na, Np)) :-
	sommet(Num, Prop, Na, Np),
	Prop =.. [_|Larg],
	appartient1(At, Larg).
	
/*------------------------------------------*
*        Mise a jour du graphe              *
*  de raisonnement :                        *
*        complete_raisona/3                 *
*        complete_raisonp/3                 *      
*        complete_suite_raison              *
*                                           *
*-------------------------------------------*/
/*-----------------------------------------------------____-*
*   Explications et révision                                *
*   révisions 2 avril 2021                                  *
*   prise en compte des règles à plusieurs prémisses        *
*    - 2 clauses pour complete_raisona et complete_raisonp  *     
*    - complete_suite_raison pour tenir compte des          *
*      prémisses supplémentaires.                           *
*                                                           *
*___________________________________________________________*/
complete_raisona(Prop : Deg, Clausepos & Suite, Numregle) :-
        (Clausepos \= _ & _,! ; write('probleme complete-raisona'), halt),
         insom(Prop, Nsom, Repsom),
         insar(Clausepos :> Nsom, Numregle, _, Repar),
         (
         Repar == faux, !, fail
         ;
         Repsom == vrai, !,ajoute_ut_act(Nsom, Deg),
			   ajoute_ut_pas(Clausepos),
               complete_suite_raison(Nsom, Suite, Numregle)
         ;
         true /* pas de controle .... */
         ).

complete_raisona(Prop : Deg, Clausepos, Numregle) :-
        (Clausepos \= _ & _,! ; write('probleme complete_raisona'), halt),
         insom(Prop, Nsom, Repsom),
         insar(Clausepos :> Nsom, Numregle, _, Repar),
         (
         Repar == faux, !, fail
         ;
         Repsom == vrai, !,ajoute_ut_act(Nsom, Deg),
			   ajoute_ut_pas(Clausepos)
         ;
         true		/* pas de controle .... */
         ).



complete_raisonp(Prop, Clausepos & Suite, Numregle) :-
         (Clausepos \= _ & _,! ; write('probleme complete_raisona'), halt),
         insom(Prop, Nsom, Repsom),
         insar(Clausepos :> Nsom, Numregle, _, Repar),
         (
         Repar == faux, !, fail
         ;
         Repsom == vrai, !, ajoute_ut_pas(Clausepos),
         complete_suite_raison(Nsom,Suite,Numregle)
         ;
         true		/* pas de controle .... */
         ).

complete_raisonp(Prop, Clausepos, Numregle) :-
        Clausepos \= _ & _,!,
         insom(Prop, Nsom, Repsom),
         insar(Clausepos :> Nsom, Numregle, _, Repar),
         (
         Repar == faux, !, fail
         ;
         Repsom == vrai, !, ajoute_ut_pas(Clausepos)
         ;
         true		/* pas de controle .... */
         ).


complete_suite_raison(Nsom, P & S, Numregle)  :-
    insar(P :> Nsom, Numregle, _, vrai),
    ajoute_ut_pas(P),
    complete_suite_raison(Nsom, S, Numregle).

complete_suite_raison(Nsom, P , Numregle)  :-
    P \= _ & _, !,
    insar(P :> Nsom, Numregle, _, vrai),
    ajoute_ut_pas(P).


/*----------------------------------------------*
*   Mise à jour des compteurs d'utilisation     *
*_______________________________________________*/
ajoute_ut_act(Nsom, Deg) :-
    Rep repccx Nsom, !,
    ajoute_ut_acti(Rep, Deg).

ajoute_ut_acti(Rep, Deg) :-
    Rep repccx Nsom,
    retract1(sommet(Nsom, Prop, Nba, Nbp)),
    NNba is Nba + Deg,
    assert(sommet(Nsom, Prop, NNba, Nbp)), fail.
    
ajoute_ut_acti(_,_).    

ajoute_ut_pas(Np1 & R) :-
	!,retract1(sommet(Np1,Prop, Nba, Nbp)),
	NNbp is Nbp + 1,
	assert(sommet(Np1,Prop,Nba,NNbp)),
	ajoute_ut_pas(R).

ajoute_ut_pas(Np) :-
	!,retract1(sommet(Np,Prop, Nba, Nbp)),
	NNbp is Nbp + 1,
	assert(sommet(Np,Prop,Nba,NNbp)).


retract1(Clause) :- retract(Clause), !. /* reste de l'utilisation d'un Prolog merdique */


