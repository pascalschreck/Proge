/*---------------------------------*
*        moteur.pro                *
*   moteur d'inference.            *
*                                  *
*----------------------------------*/

moteur(X) :- var(X), ! ,
		nl, write('strategie par defaut, essai_regle'), nl, 
		assert(cur_strat(essai_regle)),
		!,
		moteur(essai_regle), !.

moteur(Strat) :-
	point_arret('appel au moteur'), write(Strat),
	call(Strat), !, write('strategie : '), write(Strat), 
	 nl, write('un cas de construction a ete examine avec succes'), nl,
	 point_arret(' apres succes '),
	(
	 not(exist_config), !
	;
	 efface_config, 
         depile_config,
	 moteur(Strat)
	)
	;
	exist_config,
	num_prog_cur(N),
	assert(pcg(N, echec)),
	nl, write('(moteur) echec de cette branche '), write(N),nl,
	efface_config,
	depile_config, !,
	moteur(Strat).

moteur(_) :-
	nl, write('echec definitif'),
	num_prog_cur(N),
	assert(pcg(N,echec)).	/* sinon echec definitif  .... */
	

/*----- resolution dirigee par les regles :      *
*        essai_regle/0                           *
*------------------------------------------------*/

essai_regle :- act_pas_0, fini, !.

essai_regle :- 
         gen_minact_minpas,
         choisir_regle(Regle),
         regle_num(Regle, Nregle),
	 not(rout(Nregle)),
         regle_cond(Regle, [Pcond | _]),
         Pcond =.. [CRG|_],
         rech_rais_crg(CRG, Sommet),
	 sommet_num(Sommet, Ns),
  
	(
            not(inhib(Nregle, Ns)),
            applique(Regle, Sommet), /* si on veut visualiser, on fait echo */
           !
  	  ;
	    inhibe(Nregle, Ns), fail	
	 ),
         essai_regle.


/*---------- utilitaires pour essai_regle/0----*/

/*---------------------------------*
*		fini/0		   *
*----------------------------------*/
/*
*    Remarque :
* avec la gestion des cas particuliers ce predicat est a completer ...
*
*/
fini :- cas_fini, !, complete.
fini :- 
	trappe_echec, 
	abolishe trappe_echec,
	nl, write('(fini) echec de ce cas de figure'), nl.
	
cas_fini :- cherche(Nom), not(construit(Nom)),!,fail.
cas_fini.

/*------------------------------*
*	complete		*
* Version sommaire pour le      *
* moment (13/3/92)		*
*-------------------------------*/
complete :- complete1, complete2.
complete1 :-
	findall(O nomme T, lu(_ 'dec:' _ :: O nomme T), Ln),
        traite_complete1(Ln).

traite_complete1([]).
traite_complete1([O nomme T | Suite]) :-
       (
        construit(O), !
       ;
        num_prog_cur(Num), 
	assert(pcg(Num,O:= T)), 	/* il faudrait bien sur vrifier */
        assert(deja_construit(O))        /* que T est construit et que 	 */
       ),                              /* ses arg. figurent dans le prog*/
	traite_complete1(Suite).	
					
complete2 :-
	findall(O, lu(_ 'dec:' _ :: O mentionne), Lo),
	traite_complete2(Lo).

traite_complete2([]).

traite_complete2([O|Suite]) :-
        ( 
          construit(O), !  
        ;
	definir(O,T),
        num_prog_cur(Num),
	assert(pcg(Num,O:=T)),
        assert(deja_construit(O))
        ),
	traite_complete2(Suite).
     

definir(O,T) :-
        fe(O,Ty,Deg,Def,Reps, Parts),!,
        (
        Deg == 0, fail
        ;
        cherche_connu(Reps,T), !
        ;
        cherche_connus(Ty, Deg, Parts, Def, T), !
        ;
        nl, write('(defini) *** impossible de trouver une def de '),
	write(O), nl, T = nodef
        ).

cherche_connu([T|_], T) :- construit(T), !.
cherche_connu([_|S], T) :- cherche_connu(S,T).

cherche_connus(Ty, Deg, [P|S], L, T) :-
        majdef(Ty, Deg, L, P, Ndeg, NL),
        (
        Ndeg == 0, T = NL, !
        ;
        cherche_connus(Ty, Ndeg, S, NL, T), !
        )
        ;
        cherche_connus(Ty, Deg, S, L, T).



/*---------------------------------*
*  gestion des niveaux actifs et   *
* passifs admissibles.	           *
*----------------------------------*/
/*------ act_pas_0/0 : remise a zero de minact et minpas 	*/
act_pas_0 :- 
	abolishl [minact, minpas],
	assert(minact(0)), assert(minpas(0)).
  
gen_minact_minpas :-
	table_pas_act(_, Pas, Act),
	retract1(minact(_)),
	retract1(minpas(_)),
	assert(minpas(Pas)),
	assert(minact(Act)) /*, nl, write(N) */.

/*------- resolution dirigee par les proprietes :     *
*        essai_prop/0                                 *
*-----------------------------------------------------*/
essai_prop :- act_pas_0, fini, !.
essai_prop :-
	 gen_minact_minpas,
         choisir_sommet(Sommet),  /* possibilites de strategies differentes */
         sommet_prop(Sommet, Prop),
         pr_regle(Prop, Regle),   /* choix d'une regle possible pour cette propriete */
	 sommet_num(Sommet, Ns),
         regle_num(Regle, Nregle),
	 not(rout(Nregle)),
	(
            not(inhib(Nregle, Ns)),
            applique(Regle, Sommet), !
  	  ;
	    inhibe(Nregle, Ns), fail	
	 ),
          essai_prop.
       

/*---------------------------------------------------------------*
*        applique/2                				 *
*  Application d'une regle a un sommet. (Le reste des premisses  *
* etant a instancier dans le graphe de raisonnement.             *
*----------------------------------------------------------------*/
applique(Regle, Sommet) :-
    not(disjonctive(Regle)),
    sommet_num(Sommet, Ns),
    regle_num(Regle, Nregle),                    /* Numero de la regle         */
    regle_cond(Regle, [Pcond | Sconds]),     	 /* conditions geometriques    */
    regle_veri(Regle, Lav),                      /* Liste des verif. sur fig.  */
    regle_concl(Regle, Lconcl),                  /* Liste des conclusions      */
    sommet_prop(Sommet, Prop),
    !,
    unifie_prop(Pcond, Prop),
    write_regle(Regle),
    (
    Sconds == [],
    verifie_l(Lav),             /* si echec de la concl, echec de la regle */
    applique_concl(Lconcl, Ns, Nregle)
    ;
    Sconds \== [],
    prouve_l(Sconds, Clause_p),     /* Clause_p : conjonction de nø de sommets du raisonnement */
    verifie_l(Lav),             /* si echec de la concl, echec de la regle */
    applique_concl(Lconcl, Ns & Clause_p, Nregle)
    ),!,
    ((debog;bavard;step_regle;echo;spyre), write(Nregle), write(' ok'), ! ; true).

applique(Regle, Sommet) :-
    exclusive(Regle),
    sommet_num(Sommet, Ns),
    regle_num(Regle, Nregle),
    regle_cond(Regle, [Pcond | Sconds]),
    regle_veri(Regle, Lav),
    cas_exclusifs(Regle,Lconcl),
    sommet_prop(Sommet, Prop),
    !,
    unifie_prop(Pcond, Prop),
    write_regle(Regle),
    (
    Sconds == [],
    verifie_l(Lav),
    applique_exclu_concl(Lconcl, Ns, Nregle)
    ;
    Sconds \== [],
    prouve_l(Sconds, Clause_p),
    verifie_l(Lav),
    applique_exclu_concl(Lconcl, Ns & Clause_p, Nregle)
    ),!,
    ((debog;bavard;step_regle;echo;spyre), write(Nregle), write(' ok'), ! ; true).

applique(Regle, Sommet) :-
    non_exclusive(Regle),
    sommet_num(Sommet, Ns),
    regle_num(Regle, Nregle),
    regle_cond(Regle, [Pcond | Sconds]),
    regle_veri(Regle, Lav),
    cas_possibles(Regle,Lconcl),
    sommet_prop(Sommet, Prop),
    !,
    unifie_prop(Pcond, Prop),
    write_regle(Regle),
    (
    Sconds == [],
    verifie_l(Lav),             /* si echec de la concl, echec de la regle */
    applique_mult_concl(Lconcl, Ns, Nregle)
    ;
    Sconds \== [],
    prouve_l(Sconds, Clause_p),     /* Clause_p : conjonction de no de sommets du raisonnement */
    verifie_l(Lav),             /* si echec de la concl, echec de la regle */
    applique_mult_concl(Lconcl, Ns & Clause_p, Nregle)
    ),!,
    ((debog;bavard;step_regle;echo;spyre), write(Nregle), write(' ok'), ! ; true).



 
/*---------------------------------*
*        unifie_prop/2             *
*    unifie une premisse avec      *
*  une propriete dans le graphe    *
*  de raisonnement.                *
*                                  *
*----------------------------------*/

unifie_prop(Premisse, Prop) :-
         Premisse pequiv Prem,
         Prem =.. [CRG|Largs],
         Prop =.. [CRG|Lats],
         unif_repl(Largs, Lats)     /* unif_repl dans util.pro */
         .


/*---------------------------------*
*        prouve/2                  *
*  Instancie une premisse a l'aide *
* de la configuration courante.    *
*  mode (i,o).                     *
*----------------------------------*/
prouve(Premisse, Num_som) :-
         Premisse =.. [CRG|_],
         rech_rais_crg(CRG, Sommet),   /* chercher d'abord les sommets les moins */
         sommet_prop(Sommet, Prop),    /* utilises activement et passivement     */
         unifie_prop(Premisse, Prop),
         sommet_num(Sommet, Num_som).

/* idem, mais sans consideration des utilisations passees */         
prouve_abs(Premisse, Num_som) :-
         Premisse =.. [CRG|_],
         rech_crg_abs(CRG, Sommet),  
         sommet_prop(Sommet, Prop),  
         unifie_prop(Premisse, Prop),
         sommet_num(Sommet, Num_som).
 
         
/*---------------------------------*
*        prouve_l/2                *
* Comme prouve/2, mais pour des    *
* listes :                         *
* le deuxieme argument est de la   *
* forme num1 & num2 & ...          *
*----------------------------------*/
prouve_l([Pt|S],Numsomt & Suite) :- 
         S \== [],
         prouve(Pt, Numsomt), 
         prouve_l(S,Suite).
prouve_l([Pt],Numsomt) :-
         prouve(Pt, Numsomt).


/*---------------------------------*
*        verifie/1                 *
* Verifications sur la figure      *
* langage externe d'interrogation  *
* de la figure (peut etre a        *
* completer.                       *
*----------------------------------*/
verifie(connu(X)) :- construit(X), !.
verifie(pas_connu(X)) :- not(construit(X)), !.
verifie(cherche(X)) :- not(construit(X)), !.
verifie(type(X, Typ)) :- ttype(X, Typ); atom(X), xtype(X, Typ), !.
verifie(X incid Y) :- fe(X,_,_,_,_,Lpart), app_titre_syn(Y/incid, Lpart), !. /* peut etre a changer ...*/
verifie(deg_lib(X,Deg)) :- fe(X,_,_,Deg,_,_), !.
verifie(differents L) :- tous_dif_rep(L).

/*-------------------------------------*
*        verifie_l/1                   *
* Comme verifie/1, mais pour une liste *
*--------------------------------------*/
verifie_l([P|S]) :- verifie(P), !, verifie_l(S).
verifie_l([]).

/*--------------------------------------------------------------------------*
*        applique_concl/3               				    *
*        applique_conc/3                				    *
* Applique la liste conclusion (arg1) en faisant les actions explicites     *
* sur la figure ( operateur ':'), les actions implicites (par atomisation   *
* et mise a jour) et en completant le graphe de raisonnement. A cet effet,  *
* on conserve les nø de sommets correspondant aux premisses (arg2)    	    *
* et le numero de la regle appliquee (arg3).                                *
* TOUS les arguments doivent etre instancies au moment de l'appel.	    *
*---------------------------------------------------------------------------*/

applique_concl([Pconc|Sconc], Clausepos, Numregle) :-
         applique_conc(Pconc, Clausepos, Numregle), !,
         applique_concl(Sconc, Clausepos, Numregle).
applique_concl([],_,_).

/* applique_conc(T,_,_) :- write('***** applique_conc avec ***** '), write(T), nl, point_arret.*/

applique_conc( Nom nomme Terme,_,_) :- !, atomise(Terme, Nom).   /* trappe discutable ... */
applique_conc(tinhibe(Num),_,_) :- !, assert(rout(Num)).

applique_conc(Prop : Deg, Clausepos, Numregle) :-
          complete_fig(Prop,Prop_at, Rep), Rep = vrai,
          complete_raisona(Prop_at : Deg, Clausepos, Numregle), ! , 
	  desinhibe_prop(Numregle, Prop_at).


applique_conc(Prop, Clausepos, Numregle) :-
         not( Prop = (_ : _)), 
         complete_fig(Prop,Prop_at, _),
         complete_raisonp(Prop_at, Clausepos, Numregle), ! ,
	 desinhibe_prop(Numregle, Prop_at) .
 
/*----------------------------------------------------------------------*
*	applique_exclu_concl/3						*
* appliquer une conclusion dans le cas d'une regle disjonctive, les	*
* arguments sont les memes que pour applique_concl/3, mais le premier	*
* argument est une liste de termes de la forme List_cond >> List_concl  *
* correspondant aux cas de figures possibles et aux conclusions		*
* appropriees								*
*-----------------------------------------------------------------------*/
applique_exclu_concl(Lcas, Clausepos,NumR) :-
          examine_cas(Lcas, [Bon_cas|Restants], Rep),
          (
           Rep == ok, ! 
           ;
           Rep == echec, !, fail
           ;
           Rep == cas_part, !, 
           declenche_cas_part([Bon_cas|Restants], Clausepos, _)
           ;
           nl, 
           write('(applique_exclu_concl)*** ATTENTION code de retour inconnu de examine_cas : '),
           write(Rep),
           nl, point_arret(applique_exclu_concl)
           ),
           Bon_cas = _ >> Lconc,
           applique_concl(Lconc, Clausepos,NumR), !.
           
applique_mult_concl([[] >> Concl1 | Autres], Clausepos, Numr) :-
	concl([[] >> Concl1|Autres],Concls),
	concl2hyp(Concls, Lcas), 
	examine_cas(Lcas,_,Reponse), write(Reponse), point_arret(applique_mult_concl), Reponse == cas_part,
	num_prog_cur(Num),
	gname(list,NL),
	gnum(num_pcg, Pgm),
	assert(pcg(Num, NL := Concls)),
	gname(cas, Cas),
	assert(pcg(Num, pour Cas dans NL faire Pgm)),
	abolishe num_prog_cur,
	gen_list_pcg(Concls,List_num),
	List_num = [PN|Num_suiv],
	assert(num_prog_cur(PN)),
	assert_pcg_mult(Pgm, Cas, Concls, List_num),
	empile_cas_part(Num_suiv, Autres, Clausepos, Numr),
	applique_concl(Concl1, Clausepos, Numr).

assert_pcg_mult(_, _, [], []):- !.
assert_pcg_mult(Nump, Cas, [Pconc|Suitec], [Pnum|Suiten]) :-
	assert(pcg(Nump,si Cas eg Pconc alors Pnum)),
	assert_pcg_mult(Nump, Cas, Suitec, Suiten).

gen_list_pcg([],[]):- !.
gen_list_pcg([_|S],[Pnum|Suite]) :-
	gnum(num_pcg,Pnum),!,
	gen_list_pcg(S,Suite).

           

/*---------------------------------*
*        complete_fig/1            *
*----------------------------------*/
/* complete_fig(tinhibe(N), thinhibe(N),faux) :- !. *//* trappé avant   */
complete_fig(Prop, Prop_at, vrai) :-
         Prop =.. [CRG, Arg1, Arg2],
         ptitre(CRG, egalite),
         (
         (atom(Arg1) ; construit(Arg1)), !,
         atomise(Arg1, Nom1),
         atomise(Arg2, Nom1),
         Prop_at =.. [CRG, Nom1, Nom1]
         ;
         (atom(Arg2) ; construit(Arg2)),!,
         atomise(Arg2, Nom1),
         atomise(Arg1, Nom1),
         Prop_at =.. [CRG, Nom1, Nom1]
         ;
         atomise(Arg1, Nom1),
         atomise(Arg2, Nom1),
         Prop_at =.. [CRG, Nom1, Nom1]
         ),
         (atom(Arg1), construit(Arg1), not(atom(Arg2)),	/* rustine !!!!! */
         'maj:' Arg1 eg Arg2 ==> Lmaj,
         complete_figl(Lmaj,_);
         true
         ),
	(
         Arg2 = _ :: Cond,
         assert_rp(Cond)
         ;
         true
         ).
/* la rustine precedente pallie le manque de reciprocite dans le
*  cas Di '=di=' dird(D) de la regle 4.
*  ( autrement, D n'est pas mise a jour)
*/         
         

complete_fig(Prop, Prop_at, Rep) :-
         Prop =.. [CRG, Arg1, Arg2],
         ptitre(CRG, incid),
         atomise(Arg1, Nom1),
         atomise(Arg2, Nom2),
         nom_fe(Nom1, Fe),
         xtype(Nom2, Ty2),
         ajoute_partl1([Nom2/Ty2/incid], Fe, Fe2), !,
         (
         Fe \== Fe2,                    /* si pas de changement, echec */
         retract(Fe),
         assert_fig(Fe2), Rep = vrai,
/*  ajout PS2012   */
         nom_fe(Nom2, Fer),
         xtype(Nom1,Ty1),
         ajoute_partl1([Nom1/Ty1/incid], Fer, Fer2), !,
         (Fer \== Fer2, !, retract(Fer), assert_fig(Fer2) ; true)
 /* fin de l'ajout ... */
         ;
         Fe == Fe2, Rep = faux
         ),
         Prop_at =.. [CRG, Nom1, Nom2].

complete_fig(Prop, Prop_at, vrai) :-
         Prop =.. [CRG|L],
         atomisel(L, Lat),
         Prop_at =.. [CRG|Lat].



complete_figl([P|S],[Pa|Sa]) :-
	complete_fig(P,Pa,_),!,
	complete_figl(S,Sa).
complete_figl([],[]).

table_pas_act(0,0,0).
table_pas_act(1,2,0).
table_pas_act(2,4,0).
table_pas_act(3,6,0).
table_pas_act(4,8,0).
table_pas_act(5,2,1).

table_pas_act(6,3,1).

table_pas_act(7,2,2).

table_pas_act(8,3,2).

table_pas_act(9,3,3).

table_pas_act(10,4,3).

/*
table_pas_act(11,5,3).
table_pas_act(12,6,3).
table_pas_act(13,7,3).
table_pas_act(14,8,3).
*/

/*------------------------------------*
	inhibe/2
	desinhibe/2
--------------------------------------*/
inhibe(NRegle, NSommet) :-
	( inhib(NRegle, NSommet), !; assert(inhib(NRegle, NSommet))).

desinhibe_prop(Numregle, Prop_at) :-
	Prop_at =.. [_|Larg],
	desinhibel(Numregle, Larg).

desinhibel(Numregle, [P|S]) :-
	desinhibe(Numregle, P), !,
	desinhibel(Numregle, S).
desinhibel(_, []).

desinhibe(_, P) :-
	atom_sommet(P, Sommet),
	sommet_num(Sommet, Num),
	retract(inhib(_, Num)),
	fail.

desinhibe(_,_).
 
