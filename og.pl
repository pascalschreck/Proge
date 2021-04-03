/*---------------------------------*
          OG.PRO
   definition contextuelle
   objet geometrique
   figures elementaires
-----------------------------------*/

/*
                                                                   
    figure elementaire :                                           

     Une figure elementaire est constituee d'un objet geometrique  
   et de sa representation.                                        
     En pratique, nous n'utiliserons qu'une structure ayant pour   
   foncteur principal fe :                                         
   fe(nom, type, dø de lib, definition context., repres., partic.) 
                                                                   
    definition contextuelle :                                      

     Il y a deux types de definitions :                            
          -les definitions incompletes sont essentiellement        
        des listes de participants (liste eventuellement vide),    
          -une definition complete est un representant (terme      
        de profondeur 1, clos et dont le foncteur principal est    
        un constructeur d'objet geometrique.                       
                                                                   
   Attention : On doit se preoccuper des exceptions lorsqu'on      

   obtient une definition complete.                                
                                                                   
*/

/* 
     figures elementaires :
     projection               */
     
is_fe(fe(_,_,_,_,_,_)).
fe_nom(fe(Nom,_,_,_,_,_),Nom).
fe_type(fe(_,Typ,_,_,_,_), Typ).
deg_lib(fe(_,_,D,_,_,_), D).
fe_def(fe(_,_,_,Def,_,_), Def).
fe_reprs(fe(_,_,_,_,Lr,_), Lr).
fe_parts(fe(_,_,_,_,_,Lp), Lp).

/*--------------------------------------------------
    Recherche d'un representant (terme de prof. 1)
  dans la  representation :
    appartient_rep/2 :
  appartient_rep(Terme,Liste) cherche Terme
 dans la liste Liste des representants
    appartient_part/2 
 appartient_part(Terme,Liste) recherche les arguments 
 de Terme dans la liste des participants.

   Ces deux predicats doivent pouvoir travailler 
  en mode (i,i) ou (o/i,i).

*--------------------------------------------------*/

appartient_rep(Rep, [R1|_]) :-
    unif_rep(Rep, R1).       

appartient_rep(Rep, [_ | S]) :- appartient_rep(Rep, S).

appartient_part(Rep, Lparts) :- 
    Rep =.. [Fonc|Li],
    copylist(Li, Lvar),
    Rep_bis =.. [Fonc|Lvar],
    decomposable(Rep_bis, Ltit),
    inclu_eff_syn(Ltit, Lparts),
    unif_rep(Rep, Rep_bis).

copylist([],[]) :- !.
copylist([_|S],[_|Sv]) :- copylist(S,Sv).


inclu_eff_syn([A/T|S], Lparts) :-
         efface_syn(A/T, Lparts, Lparts2),
         inclu_eff_syn(S,Lparts2).
inclu_eff_syn([],_).         
    
efface_syn(A/T, [Ap/T|S], S) :- A alias R, Ap alias R.     /* pas coupe */
efface_syn(A/T, [AT | S], [AT|S2]) :- efface_syn(A/T, S, S2).

/*------------------------------------------*
*        ajoute_rep/3                       *
*   ajoute_rep(Terme, Fe1, Fe2) :           *
* Ajoute le terme Terme  la representation *
* de Fe1 pour donner Fe2, ceci dans la      *
* figure courante (mise  jour de la def.)  *
*                                           *
* pre : Terme est clos de prof. 1           *
*================================           *
*                                           *
*-------------------------------------------*/
ajoute_rep(Terme, FeE, FeS) :-
    not(decomposable(Terme)), !,
    ajoute_repr(Terme, FeE, FeS).
ajoute_rep(Terme, FeE, FeS) :-
    decomposable(Terme, Ltit),
    ajoute_parts(Ltit, FeE, FeS).

/*---------------------------------*
*        ajoute_repl/3             *
*  Comme ajoute_rep, mais pour     *
* une liste de termes  ajouter    *
*----------------------------------*/
ajoute_repl([PT|TS], FeE, FeS) :-
    ajoute_rep(PT,FeE, Fe1), !,
    ajoute_repl(TS, Fe1, FeS).
ajoute_repl([], Fe, Fe). 

/*---------------------------------*
*        ajoute_repr/3             *
*----------------------------------*/
ajoute_repr(Terme, fe(Nom, Ty, D, Def, Reps, Parts), Fe) :-
         appartient_rep(Terme, Reps), !,
         Fe = fe(Nom, Ty, D, Def, Reps, Parts)
     ;
         D == 0, !,
         Fe = fe(Nom, Ty, D, Def, [Terme|Reps], Parts)
     ;
         construit(Terme), !,
         Fe = fe(Nom, Ty, 0, Terme, [Terme|Reps], Parts)
     ;
         Fe = fe(Nom, Ty, D, Def, [Terme|Reps], Parts)
     .

/*---------------------------------*
*        ajoute_repr_l/3           *
*----------------------------------*/
ajoute_repr_l([Rep1|S], Fee, Fef) :-
    ajoute_repr(Rep1, Fee, Fei), !,
    ajoute_repr_l(S, Fei, Fef).
    
ajoute_repr_l([],Fe, Fe).
    
/*---------------------------------*
*        ajoute_parts/3            *
*----------------------------------*/
ajoute_parts(Ltit, fe(Nom, Ty, D, Def, Reps, Parts), Fe) :-
           D == 0, !,
           ajoute_partl0(Nom, Ltit,Parts, NParts),
           Fe = fe(Nom, Ty, D, Def, Reps, NParts)
          ;
           ajoute_partl1(Ltit,fe(Nom, Ty, D, Def, Reps, Parts), Fe).
    
/*---------------------------------*
*        ajoute_partl0/4           *
*----------------------------------*/           
ajoute_partl0(Nom, [Part1|SP], Parts, NParts) :-
    app_titre_syn(Part1, Parts),!,
    ajoute_partl0(Nom, SP, Parts, NParts)
    ;
    maj_recip(Part1, Nom), !,               /* mise  jour reciproque de Part1 */
    ajoute_partl0(Nom, SP, [Part1|Parts], NParts).
    
ajoute_partl0(_, [], LP, LP).

/*---------------------------------*
*        app_titre_syn/2            *
*----------------------------------*/
app_titre_syn(P/T/Ti, [PP/T/Ti|_]) :-
         P alias R, PP alias R.
app_titre_syn(P/T/Ti, [_ | S]) :- app_titre_syn(P/T/Ti, S).

/*---------------------------------*
*        maj_recip/2               *
*----------------------------------*/
/* corrigé avec util de nom_fe et remplacement de Nompart par Nompartb --PS2012 */
maj_recip(NomPart/_/Titre, Nom) :-
    nom_fe(NomPart, fe(NomPartb, T, Deg, Def, Lrep, Lpart)),
    xtype(Nom, Type),
    (
      recip(Titre, Rtitre,participant), !,
         (
         app_titre_syn(Nom/Type/Rtitre,Lpart), !
         ;
         Deg == 0, !,
         retract(fe(NomPartb, T, Deg, Def, Lrep, Lpart)),
	 inserb_fe(fe(NomPartb, T, Deg, Def, Lrep, [Nom/Type/Rtitre|Lpart]))
         ;
	 construit(Nom),!,
         retract(fe(NomPartb, T, Deg, Def, Lrep, Lpart)),
         majdef(T,Deg,Def, Nom/Type/Rtitre, Ndeg, Ndef),
         assert_fig(fe(NomPartb, T, Ndeg, Ndef, Lrep, [Nom/Type/Rtitre|Lpart]))
         ;
         retract(fe(NomPartb, T, Deg, Def, Lrep, Lpart)),
         assert_fig(fe(NomPartb, T, Deg, Def, Lrep, [Nom/Type/Rtitre|Lpart]))
         )
    ;
      recip(Titre, Fonct, representant),                             
      Nrep =.. [Fonct, Nom],
      retract(fe(NomPartb, T, Deg, Def, Lrep, Lpart)),           /* tres mauvais, à revoir */
      ajoute_repr(Nrep,fe(NomPartb, T, Deg, Def, Lrep, Lpart), Fel),
      assert_fig(Fel), !
    ).
      
      
/*---------------------------------*
*        ajoute_partl1/3            *
*----------------------------------*/ 
ajoute_partl1([Part|SP], fe(Nom, Ty, Deg, Def, Lrep, Lpart), FeS) :-
    nomp(Part,Np), construit(Np),!,
    majdef(Ty, Deg, Def, Part, Ndeg, Ndef),
    maj_recip(Part, Nom), !,
   (
    app_titre_syn(Part, Lpart), !,
    ajoute_partl1(SP, fe(Nom, Ty, Ndeg, Ndef, Lrep, Lpart), FeS)
   ;
    ajoute_partl1(SP, fe(Nom, Ty, Ndeg, Ndef, Lrep, [Part|Lpart]), FeS)
   )
   ;
    app_titre_syn(Part, Lpart), !,
    ajoute_partl1(SP, fe(Nom, Ty, Deg, Def, Lrep, Lpart), FeS)
   ;
   maj_recip(Part, Nom),
  ( 
     app_titre_syn(Part, Lpart), !,
    ajoute_partl1(SP, fe(Nom, Ty, _, _, Lrep, Lpart), FeS)   /* avant Ndeg et Ndef au lieu de _,_*/
    ;
   ajoute_partl1(SP, fe(Nom, Ty, Deg, Def, Lrep, [Part|Lpart]), FeS)
  ).
   

ajoute_partl1([], Fe, Fe).

/*----------------------------------------------*
*          majdef/6                             *
* mise  jour d'une definition                  *
* incomplete (precondition) par ajout           *
* d'un participant.                             *
* profil :                                      *
* majdef(type,deg_lib,def,part.,deg_lib, def)  *
* flux   (i,i,i,i,o,o)                          *
*-----------------------------------------------*/
majdef(_, 0, Def, _, 0, Def) :- !.

majdef(_, Deg, Def, Part, Deg, Def) :-
    Deg > 0, app_titre_syn(Part, Def), !.

majdef(_, Deg, Def, Part, Ndeg, Ndef) :-
    Deg > 0, nomp(Part,Nom), construit(Nom),
    titre(Part, Titre),
    deg_titre(Titre, Dt),
    Dt < Deg,!,
    Ndef = [Part|Def],
        Ndeg is Deg - Dt.

majdef(Typ, Deg, Def, Part, Ndeg, Ndef) :-
    Deg > 0, nomp(Part,Nom), construit(Nom),
    princip(Typ, Lcog),
        (
    recherche_coinc_liste([Part|Def], Lcog,Ndef),
    !, Ndeg = 0
    ;
    Ndef = [Part|Def], Ndeg = Deg, !
    ).

/*----- utilitaires pour majdef :      --------*/
recherche_coinc_liste(L, [COG | Suite], Defc) :-      /* recherche_coinc_liste */
    recherche_coinc(L, COG, Defc), !
    ;
    recherche_coinc_liste(L, Suite, Defc).

recherche_coinc(Defi, Cog, Defc) :-                   /* recherche_coinc       */
    decomp(Cog, Listt),
    coincidences(Defi, Listt, Lnoms), !,
    Defc =.. [Cog|Lnoms].

coincidences(_, [], []).                              /* coincidences          */
coincidences(Defi, Listt, [Nom|Lnoms]) :-
    efface(Type/Titre, Listt, Listtbis),
    efface(Nom/_/Titre, Defi, Defibis),
    xtype(Nom, Type),
    coincidences(Defibis, Listtbis, Lnoms).

/*---------------------------------*
*        fusion_fe/2               *
*   fusion de deux figures         *
* elementaires. Seule la deuxieme  *
* existera explicitement dans la   *
* figure.                          *
*   Action sur la figure par effet *
* de bord !!!                      *
*----------------------------------*/

fusion_fe(Fe1, Fe2) :-
    Fe1 = fe(Nom1, Ty, Deg1, Def1, Lr1, Lp1),
    Fe2 = fe(Nom2, Ty, Deg2, Def2, Lr2, Lp2),
    def_fin(Deg1, Def1, Deg2, Def2, Degi, Defi),
    Fei = fe(Nom2, Ty, Degi, Defi, Lr2, Lp2),
    traite_alias(Nom1, Nom2),                   /* ajout --PS2012   */
    ajoute_repr_l(Lr1, Fei, Feii),
    ajoute_parts(Lp1, Feii, Fef),
    retract(fe(Nom1, Ty, _, _, _, _)),
    retract(fe(Nom2, Ty, _, _, _, _)),
    assert_fig(Fef),
    ((Deg1==0, Deg2 \== 0; Deg1 \== 0, Deg2 ==0), !, propage0(Nom2); true),
    revise_fig(Nom2,Nom1).

choisi_nom(Nom1, _, 0, Nom1):- !.      /* on peut, peut-tre, faire plus fin ! */
choisi_nom(_,Nom2,_,Nom2).

def_fin(_, _, 0, Def, 0, Def) :- !.
def_fin(0, Def, _, _, 0, Def) :- !.
def_fin(_, _, Deg, Def, Deg, Def).


traite_alias(Nom1, Nom2) :- Nom1 alias Nom2, !. /* Nom2 est bien le représentant des egaux */
traite_alias(Nom1, Nom2) :- Nom1 alias Rep, !, traite_aliases(Rep, Nom2). /* en principe Rep = Nom1 */

traite_aliases(Rep, Nom2) :- retract(X alias Rep), assert(X alias Nom2), fail.
traite_aliases(_,_).

    
/*------------------------------------------------------------------*
		revise_fig/1
	revise(Nom)
	le premier arg est le nom figurant effectivement dans la fe
        calculée par fusion
	on suppose que la table des syn. a dj t mise  jour !
*-------------------------------------------------------------------*/
revise_fig(Nom1, Nom2) :-
	cherche_fe(Fe),
	traite_fus(Nom1, Fe),
        traite_rel_part(Nom1, Nom2),
	fail.

revise_fig(_, _).      /* revise_fig réussit toujours */

traite_fus(Nom, Fe) :-
	fe_def(Fe, Def),
/*	partielle(Def),!,  */
        Def = [_,_|_], !,     /* on ne traite que les def partielles avec au moins 2 args */
	double_occ(Nom, Def),
	retract(Fe),
        met_a_jour(Nom, Fe, Nfe),
	assert_fig(Nfe).

traite_fus(Nom, Fe) :-
	deg_lib(Fe, 0),
	fe_def(Fe, Def),
	Def =.. [_|Largs],
	appartient_syn(Nom, Largs),!,
        def_fe(Def, Feb),		/* def_fe est  REVOIR */
        Fe \== Feb,
	fe_nom(Fe, Nom3),
	fe_nom(Feb, Nom4),
        not(synonymes(Nom3, Nom4)),
	majsyn(Nom3, Nom4),
	fusion_fe(Fe, Feb).


partielle([]).
partielle([_|_]).

double_occ(_, []) :- !, fail.
double_occ(_, [_]) :- !, fail.
double_occ(Nom, Def) :-
	eff_tit_syn(Nom/_, Def, Defb),!,
	eff_tit_syn(Nom/_, Defb, _).

eff_tit_syn(Nom/T, [Nom2/T|S], S) :- synonymes(Nom, Nom2).
eff_tit_syn(Nom/T, [Part|Suite], [Part|Suitb]) :-
	eff_tit_syn(Nom/T, Suite, Suitb).

met_a_jour(Nom, Fe, Nfe) :-
	Fe = fe(N, Ty, Deg, Def, Parts, Reps),
	eff_tit_syn(Nom/T, Def, Defp), !,
	supp_doub(Nom, Deg, Defp, Ndeg, Ndef),
	Nfe = fe(N, Ty, Ndeg, [Nom/T|Ndef], Parts, Reps).

supp_doub(_, Deg, [], Deg, []) :- !.
supp_doub(Nom, 0, Def, 0, []) :- !,
        write('(supp_doub) : erreur avec : '), write(Nom), write(' et '), write(Def), nl,
	point_arret(supp_doub).		/* ceci ne devrait pas se produire */
supp_doub(Nom, Deg, [N/Ti|S], Ndeg, Ndef):-
	synonymes(Nom, N), !,
	deg_titre(Ti, Dr),
	Dg is Deg-Dr,
        supp_doub(Nom, Dg, S, Ndeg, Ndef).
supp_doub(Nom, Deg, [Part|S], Ndeg, [Part|Ndef]) :-
	supp_doub(Nom, Deg, S, Ndeg, Ndef).

traite_rel_part(Nom1, Nom2):- (rel_part(Nom1 diff Nom2) ; rel_part(Nom2 diff Nom1)),
        write(' contradiction **** au moment de la fusion de '),
        write(Nom1), write(' et '), write(Nom2), point_arret.

/* TODO : mettre à jour les relations particulières */
