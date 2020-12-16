/*------------------------------*
*       INIT.PRO                *
*   transformation d'un enonce  *
* en une configuration.         *
*-------------------------------*/

/*----------------------------------*
*         lis_decl/0                *
*  transformation de l'ensemble     *
*  des declarations en figure.      *
*-----------------------------------*/
lis_decl :- 
          retract(N 'dec:' Declaration),
          ddecl(Declaration,Nom,Ty,Motif),
          assert(lu(N 'dec:' Declaration)),
          creer(Nom,Ty,Motif),               /* complete la figure    */
          lis_decl, !.
          
lis_decl.      

/*--------------------------------------*
*         lis_cont/0                    *
*  Utilisation des contraintes          *
* pour creer le raisonnement et         *
* mettre a jour la figure.              *
*  Les premieres contraintes d'         *
* exeptions sont mises en place.        *
*---------------------------------------*/
lis_cont :- retract(N 'cont:' Contrainte),
            assert(lu(N 'cont:' Contrainte)), 
            traite_init(Contrainte),
            lis_cont.

lis_cont.




/*---------------------------------*
*         ddecl/4                  *
*  decomposition d'une declaration *
*                                  *
*----------------------------------*/
ddecl(Decl,Nom,Type,Motif) :-
     Decl =.. [Motif, TN],
     TN =.. [::, Type, Nom].

ddecl(Decl,Nom,Type,Motif) :-
     Decl =.. [nomme,TN,Rep],
     TN =.. [::, Type, Nom],
     Motif =.. [defini_par,Rep].

/*---------------------------------*
*         creer/3                  *
*    creer(Nom,Type,Motif)         *
*                                  *
*----------------------------------*/
/* directives de creations provenant en principe exclusivement de l'enonce */
creer(Nom,Type,donne) :-
     atom(Nom),               /* verification peut être inutile */
     assert_fig(fe(Nom,Type,0,donne,[],[])),
     assert(Nom alias Nom),
     autom(Type,Nom,Acreer,Reps),
     creer_liste(Acreer), 
     ajoute_repl(Reps, fe(Nom,Type,0,donne,[],[]), Felem),
     retract(fe(Nom,Type,_,_,_,_)),
     assert_fig(Felem), !.
     

creer(Nom,Type,cherche) :-
     atom(Nom),               /* verification peut tre inutile */
     dmax(Type,N),
     inserb_fe(fe(Nom,Type,N,[],[],[])),
     assert(Nom alias Nom), 
     assert(cherche(Nom)),
     autom(Type,Nom,Acreer,Reps),
     creer_liste(Acreer),
     ajoute_repl(Reps, fe(Nom,Type,N,[],[],[]), Felem),
     retract(fe(Nom,Type,_,_,_,_)),
     assert_fig(Felem), !.
     
     

creer(Nom,Type,mentionne) :-
     atom(Nom),               /* verification peut tre inutile */
     dmax(Type,N),
     inserb_fe(fe(Nom,Type,N,[],[],[])),
     assert(Nom alias Nom),
     autom(Type,Nom,Acreer,Reps),
     creer_liste(Acreer),
     ajoute_repl(Reps, fe(Nom,Type,N,[],[],[]), Felem),
     retract(fe(Nom,Type,_,_,_,_)),
     assert_fig(Felem), !.
     
     

/* directives de creations provenant de l'enonce ou du systeme */
creer(Nom,_,defini_par Terme) :- 
    atomise(Terme,Nom).



/*---------------------------------*
*         creer_liste/1            *
*   creation d'objets a partir     *
*  d'une liste de declarations     *
*----------------------------------*/

creer_liste([]) :- !.
creer_liste([Decl|Reste]) :- 
     ddecl(Decl,Nom,Type,Motif),
     creer(Nom,Type,Motif),
     creer_liste(Reste).

/*---------------------------------*
*        traite_init/1             *
* Ajoute une contrainte donnee par *
* l'enonce dans le graphe de       *
* raisonnement et fait les mises   *
* a jour necessaires dans la       *
* figure.                          *
*----------------------------------*/
traite_init(X diff Y) :- /* trappe mise en place pour ajouter des diffs. dans l'enonce */
       assert(rel_part(X diff Y)), !.    /* PS2012, tres sommaire pour le moment */
					/* ne concerne que des diff de noms */

traite_init(Cont) :-
    Cont =.. [CRG|Larg],
    ptitre(CRG,T),
    traite_init_titre(CRG, Larg, T, Condat),
    insom(Condat, _, _).
    
traite_init_titre(CRG, [Arg1, Arg2], egalite, Eg) :-
    atom(Arg1), !,
    atomise(Arg2, Arg1),
    Eg =.. [CRG, Arg1, Arg1]
    ;
    atom(Arg2), !,
    atomise(Arg1, Arg2),
    Eg =.. [CRG, Arg2, Arg2]
    ;
    atomise(Arg1, Nom1),
    atomise(Arg2, Nom1),
    Eg =.. [CRG, Nom1, Nom1].

traite_init_titre(CRG, [Arg1, Arg2], incid, Inc) :-
    !, 
    atomise(Arg1, Nom1),
    atomise(Arg2, Nom2),
    nom_fe(Nom1, Fe1),
    xtype(Nom2, Typ),
    ajoute_partl1([Nom2/Typ/incid], Fe1, FeS),
    retract(Fe1), assert_fig(FeS),
    Inc =.. [CRG, Nom1, Nom2].

traite_init_titre(CRG, Largs, _, Cat) :-
    atomisel(Largs, Lats),
    Cat =.. [CRG|Lats].
    
    

