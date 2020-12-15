/*---------------------------------*
*         FIGURE.PRO               *
*   Gestion de la figure           *
*                                  *
*----------------------------------*/

/*
 Gestion de la figure :                                            
     -figure vide.                                                 
     -inserer une figure elementaire.                              
     -acceder a une figure elementaire :                           
	  par son nom (propre),                                    
	  par un synonyme,                                         
	  par un representant (propre),                            
	  par un representant (via les synonymes),                 
     -retirer une figure elementaire,                              
     -propager les dø 0 dans la figure,                            
	et placer les objets connus dans le pcg                    
	       ....                                                
								   
 Gestion de la table des synonymes :                               
     -table vide,                                                  
     -insertion d'une autoreference,                               
     -obtention du representant privilegie d'un nom,               
     -liste des representes,                                       
     -connections de deux parties de tables,                       
	       ....                                                
								   
*/

/*---------------------------------*
*         inserb_fe/1              *
*  Insertion "brute" d'une fe.     *
*----------------------------------*/
/* 
inserb_fe(fe(N,T,D,Def,Lr,Lp)) :- fe(N,T,D,Def,Lr,Lp), !.
inserb_fe(fe(N,_,_,_,_,_)) :- fe(N, TT, DD, DDe, LLr, LLp),
	write('(inserb_fe dans figure) : **** ATTENTION on a deja : '),
	nl, write(fe(N, TT, DD, DDe, LLr, LLp)), point_arret.
inserb_fe(fe(N,T,D,Def,Lr,Lp)) :- assert(fe(N,T,D,Def,Lr,Lp)).
*/

inserb_fe(fe(N,_,_,_,_,_)) :-
    nom_fe(N, Fige),
    write(Fige),
    point_arret('erreur dans inserb_fe').
inserb_fe(fe(N,T,D,Def,Lr,Lp)) :- assert(fe(N,T,D,Def,Lr,Lp)).
/*---------------------------------*
*        assert_fig/1              *
*   Insertion d'une fe et mises a  *
* jour :                           *
*        de la figure (propagation)*
*        du PCG                    *
*                                  *
*----------------------------------*/
assert_fig(Fe) :-
    fe_nom(Fe,Nom),construit(Nom), !,
    inserb_fe(Fe)
    .
    
assert_fig(fe(N,T,0,Def,Lr,Lp)) :-          /* La figure elementaire est construite */ 
	 assert_const(fe(N,T,0,Def,Lr,Lp)), !,
	 inserb_fe(fe(N,T,0,Def,Lr,Lp)),
	 assert(deja_construit(N)),         /* ainsi que tous les synonymes ...     */
	 abolishe(inhib),
		(  
		  fini, !
		;
		  propage0(N)
		)
	;
	 defaire(fe(N,T,0,Def,Lr,Lp), Nouv_fig), !,     /* la def donnee est mauvaise */
	 inserb_fe(Nouv_fig).

assert_fig(Fe) :- inserb_fe(Fe).


/*---------------------------------*
*    recherche d'une fe par l'un   *
*  de ses champs.                  *
*----------------------------------*/
/*------------simples ... ----------------------------------*/
exist_fe :- fe(_,_,_,_,_,_).       /*   existence d'une fe dans la figure  */

cherche_fe(fe(N,T,D,Def,Lr,Lp)) :- fe(N,T,D,Def,Lr,Lp). /* recherche brute */

nom_fe(Nom, fe(Rep, T, D, Def, Lr, Lp)) :-   /* recherche d'une fe par le nom */
/* ancienne tête : nom_fe(Nom, fe(Nom, T, D, Def, Lr, Lp)) :-                */
     synonyme(Nom,Rep),                      /* Nom ou Rep rendu : Rep en PS2012 */
     fe(Rep, T, D, Def, Lr, Lp), !.           /* coupure : --PS2012 */ 

/* recherche par une definition  */
/* ancienne version             */
/*
def_fe(Def, fe(Nom, T, D, Def, Lr, Lp)) :-   
     fe(Nom, T, D, Def, Lr, Lp), !.   
*/         
/* version tirée de correct.pl --PS2012 */
def_fe(Def, fe(Nom, Ty, D, Adef, Lr, Lp)) :-
	fe(Nom, Ty, D, Adef, Lr, Lp),
	Adef equivdef Adf,
	id_syn(Def, Adf).




/*------------recherche par un representant-----------------*/
/*   en principe, flux (i,o)                                */
rep_fe(Rep, fe(Nom, T, D, Def, Lr, Lp)) :-   /* recherche par un representant */
     not(atomic(Rep)),
     not(decomposable(Rep)),                  /* non decomposable !            */
     ttype(Rep, T),
     fe(Nom, T, D, Def, Lr, Lp),             
     appartient_rep(Rep, Lr).

rep_fe(Rep, fe(Nom, T, D, Def, Lr, Lp)) :-   /* recherche par un representant */
     not(atomic(Rep)),
     decomposable(Rep),                      /* decomposable.                 */
     ttype(Rep, T),
     fe(Nom, T, D, Def, Lr, Lp),             
     appartient_part(Rep, Lp).

/*---------------------------------*
*        construit/1               *
*----------------------------------*/

construit(Terme) :- integer(Terme) , ! ; real(Terme), !.
construit(Terme) :- atom(Terme), synonyme(Terme,Term),
		    deja_construit(Term), !.
construit(Terme) :- atom(Terme), !, synonyme(Terme,Term),
		    fe(Term, _, 0, _, _, _),
		    writel(['(construit) *** Attention : ',Terme,' pas deja_construit']),
		    assert(deja_construit(Term)), !.
construit(Terme) :- Terme =.. [_ | Larg],
		    construit_l(Larg), !.

construit(Terme) :- rep_fe(Terme, Fe),
		    deg_lib(Fe,0).

construit_l([P|S]) :- construit(P), !, construit_l(S).
construit_l([]).
    
     
/*---------------------------------*
*        atomise/2                 *
*  Atomisation d'un terme.         *
*----------------------------------*/
/*
    atomise(Terme, Nom)
 Ce predicat fondamental agit surtout par effet de bord :
    - il y a creation de nouvelles figures elementaires
    - et mise a jour de certaines figures elementaires 
      existant deja dans la figure.
 Atomise a plusieur fonctionnements suivant que Nom est
 une variable instanciee ou non :
    - si Nom est instancie, il y a mise a jour de la figure
   elementaire ainsi nommee et mise a jour de la table des
   synonymes.
    - si Nom n'est pas instancie, il y recherche dans la 
   figure d'une figure elementaire correspondant a Terme,
   puis mise a jour si cette fe est trouvee, sinon creation
   d'une nouvelle fe.


*/
atomise(Nombre, Nombre) :- number(Nombre), !.

atomise(V,_) :- var(V), !, fail.

atomise(Terme, Nom) :- atom(Nom), !,atominst(Terme, Nom).

atomise(Terme, Nom) :- var(Nom), !, atomvar(Terme, Nom).


atomise(Terme, Nom) :-
    nl, writel([' (atomise) **** erreur : Terme = ',Terme,ligne,' Nom = ',Nom]).

/*---------------------------------*
*        atomisel/2                *
*  Atomise/2 pour les listes.      *
*----------------------------------*/
atomisel([], []) :- !.
atomisel([P|S], [PN|SN]) :-  atomise(P,PN), atomisel(S, SN).

/*---------------------------------*
*        atominst/2                *
*  Cf. atomise/2                   *
*----------------------------------*/
atominst(Nom1, Nom2) :- atom(Nom1), Nom1 alias R, Nom2 alias R, !.

atominst(Nom1, Nom2) :- atom(Nom1), 
    synonymes(Nom1,R1), synonymes(Nom2,R2),
    nom_fe(R1, Fe1),
    nom_fe(R2, Fe2),
    majsyn(R1,R2),
    fusion_fe(Fe1, Fe2),
     !.

atominst(Terme :: Cond, Nom) :-
    !,
    Terme =.. [F|Larg],
    atomisel(Larg, Lnom),
    Terma =.. [F|Lnom],
    (
    nom_fe(Nom, Fe), !,
    ajoute_rep(Terma, fe, Fef),
    retract(Fe),
    assert_fig(Fef)   /* + mises a jour automatiques */
    ;
    creerinit(Terma, Nom)    /* + mises a jour automatiques */
    ),
    'maj:' Nom eg Terma ==> Lmaj,
    complete_figl(Lmaj, _).

	 
atominst(Terme, Nom) :-
    Terme =.. [F|Larg],
    atomisel(Larg, Lnom),
    Terma =.. [F|Lnom],
    (
    rep_fe(Terma,Fige),!,      /* (1) recherche d'une fe a l'aide d'un rep. */
    fe_nom(Fige,NNom),
       (
	 Nom alias NNom, !    /* on le savait deja !   */
	;
	 Nom alias Arep, !,       /* on a trouve des synonymes  */
	 nom_fe(Arep, Afig),
	 majsyn(Nom, NNom),        /* on met a jours la table  */
	 fusion_fe(Afig, Fige)   /* on fusionne les 2 figures  */
	; 
	 assert(Nom alias NNom)
	)
    ;                        /* la recherche (1) a echouee           */
     nom_fe(Nom, Fe),!,
     ajoute_rep(Terma, Fe, Fef),
     retract(Fe),
     assert_fig(Fef)         /* + mises a jour automatiques.         */
    ;
     creerinit(Terma,Nom)   /* + mises a jour automatiques.          */
    ),
    'maj:' Nom eg Terma ==> Lmaj,
    complete_figl(Lmaj,_).    

/*---------------------------------*
*        atomvar/2                 *
*   Cf. atomise/2                  *
*----------------------------------*/
atomvar(Nom, Rep) :- atom(Nom), !, 
	 (Nom alias Rep, !
	 ;
	 nl, writel(['(atomvar) ### Attention : ', Nom, 'n a pas de syn']),
	 Rep = Nom
	 ).
	 
atomvar(Terme, Nom) :-
    Terme =.. [F | Larg],
    atomisel(Larg, Lnom),
    Terma =.. [F | Lnom],
    (
    rep_fe(Terma, Fig), !, fe_nom(Fig, Nom)
    ;
    creeraux(Terma, Nom), !
    ),
     'maj:' Nom eg Terma ==> Lmaj,
    complete_figl(Lmaj,_).    

/*---------------------------------*
*        creerinit/2               *
*  Ajout d'une figure elementaire  *
* dont le nom et un representant   *
* sont donnes ( nomme ).           *
* Note : Cette figure elementaire  *
* ÍÍÍÍÍ                            *
* n'est pas encore presente dans   *
* la figure.                       *
*----------------------------------*/
creerinit(Terme, Nom) :-
     ttype(Terme, Type),
     dmax(Type,N),
     assert(Nom alias Nom),
     autom(Type,Nom,Acreer,Reps),
     inserb_fe(fe(Nom, Type, N, [], [], [])), /* tres moche, cause : effets de bords sur ajoute_rep */
     (
     Terme =.. [CRG|_],
     appartient(TTerme, Reps),
     TTerme =.. [CRG|_], !,
     ajoute_rep(Terme,fe(Nom,Type,N,[],[],[]), Felem)
     ;
     ajoute_rep(Terme, fe(Nom, Type, N, [], [], []), Fbis),
     retract(fe(Nom, Type, _,_,_,_)),
     assert_fig(Fbis),
     creer_liste(Acreer), !,
     fe(Nom,Type,D,Df,Lr,Lp),
     ajoute_repl(Reps, fe(Nom,Type,D,Df,Lr,Lp), Felem)
     ),
     retract(fe(Nom, Type, _, _, _, _)),
     assert_fig(Felem), !.
     
     
/*---------------------------------*
*        creeraux/2                *
*----------------------------------*/
creeraux(Terme, Nom) :-
    ttype(Terme, Typ),
    gname(Typ, Nom),
    creerinit(Terme, Nom).

/*---------------------------------*
*        propage0/1                *
* Propagation des dø de lib. nuls. *
*----------------------------------*/
propage0(Nom) :-
	 fe(N, T, D, Def, Lr, Lp),
	 bosse1(Nom, fe(N, T, D, Def, Lr, Lp)),
	 fail.
propage0(_).

bosse1(_, Fe) :-
    deg_lib(Fe, 0), !.
bosse1(Nom,Fe) :-
    fe_def(Fe, Def),
    app_syn_sans(Nom, Def, _), !.
    
bosse1(Nom, fe(N, T, Deg, Def, Lr, Lp)) :-
    app_syn_sans(Nom, Lp, Part),!,
    majdef(T, Deg, Def, Part, Ndeg, Ndef),
    retract(fe(N, T,_,_,_,_)),
    assert_fig(fe(N, T, Ndeg, Ndef, Lr, Lp)), !.
    
bosse1(Nom, fe(N, T, Deg, Def, Lr, Lp)) :-
	 cherche_connu(Nom, Lr, Repf),
	 retract(fe(N, T, Deg, Def, Lr, Lp)),
	 assert_fig(fe(N, T, 0, Repf, Lr, Lp)), !.



app_syn_sans(N, [P/T|_], R/T) :- N alias R, P alias R.
app_syn_sans(N, [_ | S], Part) :- app_syn_sans(N, S, Part).

cherche_connu(Nom, [Repf|_], Repf) :-
    Repf =.. [_|Larg],
    efface_syn_sans(Nom,Larg, Largb),
    construit_l(Largb), ! .

cherche_connu(Nom, [_ | S], Repf) :-
    cherche_connu(Nom, S, Repf).  

efface_syn_sans(Nom, [NNom|L], L) :- Nom alias R, NNom alias R, !.
efface_syn_sans(Nom, [NNom|L], [NNom|LL]) :-
    efface_syn_sans(Nom, L, LL).

/*-----------------------------------------------------*
    Gestion de la table des synonymes.
  La table est representee par les clauses concernant
 le predicat alias/2 defini comme un operateur dans
 PROGE.PRO. 
  Dans la clause O alias R, R est le representant 
 privilegie des synonymes : c'est lui qui figure comme
 nom dans la fe correspondante.
  On s'arrangera pour que les clauses du genre R alias R
 soient les premiŠres a etre essayees de sorte que s'il
 s'agit d'une instanciation, le premier essai se fait
 avec le representant privilegie.
*------------------------------------------------------*/

synonyme(Term1,Term2) :- Term1 alias Terme, Term2 alias Terme.
synonymes(Term1,Term2) :- Term1 alias Terme, Term2 alias Terme. /* pour prevenir les confusions :-( */

/*---------------------------------*
*        majsyn/2                  *
*   mise a jour de la table des    *
* synonymes (par effet de bord)    *
* par connections de deux          *
* composantes connexes.            *
*                                  *
*----------------------------------*/
majsyn(C,E) :-
    C alias RepC,
    E alias RepE,
    RepC \== RepE, !,
    remplace_syn(RepE,RepC).

majsyn(_,_).    

remplace_syn(Nrep, Arep) :-
    retract(N alias Arep),
    assert( N alias Nrep),
    fail.

remplace_syn(_,_) :- !. 


assert_const(fe(Nom, Type, Deg, Def, Lr, Lp)) :-
	/*      a faire : verifier qu'on a pas deja la definition d'un syn. dans le PCG */
	assert(decl(Type, Nom)),
	Def =.. [F|_],
	num_prog_cur(Num),
        gen_except(fe(Nom, Type, Deg, Def, Lr, Lp), Retour),   /* verifier les exceptions ... */
	(
        Retour == maldef, !,
        fail
	;
        Retour == ok, !,
        /* write(Def), write(' '),point_arret(bon_syn), */
        bon_syn(Def, Bdef), 
        /* write('**** bon syn : '), write(Bdef), nl, */
        (
	rmult(F), !,            
	gname(list,NL),
	gnum(num_pcg, Pgm),
	assert(pcg(Num, NL := Bdef)),
	assert(pcg(Num, pour Nom dans NL faire Pgm)),
	abolishe num_prog_cur,
	assert(num_prog_cur(Pgm))
	;
	      (
		pcg(Numb, Nom :=  Anc_def), 
                Numb == Num,
                writel(['**** Attention **** nouvelle def', Nom := Bdef, 'remplacant : ', Nom := Anc_def]), point_arret
		;
                assert(pcg(Num, Nom := Bdef))
               )
        
	)	
        ;
	Retour == exceptions, !
	;
		 nl,writel(['(assert_const) *** mauvais code retour de gen except : ', Retour]),
		 nl, writel(['arguments : ', Nom, ' def : ', Def, ' ***']), nl
	
	).

/*
veille version à la suite
*//*

assert_const(fe(Nom, Type, Deg, Def, Lr, Lp)) :-
	assert(decl(Type, Nom)),
	Def =.. [F|_],
	num_prog_cur(Num),
	(
	rmult(F), !,            
	gname(list,NL),
	gnum(num_pcg, Pgm),
	assert(pcg(Num, NL := Def)),
	assert(pcg(Num, pour Nom dans NL faire Pgm)),
	abolishe num_prog_cur,
	assert(num_prog_cur(Pgm))
	;
	 gen_except(fe(Nom, Type, Deg, Def, Lr, Lp), Retour),   
		(
		 Retour == maldef, !,
		 fail
		;
		 Retour == ok, !,
		 assert(pcg(Num, Nom := Def))
		;
		 Retour == exceptions, !
		;
		 nl,writel(['(assert_const) *** mauvais code retour de gen except : ', Retour]),
		 nl, writel(['arguments : ', Nom, ' def : ', Def, ' ***']), nl
		)
	).

*/


defaire(fe(Nom, Type, _, _, Lr, Lp),fe(Nom, Type, D,[], Lr, Lp)) :-
	dmax(Type,D),
        write('(figure) *** version tres sommaire de defaire pour : '),
        write(Nom), nl.

bon_syn(Def, Bdef) :-
    Def =.. [F|Args],
    bon_syn_l(Args, Bon_args),
    Bdef =.. [F|Bon_args].

/* bon_syn(Def,Def). */

bon_syn_l([],[]) :- !.
bon_syn_l([P|S],[BP|BS]) :-
    synonymes(P, BP),
    (pcg(_, BP := _) ; pcg(_, pour BP dans _ faire _)), !,
    bon_syn_l(S,BS).
