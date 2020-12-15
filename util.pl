/*---------------------------------*
*         util.pro                 *
*  utilitaires pour proge          *
*                                  *
*----------------------------------*/

/*
 Predicats :                                                                
   Pour l'unification :                                                     
   --------------------                                                     
       ( unification standart = )                                           
        * unif_syn/2 : unification modulo la table des synonymes.           
        * unif_rep/2 : filtrage modulo la table des synonymes,              
                     la recherche dans la representation,                   
                     les permutations eventuelles des arguments.            
        * id_syn/2   : egalite syntaxique (==) modulo la table des          
                     synonymes                                              
        * unif_repb  : unif_rep apres passage par la table des syn.         
        * unif_repl  : unif_rep pour deux listes d'arguments.               
        * inst_dans_reps : instanciation dans une liste de representants.   
        * inst_dans_parts : instanciation a l'aide d'une liste de           
                          participants.                                     
        * instancie_chacun :                                                
                                                                            
    Generation        :                                                     
    -------------------                                                     
        * gnum/2 : generation de numeros types (par le premier argument)    
        * maz/1  : mise a zero pour le compteur dont le type est donne      
        * gname/2 : generation de noms par concatenation d'un type          
                   et d'un nombre genere par gnum.                          
        * toascii/2 : conversion d'un nombre  en caractere.                 
        * ctoascii/2 : conversion d'un chiffre en caractere.                
                                                                            
   Listes             :                                                     
   --------------------                                                     
        * concat                                                            

   Entrees/ Sorties   :
   --------------------
   affichage :
        * writel/1
        * writeln/1
        * writelnn/2
        * affiche_liste/2
   saisie    :
        * get1/1
        * getrc/0
        * videbuff

   Divers             :
   --------------------
        * findall/3
        * for/3
        * for/4
                                                                            

 */                                                                          


/*---------------------------------*
*         unif_syn/2               *
* Unification modulo la table des  *
* synonymes ( alias ).             *
*----------------------------------*/

unif_syn(T1,T2) :- 
     atom(T1),
     T1 alias T,
     T2 alias T.    /* si T2 est une variable ceci l'instancie */
unif_syn(T1,T2) :-
     atom(T2),
     T2 alias T,
     T1 alias T.    
unif_syn(T1,T2) :-
     T1 =.. [F|L1],
     T2 =.. [F|L2],
     unif_synl(L1,L2).
     
unif_synl([],[]).
unif_synl([P1| S1],[P2|S2]) :-
     unif_syn(P1,P2),
     unif_synl(S1,S2).
     
/*---------------------------------------------------------------*
*         unif_rep/2                                             *
*    Unification modulo la repr. d'un objet geometrique.         *
*  unif_rep(Terme, TC) TC doit õ€•˜re un terme clos. Si l'         *
*  unification standart echoue parce que TC est un nom de        *
*  fe alors on cherche dans la representation de TC les          *
*  termes qui feraient reussir l'unification.                    *
*                                                                *
*----------------------------------------------------------------*/
/*
unif_rep(T1,T2) :-
    nl, nl, nl,
    write('****** spy sur unif rep **** '), write(T1), write(' et '), write(T2),
   nl, nl, nl, trace,
   fail.
*/
   
unif_rep(Var, TC) :-
	var(Var), atom(TC), Var = TC.

unif_rep(Const, TC) :-
	atom(Const), atom(TC), Const alias R, TC alias R, !.
unif_rep(Terme, TC) :-
    atom(TC),nonvar(Terme),not(atom(Terme)),
    TC alias T,
    unif_repb(Terme, T).
unif_rep(Terme, TC) :-
    not(atom(TC)),nonvar(Terme),not(atom(Terme)),
    Terme equiv Term1,
    TC =.. [F|Lar],
    Term1 =.. [F|Lv],
    unif_repl(Lv,Lar).

/*-------- idem pour une liste --------*/
unif_repl([],[]).
unif_repl([PC|SC],[PT|ST]) :-
    unif_rep(PC,PT),
    unif_repl(SC,ST).

/*--------- idem apres passage par la table des synonymes -----*/
unif_repb(Var, TC) :-
	var(Var), atom(TC), Var = TC.

unif_repb(Const, TC) :-
	atom(Const), atom(TC), Const alias R, TC alias R, !.

unif_repb(Terme, TC) :-
    atom(TC),
    not(decomposable(Terme)),
    nom_fe(TC,F),
    fe_reprs(F,Lr),
    appartient_rep(Terme,Lr).


unif_repb(Terme, TC) :-
    atom(TC),not(atom(Terme)),
    decomposable(Terme),
    nom_fe(TC,F),
    fe_parts(F,Lp),
    appartient_part(Terme,Lp).

/*---------------------------------*
*        inst_dans_reps/2          *
*  instancie un terme dans une     *
* liste de representants           *
*----------------------------------*/
inst_dans_reps([T1|_], Terme) :-
    unif_rep(T1,Terme).
inst_dans_reps([_|S], Terme) :-
    inst_dans_reps(S,Terme).

/*---------------------------------*
*        inst_dans_parts/2         *
*  Instancie un terme avec une      *
* liste de participants.           *
*----------------------------------*/
inst_dans_parts(Lp, Terme) :-
    profil(Terme,Prof),
    Terme =.. [_|Larg],
    associe(Prof,Larg,Lapart),
    instancie_chacun(Lapart,Lp).

instancie_chacun([],_).
instancie_chacun([Parg/TP|Sarg],Lp) :-
    efface(X/TP,Lp,Lp1),
    X alias Xr,
    unif_repb(Xr,Parg),
    instancie_chacun(Sarg,Lp1).
              
/*---------------------------------*
*         id_syn/2                 *
*    == modulo les syn.            *
*----------------------------------*/
id_syn(T1,T2) :-
     T1 == T2, !.
id_syn(T1,T2) :-
     atom(T1),
     atom(T2), !,
     T1 alias T,
     T2 alias T.
id_syn(T1,T2) :-
     nonvar(T1),
     nonvar(T2),
     T1 =.. [F|L1],
     T2 =.. [F|L2],
     id_synl(L1,L2).
id_syn([P1|S1],[P2|S2]) :-
     id_syn(P1,P2),
     id_synl(S1,S2), !.
id_synl([],[]) :- !.     



/*---------------------------------*
*         gnum/2                   *
*  generateur de numeros ...       *
* le premier argument est le       *
* type de la numerotation, le      *
* deuxieme est le no genere.       *
* (pas de numerotation 1)          *
*----------------------------------*/

gnum(F,N) :-
     C =.. [F,N],
     retract(C),
     NN is N+1,
     NC =.. [F,NN],
     assert(NC),!.
     
gnum(F,0) :-
     C =.. [F,1],
     assert(C).

maz(F) :-           /*------ mise a zero --------------*/
     C =.. [F,_],
     retract(C).
/*---------------------------------*
*         gname/2                  *
*  generateur de noms              *
*                                  *
*----------------------------------*/
gname(Atome,Nom) :-
     name(Atome, String),
     list(LS, String),
     list([109,97,120,36|LS], FS),
     name(F, FS),
     gnum(F,N),
     toascii(N,L),
     concat(LS, L, LA),
     list(LA, LAS),
     name(Nom,LAS).

ctoascii(0,48) :- !.
ctoascii(1,49) :- !.  
ctoascii(2,50) :- !.
ctoascii(3,51) :- !. 
ctoascii(4,52) :- !.
ctoascii(5,53) :- !.  
ctoascii(6,54) :- !.
ctoascii(7,55) :- !. 
ctoascii(8,56) :- !.
ctoascii(9,57) :- !.
ctoascii(C,97) :- nl,
          write(' (ctoascii) *** ERREUR : '),
          write(C),
          write(' n''est pas un chiffre ***'),
          nl.
          
toascii(N, [A1,A2]) :- 
          N >= 0,
          C1 is N // 10,
          C2 is N mod 10,
          ctoascii(C1,A1),
          ctoascii(C2,A2).

                    
/*=======================================*
*        Listes                          *
*                                        *
*========================================*/

concat([X|L1],L2,[X|L3]) :- concat(L1,L2,L3).
concat([],L,L).

union([X|L1],L2,L3) :- appartientf(X,L2), !, union(L1,L2,L3).
union([X|L1],L2,[X|L3]) :- !, union(L1,L2,L3).
union([], L, L).

efface(X, [X|L], L).
efface(X, [Y|L], [Y|LL]) :- efface(X,L,LL).

appartient(X,[X|_]).
appartient(X,[_|S]) :- appartient(X, S).

appartient1(X,[X|_]) :- !.
appartient1(X,[_|S]) :- appartient1(X, S).

appartientf(X,[Y|_]) :- X == Y.
appartientf(X,[_|S]) :- appartientf(X, S).


tous_dif_syn([X|L]) :- not(appartient_syn(X,L)), !,tous_dif_syn(L).
tous_dif_syn([]).

appartient_syn(X,[Y|_]) :- X alias R, Y alias R.
appartient_syn(X,[_|S]) :- appartient_syn(X, S).

tous_dif_rep([X|L]) :- not(app_rep(X,L)), !, tous_dif_rep(L).
tous_dif_rep([]).

app_rep(X,[Y|L]) :-
	atom(X), unif_rep(Y,X), !
	;
	unif_rep(X,Y), !
	;
	app_rep(X,L).

retire(X,[Y|L],L) :- X == Y.
retire(X,[Y|L],[Y|L1]) :- retire(X,L,L1).

/*=======================================*
*        Divers                          *
*                                        *
*========================================*/
/*---------------------------------*
*    writel/1                      *
* Affiche une liste ...            *
*----------------------------------*/
writel([]) :- !.
writel([ligne|S]) :- !, nl, writel(S).
writel([P|S]) :- write(P), writel(S).

/*---------------------------------*
*    writeln/1                     *
* Idem, mais en passant a la ligne *
* a chaque membre.                 *
*----------------------------------*/
writeln([]) :- !, nl.
writeln([P|S]) :- write(P), nl, writeln(S).

/*---------------------------------*
*   writelnn/2                     *
* voir affiche_liste               *
*----------------------------------*/
writelnn(L,N) :- affiche_liste(L,N).

/*---------------------------------*
*        affiche_liste/2           *
*   affiche_liste(L,N)             *
*   Affiche la liste L en passant  *
* a la ligne a chaque membre et    *
* en s'arrõ€•˜ant toutes les N lignes*
* (utilise aff_liste_bis)          *
*----------------------------------*/

affiche_liste(L,Nl) :- aff_liste_bis(L,1,Nl).

aff_liste_bis([X|S], I, N) :- 
         I < N, !,
         nl, write(X),
         NI is I+1,
         aff_liste_bis(S,NI,N).
         
aff_liste_bis(L , N,N) :-  
    L \== [], !,
    nl, write(' ------ appuyez sur RC ------- '),
    getrc,
    aff_liste_bis(L, 1, N).

aff_liste_bis([],_,_) :- nl, !.

/*------------------------*
*    get1/1               *
* prend le premier car.   *
* ------------------------*/
get1(C) :- get1b(C), !.
get1b(C) :- repeat,
	  get0(C), C \== 10, C \== 13, !, videbuff.

/*---------------------------------*
*        getrc/0                   *
*  Attend un retour chariot        *
*----------------------------------*/
getrc :- get0(C), ( C == 10, ! ;C == 13, ! ; videbuff, !).

get1rc(C) :- get0(C), (C == 10, !; C == 13, ! ;videbuff, !).

/*------------------------*
*   videbuff/0            *
*  lis et neglige les car.*
* jusqu' a  un  retour    *
* chariot (10).           *
*-------------------------*/
videbuff :- 
    repeat, 
    get0(C),
    (C == 10, !; C == 13, !).
    

/*---------------------------------*
*        for/3                     *
*  classique ...                   *
*  (pas 1 par defaut)              *
*----------------------------------*/

for(A,A,B) :- A =< B, !.
for(I, A, B) :- A < B, AA is A + 1, for(I, AA, B).

/*---------------------------------*
*        for/4                     *
*  classique ...                   *
*  ( le pas doit õ€•˜re >0 ...)      *
*----------------------------------*/

for(A,A,B, _) :- A =< B, !.
for(I, A, B, P) :- A < B, AA is A + P, for(I, AA, B).

/*---------------------------------*
*        findall/3                 *
*   Classique, mais n'existe       *
*  pas dans la version de prolog   *
*  en ma possession.               *
*----------------------------------*/
/* commenté pour swi-prolog
findall(X, But, _) :-
    call(But),
    assertz('$$pile$$'(X)),
    fail.

findall(_,_,L) :- recupere(L).

recupere([X|Suite]) :-            /*--- utilise par findall uniquement ---*/
    retract('$$pile$$'(X)),!,
    recupere(Suite).
    
recupere([]).
*/
/*--------------------------------------*
*					*
*	essai d'utilisation de		*
*	freeze pour les regles ...	*
*					*
*---------------------------------------*/
init_diff :- new_handler(signal, freeze, diff(C,T), diff_inst(C,T)),
	new_handler(signal, freeze, diff(V,T1,T2), diff_unif(V,T1,T2)).

diff_inst(_,T) :- T.
diff_unif(V,T1,T2) :- refreeze(V,diff,(T1,T2)).

tous_diff_var(L) :-  tous_diff_vari(L,L).

tous_diff_vari([X|S],L) :-  
			retire(X,L,Lsx),
			freeze(X, diff, nappas(X,Lsx)),!,
			tous_diff_vari(S,L).
tous_diff_vari([], _).
		   
nappas(X,[Y|L]) :- (atom(X), atom(Y), X alias R, not(Y alias R) ; X \== Y),!,
                   nappas(X,L).
nappas(_,[]).


l_terme_var(LT,LV) :- l_terme_var(LT,[],LV).
l_terme_var([T|LT], L, LV) :-
	terme_var(T,Lvt),
	union(Lvt,L,Ls),
	l_terme_var(LT,Ls,LV).
l_terme_var([],L,L).

terme_var(X,[X]) :- var(X), !.
terme_var(N,[]) :- atomic(N), !.
terme_var(T, L) :-
	T =.. [_|Larg],
	l_terme_var(Larg,L).

