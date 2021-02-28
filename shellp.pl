/*---------------------------------*
*         SHELLP.PRO               *
*    Interprteur de commandes     *
*  pour PROGE.                     *
*                                  *
*----------------------------------*

Les principales fonctions autorises par l'interprteur
sont :
     l'dition d'noncs (contraintes et dclarations),
     l'exploration d'une figure,
     l'exploration d'un raisonnement,
     le lancement de requtes Prolog.
     
*/   

/*-----------------------*
*    shell/0             *
*  boucle principale     *
*------------------------*/
shell :- 
     nl, write('commande : '),
     /* get1(C), list([C],S), */ /* ancienne version incompatible avec swipl */
     get_str1(S), !,
     traitecom(S), shell, !.
     
shell.

/*--------------------------*
*     traitecom/1              *
* interprteur de commandes *
*---------------------------*/
traitecom("H") :- traitecom("h"), !.
traitecom("h") :- 
     nl, write(' C : edition des contraintes'),
     nl, write(' D : edition des declarations'),
     nl, write(' L : listing d un enonce'),
     nl, write('----------------------------------'),
     nl, write(' G : lance la resolution'),
     nl, write('----------------------------------'),
     nl, write(' F : interrogations sur la figure '),
     nl, write(' R : interrogations sur le raisonnement'),
     nl, write('----------------------------------'),
     nl, write(' P : requete Prolog'),
     nl, write('----------------------------------'),
     nl, write(' A : quitter le shell'),
     nl, write(' Q : quitter Prolog'),
     nl, write(' H : aide'), !. 
     
traitecom("a") :- !, fail.
traitecom("A") :- !, fail.

traitecom("Q") :- traitecom("q").
traitecom("q") :- 
     nl, write('confirmation retour a MS-DOS (o/n) : '),
     /* get1(C), *//* ancienne version incompatible avec swipl */
     get_str1(S), !,
     (S == "o"; S == "O"), halt.


traitecom("G") :- resout, !.
traitecom("g") :- resout, !.


traitecom("C") :- traitecom("c"),!.
traitecom("c") :- editcontraintes, !.

traitecom("D") :- traitecom("d"), !.
traitecom("d") :- editdeclar, !.

traitecom("L") :- traitecom("l"), !.
traitecom("l") :- 
     nl, write('----- declarations : '),
     N 'dec:' T,
     nl, write(N 'dec:' T), fail.
traitecom("l") :-
     nl, nl, write('----- contraintes : '),
     N 'cont:' T,
     nl, write(N 'cont:' T), fail.
traitecom("l") :- !.     

traitecom("F") :- traitecom("f"), !.
traitecom("f") :- figure_info, !.

traitecom("R") :- traitecom("r"), !.
traitecom("r") :- raison_info, !.

traitecom("P") :- !, traitecom("p").
traitecom("p") :- 
         nl, write('?- '),
         read(T),
         (
         T == halt, !;
         T == fin, !;
         (
         call(T),!, nl, writel(['prouve : ',T]);
         nl, write(no), nl
         ),
         traitecom("p")
         ).

traitecom(_) :- nl, put(7), write('*** Commande non reconnue').

/*---------------------------------*
*         editcontraintes/0        *
*----------------------------------*/
editcontraintes :- 
     nl, write('editions contraintes : '),
     /* get1(C), list([C], S), */ /* ancienne version incompatible avec swipl */
     get_str1(S), !,
     traite_editc(S), editcontraintes, !.
editcontraintes.

/*---------------------------------*
*         traite_editc/1           *
*----------------------------------*/

traite_editc("H") :- traite_editc("h"), !.
traite_editc("h") :-
     nl, write('--- editions de contraintes :'),
     nl, write(' L : lister les contraintes'),
     nl, write(' A : ajouter une contraintes'),
     nl, write('    (fin = arret de l ajout)'),
     nl, write(' I : inserer une contrainte'),
     nl, write(' M : mouvoir une contrainte'),
     nl, write(' N : renumeroter les contraintes'),
     nl, write(' D : effacer une contrainte'),
     nl, write(' Z : effacer toutes les contraintes'),
     nl, write(' H : aide'),
     nl, write(' Q : quitter l edition'),nl, !.

traite_editc("Q") :- !, fail.
traite_editc("q") :- !, fail.

traite_editc("L") :- traite_editc("l"), !.
traite_editc("l") :- 
          N 'cont:' T,
          write(N 'cont:' T), nl,
          fail.
traite_editc("l") :- !.
          
traite_editc("A") :- traite_editc("a"), !.
traite_editc("a") :- 
     nl, write('ec> '),
     read(T),
     (
      T == fin, ! 
      ;
      gnum(maxcont,N),
      assert(N 'cont:' T),
      traite_editc("a"), !
     ).

traite_editc("I") :- traite_editc("i"), !.
traite_editc("i") :- 
     nl, write(' Numero de la contrainte a inserer (ou nul) : '),
     read(Num),
     (
      Num == nul, !
      ;
      Num 'cont:' _ ,
      nl, write('*** Il y a deja une contrainte portant ce numero'), !
      ;
      nl, write(' Contrainte : '),
      read(T),
      assert(Num 'cont:' T), !
     ).

traite_editc("M") :- traite_editc("m"), !.
traite_editc("m") :-
     nl, write(' Numero de la contrainte a mouvoir (ou nul) : '),
     read(Num),
     (
      Num == nul, !, fail
      ;
      Num 'cont:' T, nl, write( Num 'cont:' T)
      ;
      nl, put(7), write('*** Numero de contrainte inexistant '),fail
     ),
     nl, write(' Nouveau numero pour cette contrainte : '),
     read(NNum),
     (
      NNum == nul, !, fail
      ;
      NNum == Num, !
      ;
      NNum 'cont:' _ , nl, put(7), 
      write('*** Il y a deja une contrainte portant ce numero'), !, fail
      ;
      retract(Num 'cont:' T),
      assert(NNum 'cont:' T), !
      ).
     
traite_editc("N") :- traite_editc("n"), !.
traite_editc("n") :-
     renum_cont, !.

traite_editc("D") :- traite_editc("d"), !.
traite_editc("d") :- 
     nl, write(' Numero de la contrainte a effacer : '),
     read(Num),
     (
      retract(Num 'cont:' _) 
     ;
      true
     ).

traite_editc("Z") :- traite_editc("z"), !.
traite_editc("z") :-
     nl, write(' Voulez-vous vraiment effacer toutes les contraintes (o/n)'),
     /* get1(C), */ /* ancienne version incompatible avec swipl */
     get_str1(S), !,
     (
      S \== "O",
      S \== "o", !
     ;
      retract( _ 'cont:' _ ),
      fail
     ;
      true
     ), !.

traite_editc(_) :-
     nl, put(7), write('(editc) *** Commande non reconnue').

renum_cont :-
    maz(maxcont),
    renum_contb,
    renomme_cont.
    
renum_contb :-
     ppl_cont(N),
     retract( N 'cont:' T),
     gnum(maxcont,N),
     assert(cont(N,T)),
     renum_contb, !.

renum_contb.       

renomme_cont :-
     retract(cont(N,T)),
     assertz( N 'cont:' T),
     fail.
renomme_cont.

ppl_cont(N) :-
     M 'cont:' _ ,
     ppl_cont(N,M), !.
     
     
ppl_cont(N,M) :-
     Mp 'cont:' _,
     Mp < M,
     ppl_cont(N,Mp), !.
     
ppl_cont(N,N).           
     
     
/*---------------------------------*
*         editdeclar/0        *
*----------------------------------*/
editdeclar :- 
     nl, write('editions des declarations : '),
     /* get1(C), list([C], S), */ /* ancienne version incompatible avec swipl */
     get_str1(S), !,
     traite_editd(S), editdeclar, !.
editdeclar.

/*---------------------------------*
*         traite_editd/1           *
*----------------------------------*/

traite_editd("H") :- traite_editd("h"), !.
traite_editd("h") :-
     nl, write('--- editions de declarations :'),
     nl, write(' L : lister les declarations'),
     nl, write(' A : ajouter une declarations'),
     nl, write('    (fin = arret de l ajout)'),
     nl, write(' I : inserer une declaration'),
     nl, write(' M : mouvoir une declaration'),
     nl, write(' N : renumeroter les declarations'),
     nl, write(' D : effacer une declaration'),
     nl, write(' Z : effacer toutes les declarations'),
     nl, write(' H : aide'),
     nl, write(' Q : quitter l edition'),nl, !.

traite_editd("Q") :- !, fail.
traite_editd("q") :- !, fail.

traite_editd("L") :- traite_editd("l"), !.
traite_editd("l") :- 
          N 'dec:' T,
          write(N 'dec:' T), nl,
          fail.
traite_editd("l") :- !.
          
traite_editd("A") :- traite_editd("a"), !.
traite_editd("a") :- 
     nl, write('ed> '),
     read(T),
     (
      T == fin, ! 
      ;
      gnum(maxdec,N),
      assert(N 'dec:' T),
      traite_editd("a"), !
     ).

traite_editd("I") :- traite_editd("i"), !.
traite_editd("i") :- 
     nl, write(' Numero de la declaration a inserer (ou nul) : '),
     read(Num),
     (
      Num == nul, !
      ;
      Num 'dec:' _ ,
      nl, write('*** Il y a deja une declaration portant ce numero'), !
      ;
      nl, write(' Declaration : '),
      read(T),
      assert(Num 'dec:' T), !
     ).

traite_editd("M") :- traite_editd("m"), !.
traite_editd("m") :-
     nl, write(' Numero de la declaration a mouvoir (ou nul) : '),
     read(Num),
     (
      Num == nul, !, fail
      ;
      Num 'dec:' T, nl, write( Num 'dec:' T)
      ;
      nl, put(7), write('*** Numero de declaration inexistant '),fail
     ),
     nl, write(' Nouveau numero pour cette declaration : '),
     read(NNum),
     (
      NNum == nul, !, fail
      ;
      NNum == Num, !
      ;
      NNum 'dec:' _ , nl, put(7), 
      write('*** Il y a deja une declaration portant ce numero'), !, fail
      ;
      retract(Num 'dec:' T),
      assert(NNum 'dec:' T), !
      ).
     
traite_editd("N") :- traite_editd("n"), !.
traite_editd("n") :-
     renum_dec, !.

traite_editd("D") :- traite_editd("d"), !.
traite_editd("d") :- 
     nl, write(' Numero de la declaration a effacer : '),
     read(Num),
     (
      retract(Num 'dec:' _) 
     ;
      true
     ).

traite_editd("Z") :- traite_editd("z"), !.
traite_editd("z") :-
     nl, write(' Voulez-vous vraiment effacer toutes les declarations (o/n)'),
     /* get1(C), */ /* ancienne version incompatible avec swipl */
     get_str1(S), !,
     (
      S \== "O",
      S \== "o", !
     ;
      retract( _ 'dec:' _ ),
      fail
     ;
      true
     ), !.
     
traite_editd(_) :-
     nl, put(7), write('(editd) *** commande non reconnue').     

renum_dec :-
    maz(maxdec),
    renum_decb,
    renomme_dec.
    
renum_decb :-
     ppl_dec(N),
     retract( N 'dec:' T),
     gnum(maxdec,N),
     assert(dec(N,T)),
     renum_decb, !.

renum_decb.        

renomme_dec :-
     retract(dec(N,T)),
     assertz( N 'dec:' T),
     fail.
renomme_dec.

ppl_dec(N) :-
     M 'dec:' _ ,
     ppl_dec(N,M), !.
     
     
ppl_dec(N,M) :-
     Mp 'dec:' _,
     Mp < M,
     ppl_dec(N,Mp), !.
     
ppl_dec(N,N).            
     
/*---------------------------------*
*         figure_info/0            *
*----------------------------------*/
figure_info :-
     nl, write(' figure > '),
     /* get1(C), list([C], S), */ /* ancienne version incompatible avec swipl */
     get_str1(S), !,
     traite_figi(S), figure_info, !.

figure_info.   

traite_figi("Q") :- !, fail.
traite_figi("q") :- !, fail.

traite_figi("H") :- traite_figi("h"), !.
traite_figi("h") :-
     nl, write(' ----- figure info : '),
     nl, write(' L : lister toutes les figures elementaires'),
     nl, write(' C : lister toutes les figures connues '),
     nl, write(' T : lister tous les OG d un type'),
     nl, write(' S : lister la table des synonymes'),
     nl, write(' A : ajouter une figure elementaire'),
     nl, write(' H : aide'),
     nl, write(' Q : quitter figure info'), !.
     
traite_figi("L") :- !, traite_figi("l").
traite_figi("l") :-
         findall(fe(N,T,D,Df,Lr,Lp),fe(N,T,D,Df,Lr,Lp),L),
         affiche_liste(L, 23), !.

traite_figi("C") :- !, traite_figi("c").
traite_figi("c") :-
         findall(fe(N,T,0,Df,Lr,Lp),fe(N,T,0,Df,Lr,Lp),L),
         affiche_liste(L, 23), !.

traite_figi("T") :- !, traite_figi("t").
traite_figi("t") :-
         nl, write(' type : '), read(T),
         findall(fe(N,T,D,Df,Lr,Lp),fe(N,T,D,Df,Lr,Lp),L),
         nl, affiche_liste(L, 23), !.

traite_figi("S") :- !, traite_figi("s").
traite_figi("s") :-
         findall(X alias Y, X alias Y, L),
         affiche_liste(L, 23), !.
         
traite_figi("A") :- !, traite_figi("a").               
traite_figi("a") :- !, nl,
    write(' fe : nom  : '), read(Nom), nl,
    (Nom == fin, !
    ;
    write('      type : '), read(Ty), nl,
    write('      deg  : '), read(Deg), nl,
    write('      def  : '), read(Def), nl,
    write('     reps  : '), read(Lrs), nl,
    write('     parts : '), read(Lps), nl,
    inserb_fe(fe(Nom, Ty, Deg, Def, Lrs, Lps)),
    !, traite_figi("a")
    ).

traite_figi(_) :- nl, write(' Commande incorrecte ou non en service '), nl.

/*============ A COMPLETER =======================*/

/*---------------------------------*
*         raison_info/0            *
*----------------------------------*/
raison_info :-
     nl, write(' raison > '),
     /* get1(C), list([C], S),*/ /* ancienne version incompatible avec swipl */
     get_str1(S), !,
     traite_raisi(S), raison_info, !.

raison_info.   

traite_raisi("Q") :- !, fail.
traite_raisi("q") :- !, fail.

traite_raisi("H") :- traite_raisi("h"), !.
traite_raisi("h") :-
          nl, write('------ raisonnement info :'),
          nl, write(' L : lister toutes les proprietes '),
          nl, write(' C : lister les composantes connexes'),
          nl, write(' U : lister les proprietes non utilisees activement'),
          nl, write(' D : lister les drivations'),
          nl, write(' P : lister le PCG'),
          nl, write(' H : aide'),
          nl, write(' Q : quitter raison info'), !.
          
traite_raisi("l") :- !, traite_raisi("L").
traite_raisi("L") :-
    findall(sommet(N,P,Na,Np), sommet(N,P,Na,Np),L),
    affiche_liste(L,23), !.
traite_raisi("C") :- !, traite_raisi("c").
traite_raisi("c") :-
    findall(N repccx M, N repccx M, L),
    affiche_liste(L,23), !.
traite_raisi("U") :- !, traite_raisi("u").
traite_raisi("u") :-
    findall(sommet(N,P,_,Np), sommet(N,P,0,Np), L),
    affiche_liste(L,23), !.

traite_raisi("D") :- !, traite_raisi("d").
traite_raisi("d") :-
    findall(deriv(N,Ch,Nr),deriv(N,Ch,Nr), L),
    affiche_liste(L,23), !.

traite_raisi(_) :- nl, write(' Commande incorrecte ou non en service'), nl.

