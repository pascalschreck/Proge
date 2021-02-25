
/*---------------------------------*
*         PROGE.PRO                *
*    fichier principal de PROGE    *
*                                  *
*----------------------------------*/
:- dynamic(fichiers_charges), 
   dynamic(defop),
   dynamic(gnum/2), 
   dynamic(batchmode/0),
   dynamic(num_pgc/1), 
   dynamic(pcg/2),
   dynamic('dec:'/2),
   dynamic(alias/2),
   dynamic(fe/6),
   dynamic(cherche/1),
   dynamic(real/1),
   dynamic('cont:'/2),
   dynamic('cnd:'/2),              /* ajout PS 02/2021   */
   dynamic(sommet/4),
   dynamic(trappe_echec),
   dynamic(rout/1),
   dynamic(config/10),
   dynamic(spyre),
   dynamic(debog),
   dynamic(step_regle),
   dynamic(bavard),
   dynamic(echo),
   dynamic(rout/1),
   dynamic(inhib/2),
   dynamic(minact/1),
   dynamic(minpas/1),
   dynamic(deriv/3),
/*   dynamic(concl2hyp/2), */ /* grrrrrrrr */
   dynamic(deja_construit/1),
   dynamic(mal_def/2),
   dynamic(rel_part/1),
/*   dynamic(rech_crg_abs/2), *//* hum ... a verifier : grrrr */
   /* dynamic(synonymes/2),*/ /* grrr */
   dynamic(repccx/2),
   dynamic(exist_config),
   dynamic(no_spam).


   

repertoire('Wernick/').
batchmode. 

/*
echo.

debog.

bavard.
*/

/*---------------------------------*
*         defop/0                  *
*    definition des operateurs     *
*----------------------------------*/

:-   op(800, fx, abolishe),
     op(800, fx, abolishl),
     op(800, fx, abolish),
     op(600, xfx, 'dec:'),
     op(600, xfx, 'cont:'),
     op(600, xfx, 'cnd:'),         /* ajout PS 02/2021 */
     op(600, fx, existe),          /* 02/2021 :  */
     op(650, xfy, tel_que),        /* */
     op(400, xfx, :>),
     op(50, xfy, &),
     op(400, xfx, alias),
     op(50, xfy, x),
     op(400, xfx, >>),
     op(500, xfx, 'except:'),
     op(300, xfx, [eg,diff]),
     op(500, fx, 'maj:'),
     op(400, xfx, ==>),
     op(300, xf, donne),
     op(300, xf, cherche),
     op(300, xf, mentionne),
     op(300, xfx, nomme),
     op(300, fx, pas_connu),
     op(300, fx, defini_par),
     op(30, xfx, ::),
     op(50, xfx, app),
     op(600, xfx, equiv),
     op(600, xfx, pequiv),
     op(600, xfx, equivdef),
     op(200, xfx, repccx),
     op(650, xfx, #),
     op(590, fx, si),    /* modif PS 02:2021 (avant 600, mais utilisé dans cnd:) */
     op(590, xfx, si),
     op(500, xfx, alors),
     op(475, xfx, sinon),
     op(600, fx, pour),
     op(550, xfx, dans),
     op(450, xfx, faire),
     op(400, xfx, et),
     op(425, fx, soit),
     op(451, xfy, ou),
     op(600, xfx, : ),
     op(530, xfx, '=l='),
     op(530, xfx, '=p='),
     op(530, xfx, '=d='),
     op(530, xfx, '=c='),
     op(530, xfx, '=di='),
     op(530, xfx, '=a='),
     op(300, fx, connu),
     op(300, fx, differents),
     op(300, xfx, est_sur),
     op(300, xfx, incid),
     op(300, xfx, ortho),
     op(400, xfx, :=),
     op(300, xfx, hors_de),
     op(400, xfx, de),
     op(300, fx, type),
     op(200, xfy, /), 
     op(700, xfx, not),assert(defop).
     

defop :-
     op(800, fx, abolishe),
     op(800, fx, abolishl),
     op(800, fx, abolish),
     op(600, xfx, 'dec:'),
     op(600, xfx, 'cont:'),
     op(600, xfx, 'cnd:'),         /* ajout PS 02/2021 */
     op(650, xfy, tel_que),        /* ajout PS 02/2021 */
     op(600, fy, existe),
     op(400, xfx, :>),
     op(50, xfy, &),
     op(400, xfx, alias),
     op(50, xfy, x),
     op(400, xfx, >>),
     op(500, xfx, 'except:'),
     op(300, xfx, [eg,diff]),
     op(500, fx, 'maj:'),
     op(400, xfx, ==>),
     op(300, xf, donne),
     op(300, xf, cherche),
     op(300, xf, mentionne),
     op(300, xfx, nomme),
     op(300, fx, pas_connu),
     op(300, fx, defini_par),
     op(30, xfx, ::),
     op(50, xfx, app),
     op(600, xfx, equiv),
     op(600, xfx, pequiv),
     op(200, xfx, repccx),
     op(650, xfx, #),
     op(600, fx, si),
     op(500, xfx, alors),
     op(450, xfx, sinon),
     op(600, fx, pour),
     op(550, xfx, dans),
     op(450, xfx, faire),
     op(400, xfx, et),
     op(451, xfx, ou),
     op(600, xfx, : ),
     op(530, xfx, '=l='),
     op(530, xfx, '=p='),
     op(530, xfx, '=d='),
     op(530, xfx, '=c='),
     op(530, xfx, '=di='),
     op(530, xfx, '=a='),
     op(300, fx, connu),
     op(300, fx, differents),
     op(300, xfx, est_sur),
     op(300, xfx, incid),
     op(300, xfx, ortho),
     op(400, xfx, :=),
     op(200, xfy, /),
     op(700, xfx, not).


/*---------------------------------*
*         charge/0                 *
*    chargement des fichiers       *
*----------------------------------*/
charge :- fichiers_charges, !.     
         
charge :-
     defop,!,
     nl, write(' chargement des fichiers en cours :'),
     nl, tab(5), write('fichier TYPE.PRO ........'), consult(type), write(consulte),
     nl, tab(5), write('fichier OG.PRO ..........'), consult(og), write(consulte),
     nl, tab(5), write('fichier COG.PRO..........'), consult(cog),write(consulte),
     nl, tab(5), write('fichier WERNICK_COG.PRO..'), consult(cog_wernick),write(consulte),
     nl, tab(5), write('fichier CRG.PRO..........'), consult(crg),write(consulte),
     nl, tab(5), write('fichier INIT.PRO.........'), consult(init),write(consulte),
     nl, tab(5), write('fichier UTIL.PRO.........'), consult(util),write(consulte),
     nl, tab(5), write('fichier FIGURE.PRO.......'), consult(figure),write(consulte),
     nl, tab(5), write('fichier RAISON.PRO.......'), consult(raison),write(consulte),
     nl, tab(5), write('fichier MOTEUR.PRO.......'), consult(moteur),write(consulte),
     nl, tab(5), write('fichier MREGLE.PRO.......'), consult(mregle),write(consulte),       
     nl, tab(5), write('fichier REGLES.PRO.......'), consult(regles),write(consulte),
     nl, tab(5), write('fichier REGLES_WERNICK.PRO'), consult(regles_wernick),write(consulte),
     nl, tab(5), write('fichier EXCEPT...........'), consult(except),write(consulte),
     nl, tab(5), write('fichier INTER............'), consult(inter), write(consulte),
     nl, tab(5), write('fichier TEST.............'), consult(test), write(consulte),
/*     nl, tab(5), write('fichier GRAPH............'), consult(graph), write(consulte), */
     nl, tab(5), write('fichier DEBOG............'), consult(debog),write(consulte),
     nl,consult(shellp),
     assert(fichiers_charges).
     /*=========    a completer ===============*/
     
/*---------------------------------*
*         run/0                    *
*----------------------------------*/
run :- charge, 
       abolishe(num_prog_cur), 
       abolishe(num_pcg), 
       gnum(num_pcg,Num), 
       assert(num_prog_cur(Num)).


/*----------------------------------*
*	cldb			    *
*-----------------------------------*/

cldb :- abolishl [fe,deja_construit,alias,decl,:=,lu,'dec:', 'cont:', cherche, minact, pcg],
	abolishl [minpas, sommet, repccx, deriv, cont, dec, 'max$point','max$droite', rout],
	abolishl ['max$cercle', 'max$long', 'max$dir','max$list','max$num_pcg',har, som, nar, maxcont, maxdec, '$$pile$$'].

abolishe(F) :- Fact =.. [F],
	       retract(Fact),
	       fail.

abolishe(F) :- Fact =.. [F,_],
	       retract(Fact),
	       fail.

abolishe(F) :- Fact =.. [F,_, _],
	       retract(Fact),
	       fail.

abolishe(F) :- Fact =.. [F,_, _, _],
	       retract(Fact),
	       fail.
abolishe(F) :- Fact =.. [F,_, _, _, _],
	       retract(Fact),
	       fail.
abolishe(F) :- Fact =.. [F,_, _, _,_,_],
	       retract(Fact),
	       fail.
abolishe(F) :- Fact =.. [F,_, _, _,_,_,_],
	       retract(Fact),
	       fail.
abolishe(F) :- Fact =.. [F,_, _, _,_,_,_,_],
	       retract(Fact),
	       fail.
abolishe(F) :- Fact =.. [F,_, _, _,_,_,_,_,_],
	       retract(Fact),
	       fail.
abolishe(F) :- Fact =.. [F,_, _, _,_,_,_,_,_,_],
	       retract(Fact),
	       fail.
abolishe(F) :- Fact =.. [F,_, _, _,_,_,_,_,_,_,_],
	       retract(Fact),
	       fail.
abolishe(F) :- Fact =.. [F,_, _, _,_,_,_,_,_,_,_,_],
	       retract(Fact),
	       fail.

abolishe(_).

abolishl([]).
abolishl([P|S]) :- abolishe(P), abolishl(S).

consultc(F) :-
	nl, write(' doit-on charger '), write(F), write(' ? '),
	read(R),
	(
	R == oui,
	consult(F)
	;
	write('... non consulte...')
        ).
        
 /* paches beurk */      
 /* 
loade(Nom) :- 
     atom_concat('examples/',Nom, File), consult(File).
*/

loade(Nom) :- 
     repertoire(Rep),
     atom_concat(Rep,Nom, File0), 
     atom_concat(File0,'.pl', File), consult(File).
     
list(L,L).

traite_roule(quit) :- !.
traite_roule(Nom) :- loade(Nom), roule.
traite_roule(_).

  /* fin */   

monhalt :- not(batchmode), !.
monhalt :- halt.
  

:- run, write(' nom de l''exercice : '), read(Nom),  traite_roule(Nom), monhalt.


