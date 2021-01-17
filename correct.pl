/* corrections du 28 mars 92

 Objet de la correction :
 ========================
 Traite comme elle l'est actuellement, la fusion de deux objets n'est pas
 très satisfaisante, plusieurs problmes se posent qui découlent du même
 problme fondamental : traduire l'identit entre deux objets.

 Ces problmes ont t mis en vidence notamment à la rsolution de l'
 exo8b :

 1- On pose, à un moment, l'galit o1 = o3, or cette galit impose
 que c1 = c3 puisque le cercles c1 et c3 ont même rayon ... Progé ne trouve
 pas cette égalité.

 2- Un peu plus loin, on trouve ces dfinitions de a1 et a3 (via deux listes) :
	a3 := intercc(c1, c3) 	et 	a1 := intercc(c3, c1),
 en dehors de l'inconsistance de l'intersection (c1 = c3 !), on devrait
 avoir, au moins formellement que a1 = a3, or Prog ne dcouvre pas cette
 galit.
 3- Plus loin encore, alors que l'galit a1 = a3 a t suppose, on trouve
 cette dfinition de s :
		s := cr2p(long00, a1, a1)
 a priori surprenante, y aurait-il un bug dans les oprations faisant
 voluer une dfinition ? En fait, avant de dclarer que a1 = a3, on
 avait dj la dfinition partielle de s : [a1/point/incid, a3/point/incid]
 cette dfintion partielle n'a pas t remise en cause par la supposition
 a1 = a3 et la promotion d'une df. ne contrle plus les synonymies sauf
 au placement dans le programme (sinon on aurait eu s := cr2p(long00,a1,a3)).

 Pourquoi ces problmes ?
 ========================
 La fusion de deux fe est pour le moment limite  ces deux figures et n'a
 pas d'influence sur les autres objets dpendants de ces deux fe. Il
 faudrait donc rviser toute la figure aprs une fusion ... ce que nous
 avions pens viter par l'utilisation d'une table de synonymes !

 Un algo de révision
 ===================
 Aprs fusion de O1 et O2
	Pour toutes les figures lmentaires Fe de la figure
		si Fe a une def partielle
		  alors si O1 et O2 apparaissent en 2 occurences
			   distinctes de cette df.
			alors en liminer une
			sinon pas de changement
		sinon Fe a une df complte (Deg = 0)
			si O1 ou O2 intervient dans cette df.
			alors chercher une autre figure Feb rpondant
				 cette df. (Feb \= Fe)
			      si il existe une telle figure
			       alors fusion de Fe et Feb
			       sinon rien
			sinon rien

 Un algo de contrôle
 ===================
 Au moment de donner une df. complte  une figure lmentaire,
 chrecher s'il n'existe pas dans la figure une autre fe rpondant
  cette df. si oui alors fusion
		sinon traitement et promotion normale.

*/
/* Rappel de l'ancienne version du fusion (MS-dos):
===================================================
fusion_fe(Fe1, Fe2) :-
    Fe1 = fe(Nom1, Ty, Deg1, Def1, Lr1, Lp1),
    Fe2 = fe(Nom2, Ty, Deg2, Def2, Lr2, Lp2),
    def_fin(Deg1, Def1, Deg2, Def2, Degi, Defi),
    Fei = fe(Nom2, Ty, Degi, Defi, Lr2, Lp2),
    ajoute_repr_l(Lr1, Fei, Feii),
    ajoute_parts(Lp1, Feii, Fef),
    retract(fe(Nom1, Ty, _, _, _, _)),
    retract(fe(Nom2, Ty, _, _, _, _)),
    assert_fig(Fef).


def_fin(_, _, 0, Def, 0, Def) :- !.
def_fin(0, Def, _, _, 0, Def) :- !.
def_fin(_, _, Deg, Def, Deg, Def).

aprs le dernier assert_fig(Fef) on peut ajouter revise_fig(Nom2, Nom1)
dont la dfinition est la suivante
*/
/*------------------------------------------------------------------*
		revise_fig/2
	revise(Nom1, Nom2)
	le premier arg est le nom figurant effectivement dans la fe
	on suppose que la table des syn. a dj t mise  jour !
*-------------------------------------------------------------------*/
revise_fig(Nom1, Nom2) :-
	cherche_fe(Fe),
	traite_fus(Nom1, Fe),
	fail.

revise_fig(_,_).

traite_fus(Nom, Fe) :-
	fe_def(Fe, Def),
	partielle(Def),!,
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
	Feb \== Fe,
	fe_nom(Fe, Nom3),
	fe_nom(Fe, Nom4),
	majsyn(Nom3, Nom4),
	fusion(Nom3, Nom4).


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
supp_doub(Nom, 0, _, 0, []) :- !,
	point_arret(supp_doub).		/* ceci ne devrait pas se produire */
supp_doub(Nom, Deg, [N/T/ti|S], Ndeg, Ndef):-
	synonymes(Nom, N), !,
	deg_titre(Ti, Dr),
	Dg is Deg-Dr,
        supp_doub(Nom, Dg, S, Ndeg, Ndef).
supp_doub(Nom, Deg, [Part|S], Ndeg, [Part|Ndef]) :-
	supp_doub(Nom, Deg, S, Ndeg, Ndef).


/* REVISION de def_fe :
=========================*/
def_fe(Def, fe(Nom, Ty, Adef, Parts, Reps)) :-
	fe(Nom, Ty, Adef, Parts, Reps),
	Adef equivdef Adf,
	id_syn(Def, Adf).

/* REVISION de equivdef :
==========================
(permutations pour des reprsentants dcomposanbles ou non)
     A faire dans COG
*/

A equivdef B :- A equiv B.
dro(A,B) equivdef dro(B,A).
cr2p(R,A,B) equivdef cr2p(R,B,A).
