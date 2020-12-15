/*------------------------------------*
*         MREGLES.PRO                 *
*  Manipulation des regles ...        *
*-------------------------------------*/


/*
    Structures de donnees :
  Une regle non disjonctive est un terme de la forme :
  Num  # si  
               Liste de proprietes geometriques
           et 
               Liste de verifications sur la figure
         alors
               Liste d'actions sur la configuration

Selon chaque CRG, on distinguera une action sur la figure
et une action sur le raisonnement.
Pour chaque propriete geometrique en premisse, on indiquera 
le nombre de degre de libertes qu'on espere restreindre par
la regle ( ou peut ˆtre que ceci est calculable ...)

Une regle disjonctive est de la forme :
  Num  # si  
               Liste de proprietes geometriques
           et 
               Liste de verifications sur la figure
         alors
           	soit Liste Cond et Liste d'actions sur la configuration
           ou
               soit Liste Cond et Liste d'actions sur la configuration
           ou
               soit Liste Cond et Liste d'actions sur la configuration
           ...

On peut ensuite avoir des regles disjonctives exclusives ou non exclusives :
	- pour les regles non exclusives, les listes de conditions sont vides,
       tous les cqs sont a examiner.
        - pour les regles exclusives les conditions d'application s'excluent 
       mutuellement (cette verif. est a la charge de l'expert...
Si cette distinction n'a pas d'importance pour l'application de la regle en
elle-meme, elle est cruciale quant a la generation du programme de construction.

*/

/*---------------------------------*
*   Projections ....               *
*----------------------------------*/

regle_num(Num # _, Num).
regle_cond( _ # si Lcond et _ alors _, Lcond).
regle_veri( _ # si _ et Lveri alors _, Lveri).
regle_concl( _ # si _ alors Lact, Lact).
regle_var(_ # si Lcond et _ alors _, Lvar) :- l_terme_var(Lcond,Lvar).

/*---------------------------------*
*   Recherche d'une regle ...      *
*----------------------------------*/

/*--- par son numero : ------------*
*   num_regle/2                   *
*----------------------------------*/
num_regle(Num, Num # Corps) :- Num # Corps.

/*---- par une premisse : ---------*
*   pr_regle/2                     *
*----------------------------------*/
pr_regle(Cond, Num # si Lcond et Lver alors Lact) :-
    Num # si Lcond et Lver alors Lact,
    Cond =.. [CRG|_],
    Lcond = [Condr|_],
    Condr =.. [CRG|_].    /* on pourrait tenter une "preunification" */

/*---------- general ...           *
*   choisir_regle/1                *
* Les regles sont prises dans l'   *
* ou elles se trouvent dans la     *
* base de faits.                   *
*----------------------------------*/    

choisir_regle(Num # si Lcond et Lver alors Lact) :-
    Num # si Lcond et Lver alors Lact.
/*---------------------------------*
*     regles disjonctives	   *
*----------------------------------*/
/*-------------- distinctions :            		*/
disjonctive(_ # si _ et _ alors _ ou _).
non_exclusive(_ # si _ et _ alors soit [] et _ ou _).
exclusive(_ # si _ et _ alors soit [_|_] et _ ou _).

/*--------------------------------------*
*	cas_exclusifs/2			*
* si le premier argument est une regle  *
* exclusive, alors le deuxieme arg.	*
* est la liste des cas sous la forme	*
* (List_cond >> List_actions)		*
*---------------------------------------*/
cas_exclusifs(_ # si _ et _ alors soit Cas_part1 ou Autres_cas, L_cas) :-
	not( Cas_part1 = [] et _),
	recup_cax(soit Cas_part1 ou Autres_cas, L_cas).

recup_cax(soit Cas_part et Act,[Cas_part >> Act]) :- not( Cas_part = _ ou _).
recup_cax(soit Cas_part1 et Act1 ou Autres,[Cas_part1 >> Act1 | L]) :-
	recup_cax(Autres,L).

/*--------------------------------------*
*	cas_possibles/2			*
* Meme genre que cas_exclusifs, mais on	*
* ne recupere que la liste des actions	*
* (les conditions sont toujours vraies)	*
*---------------------------------------*/
cas_possibles(_ # si _ et _ alors soit Cas_part1 ou Autres_cas, L_cas) :-
	recup_acts(soit Cas_part1 ou Autres_cas, L_cas).

recup_acts(soit [] et Acts, [[] >> Acts]).
recup_acts(soit [] et Act1 ou Suite, [[] >> Act1|L]) :-
	recup_acts(Suite,L).

concl([],[]) :- !.
concl([_ >> Conc | Suite], [Conc | Suitec]) :-
	concl(Suite, Suitec).

/* ajout PS2012 .... perte due à je ne sais pas quoi ! */
concl2hyp([],[]) :- !.
concl2hyp([C|S],[C>>[]|Sp]) :- concl2hyp(S, Sp).
