/*---------------------------------*
*         CRG.PRO                  *
*  Description des constructeurs   *
* de relations geometriques.       *
*                                  *
*----------------------------------*/

/*---------------------------------*
*        pprofil/2                 *
*  Profils des proprietes.         *
*----------------------------------*/
pprofil('=def', objet x objet).       /* affirmation d'une egalite : le deuxieme
                                        objet est forcement connu.
                                         En outre, il n'y aura pas d'introduction
                                        d'une nouvelle propriete dans le raisonnement.
                                      */
pprofil('=p=', point x point).
pprofil('=d=', droite x droite).
pprofil('=di=', dir x dir). 
pprofil('=c=', cercle x cercle).
pprofil('=l=', long x long).
pprofil(est_sur, point x lieu).        /* affirmation constructive d'une incidence :
                                          le lieu est forcement connu.
                                         En outre, il n'y aura pas d'introduction
                                        d'une nouvelle propriete dans le raisonnement.
                                      */
pprofil(appartient_a, point x lieu).   /* pourrait etre plus general :
                                         une droite appartient  une direction
                                         un bipoint appartient  un vecteur ...
                                       */
pprofil(iso, point x point x point).
pprofil(ortho, droite x droite).
pprofil(tangentdc, droite x cercle x point).
pprofil(tangentcc, cercle x cercle x point).
pprofil(pll, point x point x point x point).

/*---------------------------------*
*        ptitre/2                  *
*----------------------------------*/
ptitre('=p=', egalite):- !.
ptitre('=d=', egalite):- !.
ptitre('=di=', egalite):- !.  
ptitre('=c=', egalite):- !.
ptitre('=l=', egalite):- !.
ptitre('=h=', egalite):- !.
ptitre('=r=', egalite):- !.
ptitre('=a=',egalite):- !.
ptitre(eg, egalite):- !.
ptitre(est_sur, incid):- !.
ptitre(appartient_a, incid):- !.
ptitre('=def', egalite):- !.
ptitre( _, cond).


/*---------------------------------*
*        richesse/2                *
*----------------------------------*/
/* ???????    */

/*---------------------------------*
*        constructif/3             *
*----------------------------------*/
constructif( '=def', egalite, type).
constructif(est_sur, incident, 1).

/*---------------------------------*
*        ppermut/2                 *
*        pequiv/2                  *
* Voir aussi dans COG.PRO          *
*----------------------------------*/
ppermut('=p=', [[1,2], [2,1]]).
ppermut(iso,[[1,2,3], [1,3,2]]).
ppermut(pll,[[1,2,3,4], [2,3,4,1], [3,4,1,2], [4,1,2,3], [4,3,2,1], [3,2,1,4], [2,1,4,3], [1,4,3,2]]).

/*---- pequiv/2----------*/

 Prop pequiv Prop.
iso(A, B, C) pequiv iso(A, C, B).
pll(A, B, C, D) pequiv pll(B, C, D, A).
pll(A, B, C, D) pequiv pll(C, D, A, B).
pll(A, B, C, D) pequiv pll(D, A, B, C).
pll(A, B, C, D) pequiv pll(D, C, B, A).
pll(A, B, C, D) pequiv pll(C, B, A, D).
pll(A, B, C, D) pequiv pll(B, A, D, C).
pll(A, B, C, D) pequiv pll(A, D, C, B).
D ortho DD pequiv DD ortho D.
O diff Op pequiv Op diff O.




/*------------
	    à ajouter : calcul numérique associé
------------*/

