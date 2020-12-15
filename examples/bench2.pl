0 'dec:' point :: a donne.
1 'dec:' point :: b donne.
2 'dec:' point :: c donne.
3 'dec:' point :: d mentionne.
4 'dec:' point :: e mentionne.
5 'dec:' point :: f mentionne.
6 'dec:' point :: i cherche.

0 'cont:' dist(i,d) '=l=' dist(i,e).
1 'cont:' dist(i,e) '=l=' dist(i,f).
2 'cont:' dro(i,d) ortho dro(a,b).
3 'cont:' dro(i,e) ortho dro(b,c).
4 'cont:' dro(i,f) ortho dro(a,c).
5 'cont:' d est_sur dro(a,b).
6 'cont:' e est_sur dro(b,c).
7 'cont:' f est_sur dro(a,c).
/* Incenter of a circle. echec */
