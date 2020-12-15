0 'dec:' point :: o donne.
1 'dec:' dir :: di donne.
2 'dec:' point :: a donne.
3 'dec:' point :: b donne.
9 'dec:' long :: l donne.
4 'dec:' droite :: d1 nomme dpdir(a,di).
5 'dec:' droite :: d2 nomme dpdir(b,di).
6 'dec:' droite :: d cherche.
7 'dec:' point :: m mentionne.
8 'dec:' point :: n mentionne.

0 'cont:' m est_sur d1.
1 'cont:' n est_sur d2.
2 'cont:' m est_sur d.
3 'cont:' n est_sur d.
4 'cont:' o est_sur d.
4 'cont:' di '=di=' di.   /*   utile pour le moment */
5 'cont:' dist(a,m) + dist(b,n) '=l=' l.
6 'cont:' d1 diff d2.


