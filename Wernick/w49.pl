0 'dec:' point :: a donne.
1 'dec:' point :: hb donne.
2 'dec:' point :: hc donne.
3 'dec:' point :: b cherche.
4 'dec:' point :: c cherche.

0 'cont:' hb '=p=' prj(b, dro(a, c)).
1 'cont:' hc '=p=' prj(c, dro(a, b)).

0 'cnd:' a diff b.
1 'cnd:' b diff c.
2 'cnd:' a diff c.