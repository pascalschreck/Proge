/* Prédicats d'export pour le format solverviz */

/* Comme pour export.pl, les prédicats sont précédés de 'sviz' */

/* Ces prédicats sont un peu mieux écrits et plus évolués que ceux dans
 * 'export.pl'... */

sviz_export([Output := Cons | Tail]) :-
  write(Output), nl,
  Cons =.. [ConsName | _],
  (
    Cons = donne,
    write(donne)
  ;
    profil(ConsName, X),
    X = Smth >> _,
    Smth =.. Truc,
    write(Truc)
  ), !,
  nl,
  sviz_export(Tail).


