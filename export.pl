/* Fonctions d'export de figure dans geogebra */

/* J'ai préfixé chaque fonction par "export", pour être sûr qu'il n'y ait pas de conflits... */

export_prog(Pgm) :-
  /* Ouverture du fichier d'export */
  nl, write("Nom du fichier d'export : "), read_line_to_string(current_input, Filename),
  open('geogebra.xml', write, ExportFile, [create([default])]),
  /* Ce prédicat permet à export_prog_aux d'accéder au fichier sans le passer
     explicitement en argument. */
  assert(export_file(ExportFile)),
  write(ExportFile, '<geogebra format="4.0">'), nl(ExportFile),
  write(ExportFile, '<construction>'), nl(ExportFile),
  export_prog_aux(Pgm),
  write(ExportFile, '</construction>'), nl(ExportFile),
  write(ExportFile, '</geogebra>'), nl(ExportFile),
  close(ExportFile),
  retract(export_file(ExportFile)),
  export_zip_ggb(Filename).

export_zip_ggb(Filename) :-
  atom_concat(Filename, '.ggb', FullFilename),
  write(Command),
  (
    fork(child) -> exec(zip(FullFilename, 'geogebra.xml')) ; true
  ).
  
export_free_point(File, Name, X, Y) :-
  format(File, '<element type="point" label="~w"> ~n', [Name]),
  write(File, '<show object="true" label="true"/>'), nl(File),
  format(File, '<coords x="~w" y="~w" z="1.0" /> ~n', [X, Y]),
  write(File, '</element>'), nl(File).

export_command(File, Command, Input, Output) :-
  format(File, '<command name="~w">~n', [Command]),
  write(File, '<input '),
  length(Input, L),
  export_command_aux(File, Input, L),
  write(File, '/>~n'),
  format(File, '<output a0="~w"/>~n', [Output]),
  write(File, '</command>~n').
  
export_command_aux(File, [InputHead | InputTail], Length) :-
  length(InputTail, L),
  Number is (Length - L - 1),
  format(File, 'a~w="~w" ', [Number, InputHead]),
  export_command_aux(File, InputTail, Length).
  
export_command_aux(File, [], Length).
  
/* Sur chaque objet "donne", on demande les coordonnées à l'utilisateur */
export_prog_aux([(OG := donne) | Suite ]) :-
  export_file(File),
/*  nl, write('Définition de '), write(OG), write(' : '), nl,
  write('coordonnée x : '), read(X), nl, write('coordonnée y : '), read(Y),*/
  /* On vérifie que X et Y sont flottants ou entiers*/
  random(0.0, 5.0, X),
  random(0.0, 5.0, Y),
  export_free_point(File, OG, X, Y),
  export_prog_aux(Suite).

/* Points */
export_prog_aux([(OG := symp(A, B)) | Suite ]) :-
  export_file(File),
  export_command(File, 'Mirror', [B, A], OG),
  export_prog_aux(Suite).
  
export_prog_aux([OG := mil(A, B) | Suite]) :-
  export_file(File),
  export_command(File, 'Midpoint', [A, B], OG),
  export_prog_aux(Suite).
  
/* Droites */
  
export_prog_aux([OG := dro(A, B) | Suite]) :-
  export_file(File),
  export_command(File, 'Line', [A, B], OG),
  export_prog_aux(Suite).
  
export_prog_aux([OG := dpdir(A, B) | Suite]) :-
  export_file(File),
  export_command(File, 'Line', [A, B], OG),
  export_prog_aux(Suite).

export_prog_aux([OG := dorth(A, B) | Suite]) :-
  export_file(File),
  export_command(File, 'PerpendicularLine', [B, A], OG),
  export_prog_aux(Suite).
  
/* Cercles */
export_prog_aux([OG := ccp(A, B) | Suite]) :-
  export_file(File),
  export_command(File, 'Circle', [A, B], OG),
  export_prog_aux(Suite).
  
export_prog_aux([OG := ccr(A, B) | Suite]) :-
  export_file(File),
  export_command(File, 'Circle', [A, B], OG),
  export_prog_aux(Suite).
  
/* Longueurs */
export_prog_aux([OG := dist(A, B) | Suite]) :-
  export_file(File),
  export_command(File, 'Distance', [B, A], OG),
  export_prog_aux(Suite).
  
/* Directions (gérées à l'aide de vecteurs) */
export_prog_aux([OG := dird(A) | Suite]) :-
  export_file(File),
  export_command(File, 'Direction', [A], OG),
  export_prog_aux(Suite).
  
export_prog_aux([OG := diro(A) | Suite]) :-
  export_file(File),
  export_command(File, 'PerpendicularVector', [A], OG),
  export_prog_aux(Suite).
  
/* Intersections */
export_prog_aux([OG := interdd(A, B) | Suite]) :-
  export_file(File),
  export_command(File, 'Intersect', [B, A], OG),
  export_prog_aux(Suite).
export_prog_aux([OG := intercd(A, B) | Suite]) :-
  export_file(File),
/* Recherche de tous les représentants de cette intersection (pour obtenir les noms des intersections) */
  findall(Nom, rep_fe(intercd(A, B), fe(Nom, _, _, _, _, _)), ListeNoms),
  /* Suppression des doublons en transformant la liste renvoyée par findall/3
  en ensemble */
  list_to_set(ListeNoms, Noms),
  export_command(File, 'Intersect', [B, A], Noms),
  export_prog_aux(Suite).
  
/* Relations d'Euler */
  
export_prog_aux([OG := eulero(H, G) | Suite]) :-
  export_file(File),
  /* On créé un nom unique 'temporaire' pour le symétrique dont on a besoin 
  pour la relation d'euler */
  concat_atom([OG, '_{eulero}'], NomTemp),
  export_command(File, 'Mirror', [H, G], [NomTemp]),
  export_command(File, 'Midpoint', [NomTemp, G], [OG]),
  export_prog_aux(Suite).
  
export_prog_aux([OG := eulerh(G, O) | Suite]) :-
  export_file(File),
  concat_atom([OG, '_{eulerh}'], NomTemp),
  export_command(File, 'Mirror', [O, G], [NomTemp]),
  export_command(File, 'Mirror', [G, NomTemp], [OG]),
  export_prog_aux(Suite).
  
export_prog_aux([OG := eulerg(H, O) | Suite]) :-
  export_file(File),
  concat_atom([OG, '_{eulerh}'], NomTemp),
  export_command(File, 'Midpoint', [O, H], [NomTemp]),
  export_command(File, 'Midpoint', [O, NomTemp], [OG]),
  export_prog_aux(Suite).
  
export_prog_aux([pour _ dans _ faire Instr| Suite]) :-
  export_prog_aux(Instr),
  export_prog_aux(Suite).
  
export_prog_aux([H | T]) :-
  export_prog_aux(T).

export_prog_aux([]).
