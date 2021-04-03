/* Fonctions d'export de figure dans geogebra */

/* J'ai préfixé chaque fonction par "export", pour être sûr qu'il n'y ait pas de conflits... */

export_prog(Pgm) :-
  /* Ouverture du fichier d'export */
  nl, write("Nom du fichier d'export : "), read_line_to_string(current_input, Filename),
  open(Filename, write, ExportFile, [create([default])]),
  /* Ce prédicat permet à export_prog_aux d'accéder au fichier sans le passer
     explicitement en argument. */
  assert(export_file(ExportFile)),
  write(ExportFile, '<geogebra format="4.0">'), nl(ExportFile),
  write(ExportFile, '<construction>'), nl(ExportFile),
  export_prog_aux(Pgm),
  write(ExportFile, '</construction>'), nl(ExportFile),
  write(ExportFile, '</geogebra>'), nl(ExportFile),
  close(ExportFile),
  retract(export_file(ExportFile)).


/* Sur chaque objet "donne", on demande les coordonnées à l'utilisateur */
export_prog_aux([(OG := donne) | Suite ]) :-
  export_file(File),
  nl, write('Définition de '), write(OG), write(' : '), nl,
  write('coordonnée x : '), read(X), nl, write('coordonnée y : '), read(Y),
  /* On vérifie que X et Y sont flottants ou entiers*/
  (\+(((float(X) ; integer(X)), (float(Y) ; integer(Y)))) ->
  (write('Erreur, les coordonnées doivent être des nombres valides.')));
  export_prog_aux(Suite).
  
export_prog_aux([H | T]) :-
  export_prog_aux(T).

export_prog_aux([]).