/* Prédicats d'export pour le format solverviz */

/* Comme pour export.pl, les prédicats sont précédés de 'sviz' */

/* Ces prédicats sont un peu mieux écrits et plus évolués que ceux dans
 * 'export.pl'... */

% sviz_export(+Pgm)
% Ce prédicat est le plus général, il appelle le prédicat de traduction et
% écrit la sortie dans un fichier
sviz_export(Pgm) :-
  sviz_export_aux(Pgm, DOM),
  nl, write('Nom du fichier d\'export: '),
  read_line_to_string(current_input, Filename),
  atom_concat(Filename, '.xml', FilenameXML),
  open(FilenameXML, write, Stream),
  xml_write(Stream, DOM, []),
  close(Stream),
  nl, write('Export réussi !'), nl.


% sviz_export_aux(+Pgm, -DOM)
% Étant donné un programme de construction, génère le DOM XML correspondant
% pour le format solverviz
sviz_export_aux([], []).

% Cas donné
sviz_export_aux([Output := (donne) | Tail], [SvizHead | SvizTail]) :-
  % Quand l'objet est donné, on génère un objet aléatoire dans le plan.
  % On récupère le type de l'objet
  fe(Output, Type, _, _, _, _),
  sviz_generate_random_object(Output, Type, SvizHead),
  % Appel récursif
  sviz_export_aux(Tail, SvizTail).

% Cas spécial pour intercd
sviz_export_aux([_ := Cons | Tail], [SvizHead | SvizTail]) :-
  Cons =.. [ConsName, A, B],
  ConsName = intercd,
  sviz_fetch_signature(ConsName, ArgTypes),
  sviz_generate_arguments(ArgTypes, [A, B], Elements),
  sviz_proge(ConsName, SvizConsName),
  % Get actual name of the objects
  findall(OutName, rep_fe(intercd(A, B), fe(OutName, _, _, _, _, _)), NameList),
  list_to_set(NameList, NameSet),
  SvizHead = element(SvizConsName, [out=NameSet], Elements),
  write(SvizHead),
  sviz_export_aux(Tail, SvizTail).

% Cas spécial pour le symmétrique (ordre des arguments étrange)
sviz_export_aux([Output := Cons | Tail], [SvizHead | SvizTail]) :-
  Cons =.. [ConsName | Arguments],
  ConsName = symp,
  sviz_fetch_signature(ConsName, ArgTypes),
  reverse(ArgTypes, RevArgTypes),
  reverse(Arguments, RevArguments),
  sviz_generate_arguments(RevArgTypes, RevArguments, Elements),
  sviz_proge(ConsName, SvizConsName),
  SvizHead = element(SvizConsName, [out=Output], Elements),
  sviz_export_aux(Tail, SvizTail).

% Autres cas
sviz_export_aux([Output := Cons | Tail], [SvizHead | SvizTail]) :-
  Cons =.. [ConsName | Arguments],
  sviz_fetch_signature(ConsName, ArgTypes),
  sviz_generate_arguments(ArgTypes, Arguments, Elements),
  sviz_proge(ConsName, SvizConsName),
  SvizHead = element(SvizConsName, [out=Output], Elements),
  % Appel récursif
  sviz_export_aux(Tail, SvizTail).

% Boucle 'pour'
sviz_export_aux([pour Name dans _ faire Instr | Tail], Sviz) :-
  sviz_export_aux(Instr, SvizHead),
  sviz_export_aux(Tail, SvizTail),
  append(SvizHead, SvizTail, Sviz).

% Pour les instructions non traitées
sviz_export_aux([_ | Tail], SvizTail) :-
  sviz_export_aux(Tail, SvizTail).

% sviz_generate_arguments(+Argtypes, +Arguments, -DOM)
% Génère une liste d'arguments avec une liste de types d'arguments

% Cas de base
sviz_generate_arguments([], [], []).
sviz_generate_arguments([ArgTypesHead | ArgTypesTail], [ArgHead | ArgTail], [DOMHead | DOMTail]) :-
  sviz_generate_single_argument(ArgTypesHead, ArgHead, DOMHead),
  sviz_generate_arguments(ArgTypesTail, ArgTail, DOMTail).

% sviz_generate_single_argument(+ArgType, +Arg, -DOM)
% Génère une balise XML pour un argument donné
sviz_generate_single_argument(ArgType, Arg, DOM) :-
  sviz_proge(ArgType, SvizType),
  DOM = element(SvizType, [value=Arg], []).



% random_low(-Number)
% random_high(-Number)
% Predicates to store the superior and inferior limits of the random coordinate
% generation.
sviz_random_low(-5.0).
sviz_random_high(5.0).

% generate_random_object(+Name, +Type, -Object)
% Ce prédicat génère un objet aléatoire d'un certain type.
% Il est utilisé pour les objets donnés
sviz_generate_random_object(_, coord, Object) :-
  % Génération d'une coordonnée aléatoire
  sviz_random_low(Rl),
  sviz_random_high(Rh),
  random(Rl, Rh, Coord),
  Object = element(literal, [value=Coord], []).


sviz_generate_random_object(Name, point, Object) :-
  % Pour un point, on génère 2 coordonnées
  sviz_generate_random_object(_, coord, CoordX),
  sviz_generate_random_object(_, coord, CoordY),
  Object = element(point, [out=Name], [CoordX, CoordY]).

sviz_generate_random_object(Name, droite, Object) :-
  % Pour une droite: on génère 2 points aléatoires
  % Génération de noms pour les points générés
  atom_concat(Name, '_a', AName),
  atom_concat(Name, '_b', BName),
  sviz_generate_random_object(AName, point, A),
  sviz_generate_random_object(BName, point, B),
  Object = [A, B, element(line, [out=Name],
    [
      element(point, [value=AName], []),
      element(point, [value=BName], [])
    ])].

sviz_generate_random_object(Name, cercle, Object) :-
  % Pour un cercle, on génère 2 points random
  atom_concat(Name, '_a', AName),
  atom_concat(Name, '_b', BName),
  sviz_generate_random_object(AName, point, A),
  sviz_generate_random_object(BName, point, B),
  Object = [A, B, element(circle_center_point, [out=Name],
           [
             element(point, [value=AName], []),
             element(point, [value=BName], [])
           ])].

% sviz_fetch_signature(+ConsName, -Argtypes)
% Récupère les types des arguments pour un constructeur donné
sviz_fetch_signature(ConsName, Argtypes) :-
  profil(ConsName, Arg >> _),
  Arg =.. ArgList,
  % Nettoyage du prototype
  sviz_remove_x(ArgList, Argtypes).
  
% sviz_remove_x(+Proto, -CleanProto)
% Ce prédicat supprime les opérateurs 'x' dans une liste qui représente un
% prototype de constructeur
sviz_remove_x([], []).
sviz_remove_x([x | ProtoTail], CleanProto) :-
  sviz_remove_x(ProtoTail, CleanProto).
sviz_remove_x([Any | ProtoTail], [Any | CleanProto]) :-
  sviz_remove_x(ProtoTail, CleanProto).

% sviz_proge(?ProgeTerm, ?SvizTerm)
% Prédicat bidirectionel de traduction pour les différents termes de Prolog et
% Sviz
sviz_proge(droite, line).
sviz_proge(dro, line).
sviz_proge(ccp, circle_center_point).
sviz_proge(rayon, circle_radius).
sviz_proge(dird, line_vector).
sviz_proge(diro, vector_perpendicular_line).
sviz_proge(interdd, inter_line_line).
sviz_proge(intercd, inter_circle_line).
sviz_proge(dpdir, line_point_vector).
sviz_proge(dir, vector).
sviz_proge(symp, point_symmetry).
sviz_proge(centre, circle_center).
sviz_proge(cdiam, circle_diameter).
sviz_proge(dist, distance).

% Cas général si le terme n'est pas spécifié
sviz_proge(Term, Term).

