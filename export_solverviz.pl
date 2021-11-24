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

% Autres cas
sviz_export_aux([Output := Cons | Tail], [SvizHead | SvizTail]) :-
  Cons =.. [ConsName | Arguments],
  sviz_generate_arguments(Arguments, Elements),
  sviz_proge(ConsName, SvizConsName),
  SvizHead = element(SvizConsName, [name=Output], Elements),
  % Appel récursif
  sviz_export_aux(Tail, SvizTail).

% Pour les instructions non traitées
sviz_export_aux([_ | Tail], SvizTail) :-
  sviz_export_aux(Tail, SvizTail).

% sviz_generate_arguments(+Arguments, -DOM)
% Génère une liste d'arguments

% Cas de base
sviz_generate_arguments([], []).
sviz_generate_arguments([ArgHead | ArgTail], [DOMHead | DOMTail]) :-
  sviz_generate_single_argument(ArgHead, DOMHead),
  sviz_generate_arguments(ArgTail, DOMTail).

% sviz_generate_single_argument(+Arg, -DOM)
% Génère une balise XML pour un argument donné
sviz_generate_single_argument(Arg, DOM) :-
  % On récupère le type
  fe(Arg, ProgeType, _, _, _, _),
  sviz_proge(ProgeType, SvizType),
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
  Object = element(point, [name=Name], [CoordX, CoordY]).

sviz_generate_random_object(Name, droite, Object) :-
  % Pour une droite: on génère 2 points aléatoires
  % Génération de noms pour les points générés
  atom_concat(Name, '_a', AName),
  atom_concat(Name, '_b', BName),
  sviz_generate_random_object(AName, point, A),
  sviz_generate_random_object(BName, point, B),
  Object = [A, B, element(line, [name=Name],
    [
      element(point, [value=AName], []),
      element(point, [value=BName], [])
    ])].

% sviz_proge(?ProgeTerm, ?SvizTerm)
% Prédicat bidirectionel de traduction pour les différents termes de Prolog et
% Sviz
sviz_proge(droite, line).
sviz_proge(dro, line).
sviz_proge(ccp, circle).

% Cas général si le terme n'est pas spécifié
sviz_proge(Term, Term).

