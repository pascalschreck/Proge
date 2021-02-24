/* pour Wernick */
:- multifile(profil/2).
:- multifile(equiv/2).

/* TODO Trouver un moyen de faire ces equivalents de triangles plus facilement*/

/*profil(ccc, point x point x point >> point) :- !.

profil(ort, point x point x point >> point) :- !.*/
% cgr(A, B, C) equiv cgr(A, C, B).
% cgr(A, B, C) equiv cgr(B, A, C).
% cgr(A, B, C) equiv cgr(B, C, A).
% cgr(A, B, C) equiv cgr(C, A, B).
% cgr(A, B, C) equiv cgr(C, B, A).

% ort(A, B, C) equiv ort(A, C, B).
% ort(A, B, C) equiv ort(B, A, C).
% ort(A, B, C) equiv ort(B, C, A).
% ort(A, B, C) equiv ort(C, A, B).
% ort(A, B, C) equiv ort(C, B, A).

% ccc(A, B, C) equiv ccc(A, C, B).
% ccc(A, B, C) equiv ccc(B, A, C).
% ccc(A, B, C) equiv ccc(B, C, A).
% ccc(A, B, C) equiv ccc(C, A, B).
% ccc(A, B, C) equiv ccc(C, B, A).