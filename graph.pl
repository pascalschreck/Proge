/*------------------------------*
*	graph			*
*   Utilitaires graphiques	*
* pour Proge			*
*-------------------------------*/

:- use(tb_object), use(tb_window), tbo_init.

origine(450,450).
factK(10).
coord_max(900,900).
coord_maxf(500,500).
coord_pos(200,200).

fen :-  coord_maxf(Xfm, Yfm),
	coord_pos(Xc, Yc),
	coord_max(Xm, Ym),
	tbw_create(_,tbw_canvas,[tbw_color,7,tbw_line_color,0,tbw_bm_height,Ym,tbw_bm_width,Xm,tbw_height, Yfm, tbw_width,Xfm,tbw_xw_in_bm, Xc, tbw_yw_in_bm,Yc,tbw_scroll_hor,true, tbw_scroll_vert, true,tbw_label, dessin,tbw_mask,[tb_all_evt]]).
	
coord_ec(X,Y,Xe,Ye) :- 
	number(X), number(Y), !,
	factK(K),
	origine(Xo,Yo),
	Xe is integer(X * K + Xo),
	Ye is integer(Yo - Y * K).
coord_ec(X,Y,Xe,Ye) :-
	number(Xe), number(Ye),
	factK(K),
	origine(Xo,Yo),
	X is integer((Xe - Xo)/K),
	Y is integer((Yo - Ye)/K).

point_bas(A,B,C,X,Y) :-
	coord_max(Xm,Ym),
	coord_ec(Xrmax,Yrmin,Xm,Ym),
	coord_ec(Xrmin,_,0,0),
	Xmin is -(B*Yrmin+C)/A,
	(
	Xmin >= Xrmin, Xmin =< Xrmax,
	!, coord_ec(Xmin,Yrmin,X,Y)
	;
	Xmin < Xrmin, !,
	Xr is Xrmin,
	Yr is -(A*Xrmin+C)/B,
	coord_ec(Xr, Yr, X, Y) 
	;
	Xmin > Xrmax,
	Xr is Xrmax,
	Yr is -(A*Xrmax+C)/B,
	coord_ec(Xr, Yr, X, Y)
	).
point_haut(A,B,C,X,Y) :-
        coord_max(Xm,Ym),
        coord_ec(Xrmax,_,Xm,Ym),
        coord_ec(Xrmin,Yrmax,0,0),
        Xmax is -(B*Yrmax+C)/A,
        (
        Xmax >= Xrmin, Xmax =< Xrmax,
        !, coord_ec(Xmax,Yrmax,X,Y)
        ;
        Xmax < Xrmin, !,
        Xr is Xrmin,
        Yr is -(A*Xrmin+C)/B,
	coord_ec(Xr, Yr, X, Y)
        ;
        Xmax > Xrmax,
        Xr is Xrmax,
        Yr is -(A*Xrmax+C)/B,
	coord_ec(Xr, Yr, X, Y)
        ).

