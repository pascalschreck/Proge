/*---------------------------------*
*        REGLES.PRO                *
*  Regles pour Proge               *
*----------------------------------*/


1 # si  [ dist(A,B) '=l=' K ]
       et
        [connu A, connu K, pas_connu B]
    alors
        [ B est_sur ccr(A,K) : 1,
          A est_sur ccr(B,K)        /* ajout PS2012, pas définissant   */
        ].

2 # si [ did(A,D) '=l=' H]
      et
       [connu D, connu H, pas_connu A]
    alors
      [ A est_sur dpd(D,H) : 1].
      
3 # si [ did(A, dro(B,C)) '=l=' L]
       et
       [connu A, connu B, connu L, differents [B, prj(A, dro(B,C))], pas_connu C]
    alors
       [
        H nomme prj(A, dro(B,C)),   /* probleme si H est deja connu ... */
        H est_sur cdiam(A,B),
        H est_sur ccr(A,L) : 1
       ].
33 # si [ H '=p=' prj(A, dro(B,C))]
      et
	[connu A, connu H,  pas_connu B, pas_connu C]
     alors
	[ 
	  D nomme dorth(dro(A,H), H),
	  D '=d=' dro(B,C)
	 ].

 5 # si [ D ortho Dp ]
      et
       [ connu D,  pas_connu Dp]
    alors
       [
        Di nomme diro(D),
        Di '=di=' dird(Dp): 1
       ].

/* 
5 # si [ D ortho dro(A, B) ]
      et
       [ connu D, connu A, pas_connu B]
    alors
       [
	Di nomme diro(D),
	B est_sur dpdir(A,Di) : 1
       ].
*/
4 # si [ dro(A,B) ortho dro(A,C)]
      et
       [connu B, connu C, pas_connu A]
    alors
       [ A est_sur cdiam(B,C) : 1].



6 # si [ dist(A,M) '=l=' dist(B,M) ]
      et
       [ differents [A,B], connu med(A,B), pas_connu M]
    alors
       [ M est_sur med(A,B) : 1
       ].

7 # si  [did(A,D1) '=l=' did(A,D2)]
      et
	[differents [D1,D2], connu D1, connu D2, pas_connu A]
     alors
	soit [dird(D1) diff dird(D2)] et [ A est_sur bis(D1,D2) : 1]
       ou
        soit [dird(D1) eg dird(D2), D1 diff D2] et [A est_sur dmd(D1,D2) : 1]
       ou
        soit [D1 eg D2] et [].   /* D1 = D2 conclusion en soit sufisante ? */


8 # si  [ I '=p=' mil(A,B)]
      et
	[ connu I, connu A, pas_connu B]
    alors
	[ B '=p='symp(I,A) : 2 ].

9 # si	[ I '=p=' mil(A,B), dist(A,B) '=l=' K]
      et
	[ connu I, connu K, pas_connu A, pas_connu B]
      alors
	[ A est_sur ccr(I,K/2) : 1, B est_sur ccr(I,K/2) : 1].

20 # si [ tangentcd(ccr(O,R),D,A)]
     et
	[connu R, connu D, connu A, pas_connu O]
    alors
	[
	 O est_sur dorth(D,A) : 1,
	 O est_sur dpd(D,R)
	].

21 # si	[ tangentcd(ccr(O,R),D,A)]
       et
	[connu R, connu D, pas_connu O, pas_connu A]
      alors
	[ O est_sur dpd(D,R) ].

22 # si [ tangentcd(ccr(O,R),D,A)]
     et
	[connu D, connu A, pas_connu O, pas_connu R]
    alors
	[
/*	 Dc nomme dro(O,A),
	 Dc ortho D : 1 
  ancienne conclusion : */
	O est_sur dorth(D,A)
	].
 

23 # si [  tangentcc(ccr(O,R), ccr(Op,Rp),_)]
      et
	[ connu Op, connu Rp, connu R, pas_connu O]
     alors
	[ O est_sur ccr(Op, R+Rp) : 1 ].

  
24 # si [  tangentcc(ccr(O,R), ccr(Op,Rp),_)]
	et
	[]
     alors
	[dist(O,Op) '=l=' R + Rp].

100 # si [dist(A,B) '=l=' dist(A,C)]
        et
         [differents [A,B,C] ]
      alors
         [iso(A,B,C)].
         

102 # si [ iso(A,B,C) ]
       et
       []
    alors
       [dist(A,B) '=l=' dist(A,C)].
        
105 # si [ iso(A,B,C) ]
       et
        [pas_connu B]
      alors
        [
         I nomme mil(B,C),
	 dro(A,I) '=d=' med(B,C),
         dro(A,I) ortho dro(B,C)
        ].

110 # si [M est_sur ccr(O,R)]
        et []
      alors
        [ dist(O,M) '=l=' R].

120 # si [tangentcc(C,D,A)]
        et
	[]
       alors
	[ A est_sur C, A est_sur D].

121 # si [tangentcd(C,D,A)]
        et
	[]
       alors
	[ A est_sur C, A est_sur D].

122 # si [tangentdc(C,D,A)]
        et
	[]
       alors
	[ A est_sur C, A est_sur D].

123 # si [tangentcd(ccr(O,R),D,_) ]
	et
	 []
      alors
	 [
/*	  R '=l=' dist(O,A),	*/ /* normalement, c'est deja trouve */
	  R '=l=' did(O,D)
	 ].


 124 # si [tangentcc(ccr(O1,R1),ccr(O2,R2),A)]
        et
	[]
      alors
	[
	  D1 nomme dro(O1,O2),
	  A est_sur D1,
	  D nomme dorth(D1,A),
	  tangentcd(ccr(O1,R1),D,A),
	  tangentcd(ccr(O2,R2),D,A)
	 ,
	  tinhibe(22)
	].

125 # si [tangentcc(ccr(O1,R1),ccr(O2,R2),A), B est_sur ccr(O1,R1)]
	et
	 [ connu B, connu O2, connu R2, pas_connu A, connu dro(B,O1) ]
      alors
	[ BB nomme intercd(ccr(O2,R2),dpp(dro(B,O1),O2)),
	  A '=p=' interdc(dro(B,BB), ccr(O2,R2)) : 2
	].

200 # si [dist(A,B) '=l=' dist(C,D), dird(dro(A,B)) '=di=' dird(dro(C,D))]
	et
	 [differents [A,B,C,D]]
      alors
	soit [] et [pll(A, B, C, D), tinhibe(200)]
       ou
	soit [] et [pll(A, B, D, C)].
        
202 # si [pll(A,B,C,D)]
	et
	 []
      alors
	[mil(A,C) '=p=' mil(B,D)].

203 # si [pll(A,B,C,D)]
	et
	 []
      alors
	 [dird(dro(A,B)) '=di=' dird(dro(C,D)),
	  dird(dro(A,D)) '=di=' dird(dro(B,C))].

	

300 # si [dist(A,M) + dist(B,N) '=l=' L]
    et
       [connu dro(A, M) ]
    alors
    [
    	X nomme interdc(dro(A,M), ccr(A,L)),
        A est_sur ccr(X,L),
    	dist(X,M) '=l=' dist(B,N),
    	tinhibe(300)
    ].
 
 	
  

