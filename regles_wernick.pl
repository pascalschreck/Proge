/*   Wernick */
multifile('#'/2).

/* 301 - 399 = Centre de gravité */
/* 400 - 499 = Centre du cercle circonscrit */
/* 500 - 599 = Orthocentre */

/* Centre de gravité */

300 # si [G est_sur dro(A,mil(B,C)), G est_sur dro(B,mil(A,C))]
  et
    [ connu A, connu B, connu C,  pas_def(G, gcr(A,B,C))]           /*    PS mars 21: ajout de pas_def *//* moteur complété pour ce terme */
  alors
    [
      G '=p=' gcr(A,B,C),                     /* mars 2021 (PS) : il me semble que nomme ne met pas à jour le 
                                                raisonnement, seulement la figure (voir moteur.pl               */
      G est_sur dro(C,mil(B,C))
    ].

/* Si on connait le centre de gravité et un milieu, on peut trouver le côté opposé */
301 # si [G '=p=' cgr(A, B, C)]
    et
    
      [connu A, connu G]
    alors
      [
        Ma nomme mil(B,C),
        Ma '=p=' mil(symp(G, A), G) : 2 /* on peut ajouter : 2 */
      ].
      
/* Centre du cercle circonscrit */

/* Construction du centre du cercle circonscrit */
/* La règle suivante commentée, n'est pas correcte */
/*
400 # si [O '=p=' ccc(A, B, C)]
    et
      [connu A, connu B, connu C, pas_connu O]
    alors
      [
        MedA nomme med(B, C),
        MedB nomme med(A, C),
        O '=p=' interdd(MedA, MedB)
      ].
*/    
400 # si [dist(O,B) '=l=' dist(O,C), dist(O,A) '=l=' dist(O,C)]
et
  [connu A, connu B, connu C, differents [A,B,C], pas_connu O]
alors
  [
    MedA nomme med(B, C),
    MedB nomme med(A, C),
    MedC nomme med(A,B),
    O '=p=' ccc(A,B,C),               /* mars 2021 (PS) : il me semble que nomme ne met pas à jour le raisonnement, seulement la figure */
    O '=p=' interdd(MedA, MedB),      /* alors que '=p=' le fait et permet de réutiliser ccc dans une règle. */
    O est_sur MedC
  ].

410 # si [O '=p=' ccc(A, B, _)]
  et [connu O, connu A, pas_connu B ] /* indépendant du statut de C  */
  alors
    [
      CC nomme ccp(O, A),
      B est_sur CC : 1
    ].
      
/* Orthocentre */
      
/* Construction de l'orthocentre */
/* même chose */
/*
500 # si [H '=p=' ort(A, B, C)]
    et
      [connu A, connu B, connu C, pas_connu H]
    alors
      [
        Ha nomme dorth(dro(B, C), A),
        Hb nomme dorth(dro(A, C), B),
        H '=p=' interdd(Ha, Hb)
      ].
 */
500 # si [ortho(dro(A,H),dro(B,C)), ortho(dro(B,H),dro(A,C))]
et
  [connu A, connu B, connu C, pas_connu H]
alors
  [
    Ha nomme dorth(dro(B, C), A),
    Hb nomme dorth(dro(A, C), B),
    Hc nomme dorth(dro(A,B), C),
    H '=p=' ort(A,B,C),                 /* même chose que plus haut : différence entre 'nomme' et '=p='  */
    H '=p=' interdd(Ha, Hb),
    H est_sur Hc                        /* théorème sur H ... redondant avec 501 ?*/
  ].

501 # si [H '=p=' ort(A, B, C)]
    et
      [connu A, connu B, connu H, pas_connu C]
    alors
      [
        C est_sur dorth(dro(A, B), H) : 1
      ].
      
502 # si [H '=p=' ort(A, B, C)]
  et
    [connu A, connu B, connu H, pas_connu C]
  alors
    [
      HA nomme dro(H, A),
      C est_sur dorth(HA, B) : 1
    ].

/*   
503 # si [H '=p=' ort(A, B, C), O '=p=' ccc(A, B, C)]
  et
    [connu A, connu O]
  alors
    [
      D nomme dro(B,C),
      Hp nomme symd(D, H),
      Hp est_sur ccp(O, A)
    ].
*/

504 # si [H '=p=' ort(A, B, C), Ha '=p=' prj(H, dro(B, C))]
  et
    [connu H, connu A]
  alors
    [
      Ha est_sur dro(A, H) : 1
    ].
    
505 # si [H '=p=' ort(A, B, _), Hp '=p=' symd(dro(A, B), H)]
  et
    [connu H, connu A, pas_connu Hp]
  alors
    [
      Hp est_sur dro(A, H) : 1
    ].

  
506 # si [H '=p=' ort(A, B, C), O '=p=' ccc(A, B, C)]
et
 [connu H, connu O]
alors
 [
 G nomme cgr(A, B, C),
 G '=p=' eulerg(H,O)
 ].
 
507 # si [G '=p=' cgr(A, B, C), O '=p=' ccc(A, B, C)]
et
  [connu G, connu O]
alors
  [
    H nomme ort(A, B, C),
    H '=p=' eulerh(G, O)
  ].

508 # si [H '=p=' ort(A, B, C), G '=p=' cgr(A, B, C)]
et
  [connu H, connu G]
  alors
  [
    O nomme ccc(A, B, C),
    O '=p=' eulero(H, G)
  ].

/* problème avec cette règle :
   avec les définitions actuelles,
   le point Hp est reconnu comme le point A
*/
/*
507 # si [H '=p=' ort(A, B, C), O '=p=' ccc(A, B, C)]
  et
    [connu A, connu H, connu O, pas_connu B]
  alors
    [
      Hp nomme intercd(ccp(O,A),dro(A,H)),
      B est_sur med(H,Hp) : 1
    ].
  */