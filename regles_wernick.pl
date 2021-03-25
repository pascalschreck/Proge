/*   Wernick */
multifile('#'/2).

/* 301 - 399 = Centre de gravité */
/* 400 - 499 = Centre du cercle circonscrit */
/* 500 - 599 = Orthocentre */

/* Centre de gravité */

/* Si on connait le centre de gravité et un milieu, on peut trouver le côté opposé */
301 # si [G '=p=' cgr(A, B, C)]
    et
    
      [connu A, connu G, pas_connu B]
    alors
      [
        Ma nomme mil(B,C),
        Ma '=p=' mil(symp(A, G), G) /* on peut ajouter : 2 */
      ].
      
/* Centre du cercle circonscrit */

/* Construction du centre du cercle circonscrit */
400 # si [O '=p=' ccc(A, B, C)]
    et
      [connu A, connu B, connu C, pas_connu O]
    alors
      [
        MedA nomme med(B, C),
        MedB nomme med(A, C),
        O '=p=' interdd(MedA, MedB)
      ].
      
401 # si [O '=p=' ccc(A, B, C)]
  et [connu O, connu A, pas_connu(B)] /* indépendant du statut de C  */
  alors
    [
      CCC nomme ccp(O, A),
      B est_sur CCC : 1
    ].
      
/* Orthocentre */
      
/* Construction de l'orthocentre */

500 # si [H '=p=' ort(A, B, C)]
    et
      [connu A, connu B, connu C, pas_connu H]
    alors
      [
        Ha nomme dorth(dro(B, C), A),
        Hb nomme dorth(dro(A, C), B),
        H '=p=' interdd(Ha, Hb)
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
    
503 # si [H '=p=' ort(A, B, C), O '=p=' ccc(A, B, C)]
  et
    [connu A, connu O]
  alors
    [
      D nomme BC,
      Hp nomme symd(D, H),
      Hp est_sur ccp(O, A)
    ].
    
504 # si [H '=p=' ort(A, B, C), Ha '=p=' prj(H, dro(B, C))]
  et
    [connu H, connu A]
  alors
    [
      Ha est_sur dro(A, H) : 1
    ].
    
505 # si [H '=p=' ort(A, B, C), Hp '=p=' symd(dro(A, B), H)]
  et
    [connu H, connu A, pas_connu Hp]
  alors
    [
      Hp est_sur dro(A, H) : 1
    ].