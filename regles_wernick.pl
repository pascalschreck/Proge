/*   Wernick */
multifile('#'/2).

301 # si [G '=p=' cgr(A, B, C)]
    et
      [connu A, connu G, pas_connu B]
    alors
      [
        Ma nomme mil(B,C),
        Ma '=p=' mil(symp(A, G), G) /* on peut ajouter : 2 */
      ].

/*302 # si [O '=p=' ccc(A, B, C)]
    et
      [connu A, connu B, connu C, pas_connu O]
    alors
      [
        MedA nomme med(B, C),
        MedB nomme med(A, C),
        O '=p=' interdd(MedA, MedB)
      ].
      
303 # si [H '=p=' ort(A, B, C)]
    et
      [connu A, connu B, connu C, pas_connu H]
    alors
      [
        Ha = dpp(dro(B, C), A),
        Hb = dpp(dro(A, C), B),
        H '=p=' interdd(Ha, Hb)
      ].*/