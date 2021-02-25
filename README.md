# Proge
(French version)

## Présentation
Proge, ou Progé pour Prolog et constructions Géométriques, est un programme Prolog pour résoudre symboliquement des probèmes de construction du style "à la règle et au compas". Cela correspnd à une approche IA symbolique sans apprentissage qu'il soit profond ou non, où une base de connaissance est mise en oeuvre. Cette base de connaissances peut comporter des règles en dehors des constructions à la règles et au compas (par exemple, couper un angle en trois, extraire une racine cubique, etc.) tant qu'on sait exprimer logiquement ces opérations et que cela correspond à un modèle calculable. 

Comme tout programme Prolog, Proge est consiste en une description symbolique de différentes notions géométriques qu'on répartit classiquement en types (ou sortes), fonctions et prédicats dans une signature qui est explicitement décrite. Une base de règles est également implantée qui traduit des théorèmes classiques de la géométrie euclidienne et qu'on utilise traditionnellement dans les constructions géométriques.

liste des fichiers utilsés :
- cog.pl
- crg.pl
- except.pl
- figure.pl
- graph.pl
- init.pl
- inter.pl
- moteur.pl
- mregle.pl
- og.pl
- proge.pl
- raison.pl
- regles.pl
- test.pl
- type.pl
- util.pl


## Utilisation
On écrit un énoncé en Prolog (en utilisant les opérateurs syntaxique qui définissent un langage)
On indique dans le fichier proge.pl le répertoire (à partir du répertoire où est le programme Prolog)
par exemple 
repertoire('Wernick/').
à la ligne 43 pour le moment, juste après la définition syntaxique des opérateurs
Puis, on lance le programme avec 
$ ./run
----> les fichiers se chargent
----> puis le programme demande le nom du fichier (sans le suffixe '.pl')
par exemple
nom de l'exercice : wp3.
( ne pas oublier le point qui termine la saisie)
après différents messages (qui sont souvent echec, mais ça ne veut pas dire que rien n'est trouvé)
on a le prompt 'r:'
avec h on a les différentes commandes possibles ... presque plus rien ne fonction :( sauf afficher le programme, en tapant a.
Avec notre exemple :
```
r: |: h

 Aide : 
d        : dessine 
s     : solution suivante
p        : solution precedente
c        : changer un objet de base
i        : nouvelle interpretation (complete)
x        : coordonnees
e        : shell
p        : sauver (temporairement) un programme
r        : recuperer un programme
a        : afficher le programme
k        : kill programmes sauves
w        : modifier la fenetre
f,q      : quitter
 r: a

a:=(donne)
b:=(donne)
g:=(donne)
long00:=dist(g,a)
si [g diff a] alors
   droite01:=dro(g,a)
   dir01:=dird(droite01)
   long01:=dist(g,b)
   si [g diff b] alors
      droite03:=dro(g,b)
      dir03:=dird(droite03)
      long02:=dist(a,b)
      si [a diff b] alors
         point02:=mil(a,b)
         droite04:=dro(b,a)
         dir04:=dird(droite04)
         long03:=dist(g,point02)
         si [g diff point02] alors
            droite05:=dro(g,point02)
            dir05:=dird(droite05)
            point03:=symp(a,g)
            long04:=dist(point03,a)
            si [point03 diff a] alors
               point00:=mil(point03,a)
               long05:=dist(point00,b)
               si [point00 diff b] alors
                  droite00:=dro(point00,b)
                  dir00:=dird(droite00)
                  c:=interdd(droite00,droite05)
                  fin
                sinon
                  point04:=symp(point00,g)
                  long06:=dist(point04,point00)
                  si [point04 diff point00] alors
                     point01:=mil(point04,point00)
                     long07:=dist(point01,a)
                     si [point01 diff a] alors
                        droite02:=dro(point01,a)
                        dir02:=dird(droite02)
                        c:=symp(point01,a)
                        fin
                      sinon
                        echec
                        fin
                     fin
                   sinon
                     echec
                     fin
                  fin
               fin
             sinon
               echec
               fin
            fin
          sinon
            echec
            fin
         fin
       sinon
         echec
         fin
      fin
    sinon
      echec
      fin
   fin
 sinon
   echec
   fin
verifier(g=p=cg(a,b,c))
fin
 r: 
```
## Notions
Relatives à la signature :
* types --> type.pl
* symboles fonctionnels --> cog.pl (constructeurs d'objets géométriques)
* symboles pérdicatifs --> crg.pl (constructeurs de relations géométriques)
* objets géométriques --> og.pl (objet géométrique et ses relations avec les autres)
* théorèmes connus\règles
* raisonnement et figure --> raison.pl et figure.pl
* énoncé  --> init.pl
* moteur dinférence --> moteur.pl
* règles --> mregle.pl
* programme de construction --> diffus, mais on l'a dans figure.pl et dans inter.pl
* évaluation d'un programme --> inter.pl (la partie calcule numérique est absente ! je ne sais pas où elle est)


