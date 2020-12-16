# Proge
(French version)

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

## Motions
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


