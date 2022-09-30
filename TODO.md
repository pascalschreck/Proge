## In progress
### Prise en compte de conditions additionnelles dans l'énoncé
Pour éviter d'avoir un programme de construction plus compliqué que nécessaire, on ajoute des conditons additionnels rendant explicite les conditions du cas général (ou plus exactement, du cas qui nous intéresse).

On définit un nouvel opérateur 'cnd:' (condition de non dégénérescence) pour introduire ces conditions supplémentaires. Pour le moment on utilise le format utilisé pour les exceptions qui sont plus précisément des termes de la forme :
```
    X diff Y si <liste de conditions>
```
Par exemple, si on veut déclarer que deux points a et b sont différents, on écrira
```
    'cnd:' a diff b si []
```
Qu'on peut abbréger en 
```
    'cnd:' a diff b
```
Je ne sais pas très bien à quoi peuvent servir les conditions dans le si pour le moment, c'est juste que la syntaxe était déjà prête.

Les termes 'cnd:' étaient auparavant rangés dans les contraintes. Je crois que ça apporte en clarté de les mettre à part.

le traitement en lui-même de ces nouveaux termes ne présentait pas de problèmes. En revanche, mon petit prouveur était lui, bourré de problèmes et ne fonctionnait finalement quasiment pas :(

### Nettoyage de programme après coup
Faire un tri topologique en partant des objets cherchés ou mentionnés (et pour les objets donnés ? que fait-on s'ils ne servent à rien ?)
On part des définitions des objets cherchés ou mentionnés qu'on a prédemment marqués et on marque tous les objets qui interviennent dans leur définition. Et on recommence.
C'est un procédé classique en compilation où on détecte les variables inutilisées en construisant un graphe de dépendances. 

C'est ce qui a été fait : l'hypergraphe de dépendance est simplement la liste de toutes les définitions, la liste des sommets sont les objets géométriques de la figure. 

Un programme de construction est une liste d'instructions 
* qui est affichée avec le prédicat aff_prog/2 (aff_prog(Liste, Niveau)), les deux arguments doivent être instanciés, le Niveau sert de décalage pour faire un affichage indenté
* qui est "sauvé" sous forme d'un fait de foncteur le prédicat sauv/2, le premier argument (sans doute) un numéro généré par gname
* qui est construit avec pcg_bdf_l/2. Ce prédicat est définit dans le fichier inter.pl (inter = interprétation). 
Pour le moment, c'est dans le prédicat roule/0 qu'est fait ce travail après l'appel à moteur/1 (qui est le moteur d'inférence)

pcg_bdf_l/2 utilise  :
* pcf_gdf_l/2 : fait l'assemble de deux listes, le pgm proprement dit et les instructions de vérification ajoutées à la fin du programme ;
* pcg_bdf_l1/2 : c'est ce prédicat qui fait le gros du travail, il retire de la base de fait (dans l'ordre) les instructions avec le bon numéro de programme (avec retract(pcg(Num, Term)) ), puis traite les instrucions (en les rangeant dans la liste si elles sont simples ou en les complétant si ce sont des conditionnelles ou des boucles) ;
* pcg_bdf_l2/2 : ce prédicat ne fait que constituer la liste des instrucions de vérification qui sera ajoutée (dans le prédicat pcf_bdf_l) à la liste construite par pcf_bdf_l1 ;

Le nettoyage peut se faire après l'appel à pcg_bdf_l/2 (ou être inclus dans pcf_bdf_l si on veut traiter des spécificités par branche, mais alors on ne remonte pas à la racine du programme).
## TODO
### Liste de Wernick
Raphaël a déjà commencer à travailler dessus. Je pense cependant qu'il faut reprendre l'univers géométrique. Je vais esquisser la travail ici :
1. la liste de Wernick ne considère que des points (dans un triangle), on a déjà les points ... faut-il ajouter la notion de triangle ?
2. Points caractéristiques :
    - les sommets et le centre du cercle circonscrit : le centre du cercle circonscrit a été introduit
    - ccc/3 et il n'y a rien à faire pour les sommets.
    - les milieux des cotés et l'isobarycentre
    - les pieds des hauteurs et l'orthocentre
    - les pieds des bissectrices intérieures et le centre du cercle circonscrit
3. Nouveaux constructeurs
    - de points 
        - on a déjà milieu, il faudrait couper en 3 ... ou plus généralement, barycentre ?
        - pieds (hauteurs, bissectrices) : on met 1, 2 ou ... constructeurs
        - centres : centre cercle circonscrit, isobarycentre, orthocentre, centre cercle inscrit
    - de droites
        - médianes (?), hauteurs, médiatrices (je crois que ça y est déjà), bissectrice intérieure
    - de cercles
        - cercle circonscrit, cercle d'euler
4. relations : à lister + voir les propriétés importantes sur wikipédia
5. Mettre au point les règles et le reste en faisant des essais avec la liste de Wernick


### Sortie Geogebra
Raphaël a fait une sortie dans Geogebra, puis une autre dans un langage un peu plus portable. Le fichier export_solverviz.pl traite de ce nouveau format de sortie (je ne sais pas bien comment est ensuite faite la traduction en geogebra, avec un programme Python je crois). Ce deuxième fichier est beucoup mieux fait que le premier (export.pl) car il prend plus en compte l'univers géométrique. On pourrait même aller un peut plus loin et inclure le code xml/geogebra dans la description de l'UG (COG et type). ... à voir


### Prouveur pour les cas généraux et pour les cas dégénérés
finir d'implanter ce prouveur et les règles qu'il utilise. On pourrait d'ailleurs écrire un système de règles plus lisible et plus facile à écrire pour un non programmeur.
Par ailleurs, les règles pourraient presque étre dérivées des constructeurs d'objets.

Remarque : par rapport aux prouveurs usuels, on se soucie aussi de montrer que des objets sont différents. Pour cela, on se fonde 
* sur des déclarations explicites de différences (par exemple dans l'énoncé ou parce qu'on est dans une branche du programme (si_alors_sinon_) dans lequelle des différences ont été supposées) 
* sur des heuristiques qu'on devrait pouvoir inhiber : par exemple, si on déclare que la distance entre deux points est k, on dit impliciement que k est non nul et que le spoints sont donc différents (peut être à revoir)

### Entrées-sorties et batchmode
Permettre d'écrire les énoncés avec leurs pgm de construction dans un fichier

Dans le mini-interpréteur de commande écrire la commande n pour new qui permette de charger un nouvel énoncé et de lancer sa résolution.

faire un mod batch qui traite tout un répertoire ou une liste de fichier écrite dans fichier texte.

Revoir tous les interpéteurs de commandes ...