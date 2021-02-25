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
Je ne sais pas très bien à quoi peuvent servir les conditions dans le si pour le meoment, c'est juste que la syntaxe était déjà prête.

Les termes 'cnd:' étaient auparavant rangés dans les contraintes. Je crois que ça apporte en clarté de les mettre à part.

le traitment en lui-même de ces nouveaux termes ne présnetaient pas de problème. En revanche, mon petit prouveur était bourré de problème et ne fonctionnait finalement quasiment pas :(

** TODO
finir d'implanter ce prouveur et les règles qu'il utilise. On pourrait d'ailleurs écrire un système de règles plus lisible et plus facile à écrire pour un non programmeur.
Par ailleurs, les règles pourraient presque étre dérives des constructeurs d'objets.
