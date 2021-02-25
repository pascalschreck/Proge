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
On peut trapper éventuellement l'absence de si (ça n'est pas dur à faire)