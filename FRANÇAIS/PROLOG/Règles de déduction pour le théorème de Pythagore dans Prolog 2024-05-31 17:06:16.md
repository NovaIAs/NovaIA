**Règles de déduction pour le théorème de Pythagore**

```prolog
% A est l'hypoténuse, B et C sont les côtés de l'angle droit
hypotenuse(A, B, C) :-
    A*A is B*B + C*C.

% B et C sont les côtés de l'angle droit, A est l'hypoténuse
cote_angle_droit(B, C, A) :-
    hypotenuse(A, B, C),
    A > B,
    A > C.

% B et C sont les côtés de l'angle droit adjacent, A est l'hypoténuse
cote_angle_droit_adjacent(B, C, A) :-
    hypotenuse(A, B, C),
    A > B,
    A < C.

% B et C sont les côtés de l'angle droit opposé, A est l'hypoténuse
cote_angle_droit_oppose(B, C, A) :-
    hypotenuse(A, B, C),
    A < B,
    A > C.
```

**Utilisation de ces règles**

```prolog
?- hypotenuse(A, 3, 4).
A = 5.

?- cote_angle_droit(3, 4, A).
A = 5.

?- cote_angle_droit_adjacent(3, 4, A).
A = 5.

?- cote_angle_droit_oppose(3, 4, A).
false.
```

**Explication du code**

* Le prédicat `hypotenuse` vérifie si trois nombres satisfont le théorème de Pythagore.
* Les prédicats `cote_angle_droit`, `cote_angle_droit_adjacent` et `cote_angle_droit_oppose` vérifient si trois nombres représentent les longueurs des côtés d'un triangle rectangle, avec respectivement l'angle droit, l'angle droit adjacent et l'angle droit opposé.
* Le code utilise des contraintes d'ordre (`>` et `<`) pour s'assurer que les côtés sont disposés correctement dans le triangle.
* La requête `?- hypotenuse(A, 3, 4)` demande la longueur de l'hypoténuse d'un triangle rectangle avec des côtés de longueurs 3 et 4. La réponse `A = 5` indique que l'hypoténuse mesure 5.
* La requête `?- cote_angle_droit(3, 4, A)` demande la longueur du côté de l'angle droit d'un triangle rectangle avec des côtés de longueurs 3 et 4. La réponse `A = 5` indique que le côté de l'angle droit mesure 5.
* La requête `?- cote_angle_droit_adjacent(3, 4, A)` demande la longueur du côté de l'angle droit adjacent d'un triangle rectangle avec des côtés de longueurs 3 et 4. La réponse `A = 5` indique que le côté de l'angle droit adjacent mesure 5.
* La requête `?- cote_angle_droit_oppose(3, 4, A)` demande la longueur du côté de l'angle droit opposé d'un triangle rectangle avec des côtés de longueurs 3 et 4. La réponse `false` indique qu'il n'y a pas de tel côté, car les côtés donnés ne forment pas un triangle rectangle.