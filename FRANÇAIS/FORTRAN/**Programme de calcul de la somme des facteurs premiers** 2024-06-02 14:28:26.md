**Programme de calcul de la somme des facteurs premiers d'un nombre**

```fortran
PROGRAM SommeFacteursPremiers

IMPLICIT NONE

INTEGER :: n, i, sommeFacteursPremiers

PRINT *, "Entrez un nombre : "
READ *, n

sommeFacteursPremiers = 0

DO i = 2, n
  IF (n % i == 0) THEN
    DO WHILE (n % i == 0)
      sommeFacteursPremiers = sommeFacteursPremiers + i
      n = n / i
    END DO
  END IF
END DO

PRINT *, "La somme des facteurs premiers de ", n, " est ", sommeFacteursPremiers
END PROGRAM SommeFacteursPremiers
```

**Explication du code :**

* La ligne `IMPLICIT NONE` indique que toutes les variables doivent être explicitement déclarées.
* La ligne `PRINT *, "Entrez un nombre : "` affiche une invite à l'utilisateur pour entrer un nombre.
* La ligne `READ *, n` lit le nombre entré par l'utilisateur et le stocke dans la variable `n`.
* La ligne `sommeFacteursPremiers = 0` initialise la variable `sommeFacteursPremiers` à 0.
* La boucle `DO i = 2, n` itère sur les nombres de 2 à `n` pour rechercher les facteurs premiers.
* Dans la boucle, la condition `IF (n % i == 0)` vérifie si `i` est un facteur de `n`.
* Si `i` est un facteur de `n`, la boucle `DO WHILE (n % i == 0)` itère tant que `i` continue à diviser `n` de manière égale.
* À chaque itération de la boucle `DO WHILE`, `i` est ajouté à `sommeFacteursPremiers` et `n` est divisé par `i`.
* Après que `i` ait cessé d'être un facteur de `n`, la boucle `DO` continue avec la valeur suivante de `i`.
* Enfin, le programme affiche la somme des facteurs premiers de `n` à l'écran.