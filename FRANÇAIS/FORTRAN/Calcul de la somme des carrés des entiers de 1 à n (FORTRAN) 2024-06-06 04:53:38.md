**Calcul de la somme de carrés des entiers de 1 à n**

```fortran
PROGRAM SOMME_CARRES

  IMPLICIT NONE

  INTEGER, PARAMETER :: N = 1000000  ! Nombre jusqu'auquel calculer la somme des carrés

  INTEGER :: somme = 0  ! Variable pour stocker la somme des carrés

  DO i = 1, N
    somme = somme + i**2  ! Calcul du carré de chaque entier et ajout à la somme
  END DO

  PRINT *, "La somme des carrés des entiers de 1 à", N, "est :", somme

END PROGRAM SOMME_CARRES
```

**Explications du code :**

* **IMPLICIT NONE** indique qu'aucune variable implicite ne doit être utilisée. Cela garantit que toutes les variables sont déclarées explicitement.
* **INTEGER, PARAMETER :: N = 1000000** déclare une constante entière N avec la valeur 1 000 000.
* **INTEGER :: somme = 0** déclare une variable entière somme et l'initialise à 0.
* La boucle **DO** répète les instructions à l'intérieur de la boucle pour chaque valeur de i de 1 à N.
* Dans la boucle, **somme = somme + i**2 calcule le carré de l'entier courant (i) et l'ajoute à la somme courante.
* **PRINT *** affiche le résultat final, qui est la somme des carrés des entiers de 1 à N.