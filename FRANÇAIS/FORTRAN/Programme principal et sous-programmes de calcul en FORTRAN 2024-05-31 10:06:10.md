**Programme Principal**

```fortran
PROGRAMME PRINCIPAL

    IMPLICIT NONE

    INTEGER, PARAMETER :: NB_ITER = 1000000

    REAL(KIND=8) :: X, Y, Z

    CALL INITIALISATION(X, Y, Z)

    DO I = 1, NB_ITER
        CALL CALCUL(X, Y, Z)
    END DO

    CALL AFFICHAGE_RESULTATS(X, Y, Z)

END PROGRAM PRINCIPAL
```

**Sous-programme d'Initialisation**

```fortran
SUBROUTINE INITIALISATION(X, Y, Z)

    IMPLICIT NONE

    REAL(KIND=8), INTENT(OUT) :: X, Y, Z

    X = 0.0D0
    Y = 1.0D0
    Z = 2.0D0

END SUBROUTINE INITIALISATION
```

**Sous-programme de Calcul**

```fortran
SUBROUTINE CALCUL(X, Y, Z)

    IMPLICIT NONE

    REAL(KIND=8), INTENT(INOUT) :: X, Y, Z

    X = X + Y
    Y = Y + Z
    Z = Z + X

END SUBROUTINE CALCUL
```

**Sous-programme d'Affichage des Résultats**

```fortran
SUBROUTINE AFFICHAGE_RESULTATS(X, Y, Z)

    IMPLICIT NONE

    REAL(KIND=8), INTENT(IN) :: X, Y, Z

    PRINT *, "X =", X
    PRINT *, "Y =", Y
    PRINT *, "Z =", Z

END SUBROUTINE AFFICHAGE_RESULTATS
```

**Explication du Code**

Ce code est un programme FORTRAN qui effectue des calculs répétitifs sur les variables X, Y et Z. Il est complexe car il contient plusieurs niveaux de sous-programmes et utilise des types de données doubles précision.

Le programme principal initialise les variables, exécute une boucle avec NB_ITER itérations, et affiche les résultats. La boucle appelle un sous-programme de calcul qui incrémente les variables X, Y et Z.