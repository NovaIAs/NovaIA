**Programme de résolution d'équations différentielles partielles elliptiques**

**Module 1 : Définition des paramètres**

```fortran
MODULE Parametres
  IMPLICIT NONE
  INTEGER, PARAMETER :: NX = 100     ! Nombre de points en x
  INTEGER, PARAMETER :: NY = 100     ! Nombre de points en y
  REAL, PARAMETER :: DX = 1.0       ! Pas en x
  REAL, PARAMETER :: DY = 1.0       ! Pas en y
  REAL, PARAMETER :: TOLERANCE = 1E-6 ! Tolérance pour la précision
END MODULE Parametres
```

**Module 2 : Déclaration des fonctions et procédures**

```fortran
MODULE Fonctions_Procedures
  USE Parametres
  IMPLICIT NONE

  ELEMENTAL FUNCTION F(x, y)
    ! Fonction à résoudre
    REAL, INTENT(IN) :: x, y
    F = SIN(x) * COS(y)
  END FUNCTION F

  SUBROUTINE Relaxation(u)
    ! Procédure de relaxation
    REAL, DIMENSION(NX, NY) :: u
    ! Boucle sur les points intérieurs
    DO i = 2, NX - 1
      DO j = 2, NY - 1
        u(i, j) = (1.0 / 4.0) * (u(i+1, j) + u(i-1, j) + u(i, j+1) + u(i, j-1) - DX**2 * F(i*DX, j*DY))
      END DO
    END DO
  END SUBROUTINE Relaxation
END MODULE Fonctions_Procedures
```

**Programme principal**

```fortran
PROGRAM Resolution_EDP
  USE Parametres
  USE Fonctions_Procedures
  IMPLICIT NONE

  ! Déclaration des variables
  REAL, DIMENSION(NX, NY) :: u    ! Solution de l'équation
  INTEGER :: iter = 0             ! Nombre d'itérations
  INTEGER :: i, j                ! Indices de boucle
  REAL :: erreur_max             ! Erreur maximale

  ! Initialisation de la solution
  DO i = 1, NX
    DO j = 1, NY
      u(i, j) = 0.0
    END DO
  END DO

  ! Itérations de relaxation
  DO
    erreur_max = 0.0
    CALL Relaxation(u)
    iter = iter + 1

    ! Calcul de l'erreur maximale
    DO i = 2, NX - 1
      DO j = 2, NY - 1
        erreur_max = MAX(erreur_max, ABS(u(i, j) - (1.0 / 4.0) * (u(i+1, j) + u(i-1, j) + u(i, j+1) + u(i, j-1) - DX**2 * F(i*DX, j*DY))))
      END DO
    END DO

    ! Affichage de l'erreur maximale
    PRINT *, "Itération ", iter, ": Erreur maximale =", erreur_max

  ! Condition d'arrêt
  EXIT WHEN (erreur_max < TOLERANCE)
  END DO

  ! Affichage de la solution
  OPEN(UNIT=10, FILE="solution.txt")
  DO i = 1, NX
    DO j = 1, NY
      WRITE(UNIT=10, *) u(i, j)
    END DO
  END DO
  CLOSE(UNIT=10)

END PROGRAM Resolution_EDP
```

**Explication du code**

Ce code résout une équation différentielle partielle elliptique à l'aide de la méthode de relaxation. Il est divisé en trois modules :

* **Module 1 : Définition des paramètres** : Définit les paramètres du problème, tels que le nombre de points, le pas et la tolérance pour la précision.
* **Module 2 : Déclaration des fonctions et procédures** : Déclare la fonction à résoudre et la procédure de relaxation.
* **Programme principal** : Implémente le programme principal qui initialise la solution, effectue les itérations de relaxation et affiche les résultats.

Le code utilise la méthode de relaxation pour résoudre l'équation différentielle partielle. La méthode de relaxation est une méthode itérative qui calcule la solution en mettant à jour chaque point de la grille à l'aide des valeurs des points voisins.

Le programme s'arrête lorsque l'erreur maximale entre deux itérations consécutives est inférieure à la tolérance spécifiée.

La solution est écrite dans un fichier texte pour une analyse ultérieure.