```fortran
MODULE CalculMatrice
    Implicit none
    SAVE
    PRIVATE
    PUBLIC :: MatriceInverse

CONTAINS

!-------------------------------------------------------------------------------
! Inversion d'une matrice carrée
!-------------------------------------------------------------------------------
SUBROUTINE MatriceInverse(A, n, Inverse)
    Implicit none

! Déclaration des variables
    INTEGER, INTENT(in) :: n       ! Ordre de la matrice
    REAL(kind=8), INTENT(in), DIMENSION(n,n) :: A      ! Matrice à inverser
    REAL(kind=8), INTENT(out), DIMENSION(n,n) :: Inverse ! Matrice inverse

! Vérification de la dimension de la matrice
    IF (n <= 0) THEN
        PRINT *, "Erreur : La matrice doit être carrée d'ordre positif"
        STOP
    END IF

! Création d'une matrice identité de dimension n
    REAL(kind=8), DIMENSION(n,n) :: I = 0.0D0
    DO i = 1, n
        I(i,i) = 1.0D0
    END DO

! Inversion de la matrice en utilisant la décomposition LU
    INTEGER :: pivots(n)
    INTEGER :: info
    CALL GETRF(n, n, A, n, pivots, info)
    IF (info /= 0) THEN
        PRINT *, "Erreur : La matrice est singulière (non inversible)"
        STOP
    END IF
    CALL GETRI(n, A, n, pivots, I, info)
    IF (info /= 0) THEN
        PRINT *, "Erreur : Echec de l'inversion de la matrice"
        STOP
    END IF

! Affectation de la matrice inverse
    Inverse = I

END SUBROUTINE MatriceInverse
END MODULE CalculMatrice

PROGRAM TestMatriceInverse
    Implicit none

! Déclaration des variables
    INTEGER :: n = 3                ! Ordre de la matrice
    REAL(kind=8), DIMENSION(n,n) :: A = 0.0D0     ! Matrice à inverser
    REAL(kind=8), DIMENSION(n,n) :: Inverse        ! Matrice inverse

! Initialisation de la matrice A
    A(1,1) = 2.0D0
    A(1,2) = 3.0D0
    A(1,3) = 4.0D0
    A(2,1) = 5.0D0
    A(2,2) = 6.0D0
    A(2,3) = 7.0D0
    A(3,1) = 8.0D0
    A(3,2) = 9.0D0
    A(3,3) = 10.0D0

! Calcul de la matrice inverse
    CALL CalculMatrice%MatriceInverse(A, n, Inverse)

! Affichage de la matrice A et de sa matrice inverse
    PRINT *, "Matrice A :"
    PRINT *, A
    PRINT *, "Matrice Inverse :"
    PRINT *, Inverse

END PROGRAM TestMatriceInverse
```

**Explication du code**

Ce code Fortran implémente une inversion matricielle à l'aide de la décomposition LU. Il contient les éléments suivants :

* **Module `CalculMatrice`** : Ce module contient la sous-routine `MatriceInverse` qui effectue l'inversion de la matrice.
* **Sous-routine `MatriceInverse`** : Cette sous-routine prend une matrice carrée `A` et son ordre `n` en entrée, et renvoie la matrice inverse `Inverse` en sortie.
* **Vérification de la dimension de la matrice** : La sous-routine vérifie que la matrice `A` est carrée d'ordre positif.
* **Création d'une matrice identité** : Une matrice identité de dimension `n` est créée en initialisant tous ses éléments à 0 et en affectant 1 aux éléments diagonaux.
* **Inversion de la matrice** : La matrice `A` est inversée en utilisant la décomposition LU fournie par les sous-routines `GETRF` et `GETRI` de la bibliothèque BLAS.
* **Affectation de la matrice inverse** : La matrice inverse calculée est affectée à la variable `Inverse`.
* **Programme `TestMatriceInverse`** : Ce programme test montre comment utiliser la sous-routine `MatriceInverse` pour inverser une matrice et afficher les résultats.
* **Initialisation de la matrice `A`** : Le programme initialise la matrice `A` avec des valeurs arbitraires.
* **Calcul de la matrice inverse** : La sous-routine `MatriceInverse` est appelée pour calculer la matrice inverse de `A`.
* **Affichage des matrices** : Les matrices `A` et `Inverse` sont affichées à l'écran.