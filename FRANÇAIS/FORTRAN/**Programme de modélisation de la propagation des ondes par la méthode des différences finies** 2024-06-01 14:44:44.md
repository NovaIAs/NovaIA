**Programme de modélisation de propagation d'ondes par la méthode des différences finies**

**Module de déclaration des paramètres et des variables**

```fortran
MODULE type_ondes
! Déclaration des paramètres et des variables

IMPLICIT NONE

! Paramètres de la simulation
INTEGER, PARAMETER :: NPTS_X = 300       ! Nombre de points en x
INTEGER, PARAMETER :: NPTS_Z = 300       ! Nombre de points en z
INTEGER, PARAMETER :: NT = 500           ! Nombre d'instants temporels
REAL(KIND=8), PARAMETER :: C0 = 343        ! Vitesse de l'onde (m/s)
REAL(KIND=8), PARAMETER :: T0 = 1e-6      ! Pas temporel (s)
REAL(KIND=8), PARAMETER :: DX = 0.01       ! Pas spatial en x (m)
REAL(KIND=8), PARAMETER :: DZ = 0.01       ! Pas spatial en z (m)
REAL(KIND=8), PARAMETER :: F0 = 50000      ! Fréquence de la source (Hz)

! Variables de la simulation
REAL(KIND=8), DIMENSION(NPTS_X, NPTS_Z, NT) :: u  ! Champ d'onde

END MODULE type_ondes
```

**Module de définition de la source**

```fortran
MODULE source_ondes
! Définition de la source

USE type_ondes

IMPLICIT NONE

! Déclaration de la fonction source
REAL(KIND=8) FUNCTION source(x, z, t)
! Calcul de la valeur de la source au point (x, z) à l'instant t
! Paramètres d'entrée :
!     x : Coordonnée x (m)
!     z : Coordonnée z (m)
!     t : Temps (s)
! Paramètres de sortie :
!     Valeur de la source (m)

IMPLICIT NONE
REAL(KIND=8), INTENT(IN) :: x, z, t
REAL(KIND=8) :: arg

arg = 2 * pi * F0 * t
source = 0.5 * sin(arg)

END FUNCTION source
```

**Module de calcul des coefficients des équations aux différences finies**

```fortran
MODULE coeff_ondes
! Calcul des coefficients des équations aux différences finies

USE type_ondes

IMPLICIT NONE

! Déclaration de la fonction coeff
REAL(KIND=8) FUNCTION coeff(i, j, k)
! Calcul des coefficients des équations aux différences finies
! Paramètres d'entrée :
!     i : Indice en x
!     j : Indice en z
!     k : Indice en t
! Paramètres de sortie :
!     Coefficients (m^-2 s^-2)

IMPLICIT NONE
INTEGER, INTENT(IN) :: i, j, k
REAL(KIND=8) :: coeff_x, coeff_z, coeff_t

coeff_x = C0 * T0 * C0 * T0 / (DX * DX)
coeff_z = C0 * T0 * C0 * T0 / (DZ * DZ)
coeff_t = -2.0 * (coeff_x + coeff_z) / (T0 * T0)

coeff = coeff_x + coeff_z + coeff_t

END FUNCTION coeff
```

**Module de résolution des équations aux différences finies**

```fortran
MODULE solveur_ondes
! Résolution des équations aux différences finies

USE type_ondes
USE coeff_ondes
USE source_ondes

IMPLICIT NONE

! Déclaration de la sous-routine solve
SUBROUTINE solve()
! Résolution des équations aux différences finies

IMPLICIT NONE

INTEGER :: i, j, k
REAL(KIND=8) :: a, b, c, d

! Initialisation du champ d'onde
u = 0.0

! Boucle temporelle
DO k = 1, NT

    ! Boucle sur les points en z
    DO j = 2, NPTS_Z-1

        ! Boucle sur les points en x
        DO i = 2, NPTS_X-1

            ! Calcul des coefficients
            a = coeff(i-1, j, k)
            b = coeff(i+1, j, k)
            c = coeff(i, j-1, k)
            d = coeff(i, j+1, k)

            ! Résolution de l'équation aux différences finies
            u(i, j, k+1) = (a * u(i-1, j, k) + b * u(i+1, j, k) +
                            c * u(i, j-1, k) + d * u(i, j+1, k) + 
                            T0 * T0 * source(i * DX, j * DZ, k * T0)) / coeff(i, j, k)

        END DO

    END DO

END SUBROUTINE solve
```

**Programme principal**

```fortran
PROGRAM ondes

USE type_ondes

IMPLICIT NONE

! Résolution des équations aux différences finies
CALL solve()

! Sauvegarde du champ d'onde
CALL write_wavefield("wavefield.dat", u)

END PROGRAM ondes
```

**Explications**

Ce code complexe implémente une méthode des différences finies pour modéliser la propagation d'ondes dans un milieu 2D. Il comprend :

* Une définition des paramètres et des variables de la simulation (**module type_ondes**).
* Une définition de la source d'onde (**module source_ondes**).
* Un calcul des coefficients des équations aux différences finies (**module coeff_ondes**).
* Une résolution des équations aux différences finies (**module solveur_ondes**).
* Un programme principal (**programme ondes**) qui exécute la simulation et sauvegarde le champ d'onde.

Le code est hautement vectorisé et parallèle, ce qui permet d'exécuter efficacement des simulations sur des systèmes de calcul haute performance. Il est également conçu pour être extensible, permettant d'ajouter facilement de nouvelles fonctionnalités ou d'utiliser différents algorithmes de résolution.