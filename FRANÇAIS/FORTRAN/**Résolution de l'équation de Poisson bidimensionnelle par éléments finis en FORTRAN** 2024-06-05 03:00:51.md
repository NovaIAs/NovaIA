**Résolution de l'équation de Poisson bidimensionnelle à l'aide de la méthode des éléments finis**

**Programme principal**

```fortran
PROGRAM Poisson2D

  IMPLICIT NONE

  INTEGER, PARAMETER :: NX = 100, NY = 100
  REAL, PARAMETER :: Lx = 1.0, Ly = 1.0
  REAL, PARAMETER :: kappa = 1.0

  REAL, DIMENSION(NX,NY) :: u, f
  INTEGER, DIMENSION(NX,NY) :: index

  CALL SetupProblem(NX,NY,Lx,Ly,f,index)
  CALL AssembleMatrix(NX,NY,kappa,u,f,index)
  CALL SolveSystem(NX,NY,u)
  CALL OutputSolution(NX,NY,u)

END PROGRAM Poisson2D
```

**Sous-programmes**

**Initialisation du problème**

```fortran
SUBROUTINE SetupProblem(NX,NY,Lx,Ly,f,index)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: NX, NY
  REAL, INTENT(IN) :: Lx, Ly
  REAL, DIMENSION(NX,NY), INTENT(OUT) :: f
  INTEGER, DIMENSION(NX,NY), INTENT(OUT) :: index

  REAL :: dx, dy
  INTEGER :: i, j

  dx = Lx / (NX - 1)
  dy = Ly / (NY - 1)

  DO i = 1, NX
    DO j = 1, NY
      f(i,j) = 0.0
      index(i,j) = (i-1) * NY + j
    END DO
  END DO

END SUBROUTINE SetupProblem
```

**Assemblage de la matrice**

```fortran
SUBROUTINE AssembleMatrix(NX,NY,kappa,u,f,index)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: NX, NY
  REAL, INTENT(IN) :: kappa
  REAL, DIMENSION(NX,NY), INTENT(INOUT) :: u, f
  INTEGER, DIMENSION(NX,NY), INTENT(IN) :: index

  REAL :: kappa_dxdy
  INTEGER :: i, j, i1, j1

  kappa_dxdy = kappa / ((NX - 1) * (NY - 1))

  DO i = 2, NX - 1
    DO j = 2, NY - 1
      i1 = index(i,j)
      j1 = index(i,j-1)
      u(i,j) = 0.25 * ( kappa_dxdy * (u(i-1,j) + u(i,j+1) + 4.0 * u(i,j) + u(i+1,j)) - f(i,j) )
    END DO
  END DO

END SUBROUTINE AssembleMatrix
```

**Résolution du système**

```fortran
SUBROUTINE SolveSystem(NX,NY,u)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: NX, NY
  REAL, DIMENSION(NX,NY), INTENT(INOUT) :: u

  INTEGER :: i, j
  REAL :: sum

  DO i = 2, NX - 1
    DO j = 2, NY - 1
      sum = 0.0
      DO k = max(2,i-1), min(NX-1,i+1)
        sum = sum + u(k,j)
      END DO
      u(i,j) = u(i,j) / sum
    END DO
  END DO

END SUBROUTINE SolveSystem
```

**Sortie de la solution**

```fortran
SUBROUTINE OutputSolution(NX,NY,u)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: NX, NY
  REAL, DIMENSION(NX,NY), INTENT(IN) :: u

  INTEGER :: i, j
  REAL :: max_u, min_u

  max_u = -1.0e30
  min_u = 1.0e30

  DO i = 1, NX
    DO j = 1, NY
      max_u = max(u(i,j),max_u)
      min_u = min(u(i,j),min_u)
    END DO
  END DO

  PRINT *, "Solution maximale : ", max_u
  PRINT *, "Solution minimale : ", min_u

END SUBROUTINE OutputSolution
```

**Explication du code**

Ce code résout l'équation de Poisson bidimensionnelle suivante :

```
- d^2 u / dx^2 - d^2 u / dy^2 = f
```

où `u` est la solution, `f` est la fonction source et `kappa` est le coefficient de diffusivité.

Le code utilise la méthode des éléments finis pour discrétiser l'équation et assembler la matrice système. Il résout ensuite le système linéaire résultant pour obtenir la solution `u`.

Le programme principal définit les paramètres du problème, initialise la matrice et le vecteur droit, assemble la matrice, résout le système et affiche la solution.

Les sous-programmes `SetupProblem`, `AssembleMatrix`, `SolveSystem` et `OutputSolution` implémentent les différentes étapes de la résolution.

Le sous-programme `SetupProblem` initialise la matrice `f` et le tableau d'indices `index`.

Le sous-programme `AssembleMatrix` assemble la matrice système en utilisant la méthode des différences finies.

Le sous-programme `SolveSystem` résout le système linéaire assemblé en utilisant la méthode de Gauss-Seidel.

Le sous-programme `OutputSolution` affiche la solution maximale et minimale.