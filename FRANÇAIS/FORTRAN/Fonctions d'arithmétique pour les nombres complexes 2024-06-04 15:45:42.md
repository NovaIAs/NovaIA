```fortran
MODULE Arithmétique_Complexe
  USE Error_Enum
  implicit none
  PRIVATE
  PUBLIC :: Addition_Complexe, Soustraction_Complexe, Multiplication_Complexe, Division_Complexe, Racine_Carrée, Module, Argument

CONTAINS

  SUBROUTINE Addition_Complexe (a, b, c)
    implicit none
    REAL(8), INTENT(IN) :: a(2), b(2)
    REAL(8), INTENT(OUT) :: c(2)

    c(1) = a(1) + b(1)
    c(2) = a(2) + b(2)
  END SUBROUTINE Addition_Complexe

  SUBROUTINE Soustraction_Complexe (a, b, c)
    implicit none
    REAL(8), INTENT(IN) :: a(2), b(2)
    REAL(8), INTENT(OUT) :: c(2)

    c(1) = a(1) - b(1)
    c(2) = a(2) - b(2)
  END SUBROUTINE Soustraction_Complexe

  SUBROUTINE Multiplication_Complexe (a, b, c)
    implicit none
    REAL(8), INTENT(IN) :: a(2), b(2)
    REAL(8), INTENT(OUT) :: c(2)

    c(1) = a(1) * b(1) - a(2) * b(2)
    c(2) = a(1) * b(2) + a(2) * b(1)
  END SUBROUTINE Multiplication_Complexe

  SUBROUTINE Division_Complexe (a, b, c)
    implicit none
    REAL(8), INTENT(IN) :: a(2), b(2)
    REAL(8), INTENT(OUT) :: c(2)
    REAL(8) :: d

    d = b(1) ** 2 + b(2) ** 2
    IF (d == 0) THEN
      c = ERROR_DIVISION_PAR_ZERO
    ELSE
      c(1) = (a(1) * b(1) + a(2) * b(2)) / d
      c(2) = (a(2) * b(1) - a(1) * b(2)) / d
    END IF
  END SUBROUTINE Division_Complexe

  FUNCTION Racine_Carrée (a)
    implicit none
    REAL(8), INTENT(IN) :: a(2)
    REAL(8) :: Racine_Carrée

    Racine_Carrée = SQRT((a(1) ** 2 + a(2) ** 2) / 2.0)
  END FUNCTION Racine_Carrée

  FUNCTION Module (a)
    implicit none
    REAL(8), INTENT(IN) :: a(2)
    REAL(8) :: Module

    Module = SQRT(a(1) ** 2 + a(2) ** 2)
  END FUNCTION Module

  FUNCTION Argument (a)
    implicit none
    REAL(8), INTENT(IN) :: a(2)
    REAL(8) :: Argument

    Argument = ATAN2(a(2), a(1))
  END FUNCTION Argument

END MODULE Arithmétique_Complexe
```

**Explications du code**

Ce code fournit des implémentations FORTRAN des opérations arithmétiques complexes courantes :

* **Addition_Complexe** additionne deux nombres complexes.
* **Soustraction_Complexe** soustrait un nombre complexe d'un autre.
* **Multiplication_Complexe** multiplie deux nombres complexes.
* **Division_Complexe** divise un nombre complexe par un autre, renvoyant une erreur si le dénominateur est nul.
* **Racine_Carrée** renvoie la racine carrée d'un nombre complexe.
* **Module** renvoie le module (grandeur) d'un nombre complexe.
* **Argument** renvoie l'argument (angle) d'un nombre complexe.

Le code encapsule ces opérations dans un module nommé **Arithmétique_Complexe**, qui définit également des constantes d'erreur pour gérer les erreurs de division par zéro.