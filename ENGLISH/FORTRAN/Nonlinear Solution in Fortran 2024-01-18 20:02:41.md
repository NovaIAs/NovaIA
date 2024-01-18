```fortran

MODULE NONLINEAR_SOLUTION_MODULE
IMPLICIT NONE

! TYPE DEFINITIONS
TYPE, PUBLIC, EXTENDS(SOLUTION) :: NONLINEAR_SOLUTION
  PRIVATE
  REAL, DIMENSION(:), ALLOCATABLE :: X0 ! initial guess for x
  REAL, DIMENSION(:), ALLOCATABLE :: X0_SCALED ! scaled initial guess for x
  LOGICAL :: converged ! convergence flag
  REAL :: ftol ! tolerance for convergence (root-mean-square)
  REAL :: gtol ! tolerance for convergence (gradient)
  INTEGER :: maxit ! maximum number of iterations
  INTEGER :: iter ! current iteration number
  CHARACTER(len=16) :: method ! method to be used ("Newton", "Secant", "Bisection")
  INTEGER :: method_index ! index of method in method_list
  TYPE(FUNCTION) :: F ! function to be solved
  TYPE(FUNCTION) :: J ! Jacobian matrix of function F
  TYPE(FUNCTION) :: R ! residual function
  TYPE(FUNCTION) :: J_SCALED ! scaled Jacobian matrix of function F
  TYPE(FUNCTION) :: R_SCALED ! scaled residual function
END TYPE NONLINEAR_SOLUTION

! PROCEDURE DEFINITIONS
CONTAINS
  ! CONSTRUCTOR
  PROCEDURE, PUBLIC :: NONLINEAR_SOLUTION => CONSTRUCTOR
  ! DESTRUCTOR
  PROCEDURE, PUBLIC :: FINALIZE => FINALIZER

! CONSTRUCTOR
PROCEDURE, PUBLIC :: NONLINEAR_SOLUTION => CONSTRUCTOR
! Initialize a NONLINEAR_SOLUTION object.
!   REQUIRED ARGUMENTS:
!     F - function to be solved
!     J - Jacobian matrix of function F
!   OPTIONAL ARGUMENTS:
!     X0 - initial guess for x (default: [0, 0, ..., 0])
!     ftol - tolerance for convergence (root-mean-square) (default: 1e-6)
!     gtol - tolerance for convergence (gradient) (default: 1e-6)
!     maxit - maximum number of iterations (default: 100)
!     method - method to be used ("Newton", "Secant", "Bisection") (default: "Newton")

IMPLICIT NONE

! DECLARATIONS
CLASS(NONLINEAR_SOLUTION) :: THIS
REAL, DIMENSION(:), INTENT(IN), OPTIONAL :: X0 = 0
REAL, INTENT(IN), OPTIONAL :: ftol = 1e-6
REAL, INTENT(IN), OPTIONAL :: gtol = 1e-6
INTEGER, INTENT(IN), OPTIONAL :: maxit = 100
CHARACTER(len=16), INTENT(IN), OPTIONAL :: method = "Newton"

! PROCEDURE BODY
IF (PRESENT(X0)) THEN
  ALLOCATE(THIS%X0(SIZE(X0)))
  THIS%X0 = X0
  ALLOCATE(THIS%X0_SCALED(SIZE(X0)))
  THIS%X0_SCALED = X0
ELSE
  ALLOCATE(THIS%X0(1))
  THIS%X0 = 0
  ALLOCATE(THIS%X0_SCALED(1))
  THIS%X0_SCALED = 0
END IF
THIS%converged = .FALSE.
THIS%ftol = ftol
THIS%gtol = gtol
THIS%maxit = maxit
THIS%iter = 0
THIS%method = method
SELECT CASE (THIS%method)
  CASE ("Newton")
    THIS%method_index = 1
  CASE ("Secant")
    THIS%method_index = 2
  CASE ("Bisection")
    THIS%method_index = 3
  CASE DEFAULT
    STOP "Invalid method specified."
END SELECT
THIS%F = F
THIS%J = J
THIS%R = R
THIS%J_SCALED = J_SCALED
THIS%R_SCALED = R_SCALED

END PROCEDURE NONLINEAR_SOLUTION => CONSTRUCTOR

! DESTRUCTOR
PROCEDURE, PUBLIC :: FINALIZE => FINALIZER
! Destroy a NONLINEAR_SOLUTION object.

IMPLICIT NONE

! DECLARATIONS
CLASS(NONLINEAR_SOLUTION) :: THIS

! PROCEDURE BODY
DEALLOCATE(THIS%X0)
DEALLOCATE(THIS%X0_SCALED)

END PROCEDURE FINALIZE => FINALIZER

```