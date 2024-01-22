```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
IMPLICIT NONE

INTEGER, PARAMETER :: N = 100 ! Número de puntos en la malla
REAL, DIMENSION(N) :: x, u, f ! Vectores de coordenadas, solución y función fuente
REAL :: h ! Tamaño de paso

! Inicialización de variables
h = 1.0 / REAL(N-1)
x(1) = 0.0
u(1) = 0.0
DO i = 2, N
   x(i) = x(i-1) + h
   u(i) = SIN(x(i))
END DO
f(1) = 0.0
f(N) = 0.0
DO i = 2, N-1
   f(i) = -2.0 * SIN(x(i))
END DO

! Resolución del sistema de ecuaciones lineales
CALL RESOLVER_SISTEMA(N, x, u, f)

! Salida de resultados
OPEN(10, FILE='solucion.txt')
WRITE(10, '(A,F10.6,A,F10.6)') 'Solución en x =', x(1), ': u =', u(1)
DO i = 2, N
   WRITE(10, '(A,F10.6,A,F10.6)') 'Solución en x =', x(i), ': u =', u(i)
END DO
CLOSE(10)

END PROGRAMA PRINCIPAL

! Subrutina para resolver el sistema de ecuaciones lineales
SUBROUTINE RESOLVER_SISTEMA(N, x, u, f)

! Declaración de variables
IMPLICIT NONE

INTEGER, INTENT(IN) :: N ! Número de puntos en la malla
REAL, INTENT(INOUT) :: x(N) ! Vector de coordenadas
REAL, INTENT(INOUT) :: u(N) ! Vector de solución
REAL, INTENT(IN) :: f(N) ! Vector de función fuente

! Inicialización de variables
REAL, DIMENSION(N) :: a, b, c, d ! Coeficientes de la matriz tridiagonal
a(1) = 1.0
b(1) = 2.0
c(1) = 1.0
d(1) = f(1)
DO i = 2, N-1
   a(i) = 1.0
   b(i) = 2.0
   c(i) = 1.0
   d(i) = f(i)
END DO
a(N) = 1.0
b(N) = 2.0
d(N) = f(N)

! Resolución del sistema de ecuaciones lineales mediante eliminación gaussiana
CALL ELIMINACION_GAUSSIANA(N, a, b, c, d)

! Sustitución hacia atrás para obtener la solución
u(N) = d(N) / b(N)
DO i = N-1, 1, -1
   u(i) = (d(i) - c(i) * u(i+1)) / b(i)
END DO

END SUBROUTINE RESOLVER_SISTEMA

! Subrutina para resolver un sistema de ecuaciones lineales tridiagonal mediante eliminación gaussiana
SUBROUTINE ELIMINACION_GAUSSIANA(N, a, b, c, d)

! Declaración de variables
IMPLICIT NONE

INTEGER, INTENT(IN) :: N ! Número de puntos en la malla
REAL, INTENT(INOUT) :: a(N) ! Vector de coeficientes diagonales inferiores
REAL, INTENT(INOUT) :: b(N) ! Vector de coeficientes diagonales principales
REAL, INTENT(INOUT) :: c(N) ! Vector de coeficientes diagonales superiores
REAL, INTENT(INOUT) :: d(N) ! Vector de términos independientes

! Inicialización de variables
REAL :: m ! Factor de multiplicación

! Eliminación hacia adelante
DO i = 2, N
   m = a(i) / b(i-1)
   b(i) = b(i) - m * c(i-1)
   d(i) = d(i) - m * d(i-1)
END DO

! Sustitución hacia atrás
u(N) = d(N) / b(N)
DO i = N-1, 1, -1
   u(i) = (d(i) - c(i) * u(i+1)) / b(i)
END DO

END SUBROUTINE ELIMINACION_GAUSSIANA
```

Este código resuelve un sistema de ecuaciones lineales tridiagonal mediante eliminación gaussiana. El sistema de ecuaciones lineales es el siguiente:

```
a_1 * u_1 + b_1 * u_2 = d_1
b_1 * u_1 + a_2 * u_2 + b_2 * u_3 = d_2
...
b_{N-1} * u_{N-1} + a_N * u_N = d_N
```

donde `a_i`, `b_i`, `c_i` y `d_i` son coeficientes conocidos y `u_i` son las incógnitas que se desean determinar.

El código primero inicializa las variables y luego llama a la subrutina `RESOLVER_SISTEMA`, que resuelve el sistema de ecuaciones lineales. La subrutina `RESOLVER_SISTEMA` llama a la subrutina `ELIMINACION_GAUSSIANA`, que resuelve el sistema de ecuaciones lineales mediante eliminación gaussiana.

El código finalmente imprime la solución del sistema de ecuaciones lineales en un archivo de texto.