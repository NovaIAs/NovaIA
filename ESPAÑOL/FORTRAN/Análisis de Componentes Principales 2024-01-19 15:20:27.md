```fortran
PROGRAMA PRINCIPAL
IMPLICIT NONE

REAL, DIMENSION(100) :: X, Y
INTEGER :: I, J, N

! Solicitar al usuario el número de puntos a generar
PRINT *, '¿Cuántos puntos desea generar?'
READ *, N

! Generar los puntos X e Y aleatorios
DO I = 1, N
    X(I) = RAN(0)
    Y(I) = RAN(0)
END DO

! Calcular la matriz de correlación
DO I = 1, N
    DO J = 1, N
        R(I, J) = CORREL(X(I), Y(J))
    END DO
END DO

! Imprimir la matriz de correlación
PRINT *, 'Matriz de correlación:'
DO I = 1, N
    PRINT *, R(I, :)
END DO

! Calcular los autovalores y autovectores de la matriz de correlación
CALL EIG(R, EVAL, EVEC)

! Imprimir los autovalores y autovectores
PRINT *, 'Autovalores:'
PRINT *, EVAL

PRINT *, 'Autovectores:'
PRINT *, EVEC

END PROGRAMA PRINCIPAL

SUBROUTINE CORREL(X, Y)
IMPLICIT NONE

REAL, INTENT(IN) :: X, Y
REAL, INTENT(OUT) :: R

! Calcular la correlación entre X e Y
R = SUM((X - MEAN(X)) * (Y - MEAN(Y))) / SUM((X - MEAN(X))**2 * (Y - MEAN(Y))**2)

END SUBROUTINE CORREL

SUBROUTINE EIG(A, EVAL, EVEC)
IMPLICIT NONE

REAL, DIMENSION(N, N), INTENT(IN) :: A
REAL, DIMENSION(N), INTENT(OUT) :: EVAL
REAL, DIMENSION(N, N), INTENT(OUT) :: EVEC

! Calcular los autovalores y autovectores de A
CALL LAPACK_ZGEEV('N', 'V', N, A, N, EVAL, EVEC, N, WORK, LWORK, INFO)

END SUBROUTINE EIG
```

Este código genera un conjunto de N puntos aleatorios en el plano, calcula la matriz de correlación entre los puntos, calcula los autovalores y autovectores de la matriz de correlación e imprime los resultados.

El código utiliza las siguientes subrutinas:

* `RAN`: Genera un número aleatorio uniforme entre 0 y 1.
* `CORREL`: Calcula la correlación entre dos variables.
* `EIG`: Calcula los autovalores y autovectores de una matriz.

El código también utiliza las siguientes funciones:

* `MEAN`: Calcula la media de una variable.
* `SUM`: Calcula la suma de una variable.

El código está escrito en FORTRAN 95 y requiere una biblioteca de álgebra lineal como LAPACK.