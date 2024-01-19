**Programa en FORTRAN para resolver ecuaciones diferenciales parciales**

```fortran
PROGRAMA PDEC

IMPLICIT NONE

REAL, PARAMETER :: XMIN = -1.0, XMAX = 1.0, YMIN = -1.0, YMAX = 1.0
INTEGER, PARAMETER :: NX = 100, NY = 100
REAL, DIMENSION(NX, NY) :: U, F

REAL :: DX, DY, DT

DX = (XMAX - XMIN) / (NX - 1)
DY = (YMAX - YMIN) / (NY - 1)
DT = 0.001

DO I = 1, NX
    DO J = 1, NY
        U(I, J) = 0.0
    END DO
END DO

DO I = 2, NX - 1
    DO J = 2, NY - 1
        F(I, J) = -4.0 * PI**2 * SIN(PI * I * DX) * SIN(PI * J * DY)
    END DO
END DO

DO T = 0.0, 10.0, DT
    DO I = 2, NX - 1
        DO J = 2, NY - 1
            U(I, J) = U(I, J) + DT * (
                (U(I+1, J) - 2.0*U(I, J) + U(I-1, J)) / (DX**2) +
                (U(I, J+1) - 2.0*U(I, J) + U(I, J-1)) / (DY**2) +
                F(I, J)
            )
        END DO
    END DO

    DO I = 1, NX
        U(I, 1) = 0.0
        U(I, NY) = 0.0
    END DO

    DO J = 1, NY
        U(1, J) = 0.0
        U(NX, J) = 0.0
    END DO

    CALL PLOT(U)
END DO

END PROGRAM PDEC

SUBROUTINE PLOT(U)

IMPLICIT NONE

REAL, DIMENSION(NX, NY) :: U

INTEGER :: I, J

DO I = 1, NX
    DO J = 1, NY
        PRINT *, I, J, U(I, J)
    END DO
END DO

END SUBROUTINE PLOT
```

**Explicación del código:**

El código anterior es un programa en FORTRAN para resolver ecuaciones diferenciales parciales (EDPs) mediante el método de diferencias finitas. El programa resuelve una EDP de Laplace bidimensional con condiciones de contorno de Dirichlet.

El programa comienza declarando las constantes y variables utilizadas en el programa. Las constantes incluyen el rango del dominio espacial (XMIN, XMAX, YMIN, YMAX), el número de puntos de la malla (NX, NY) y el tamaño del paso de tiempo (DT). Las variables incluyen la solución de la EDP (U) y la función fuente (F).

El programa entonces inicializa la solución de la EDP y la función fuente. La solución de la EDP se inicializa a cero en todos los puntos de la malla. La función fuente se inicializa a un valor constante en todos los puntos de la malla.

El programa entonces entra en un bucle temporal. En cada paso de tiempo, el programa actualiza la solución de la EDP utilizando el método de diferencias finitas. El método de diferencias finitas aproxima la EDP en un conjunto de ecuaciones algebraicas lineales. Estas ecuaciones se resuelven para actualizar la solución de la EDP.

Después de que la solución de la EDP se ha actualizado, el programa grafica la solución. El programa utiliza una función `PLOT` para graficar la solución. La función `PLOT` imprime los valores de la solución en la consola.

El programa continúa ejecutando el bucle temporal hasta que se alcanza el tiempo final. Una vez que se alcanza el tiempo final, el programa termina.