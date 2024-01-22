```fortran
PROGRAMA Calcula_Ecuacion_Diferencial

IMPLICIT NONE

REAL, PARAMETER :: h = 0.01   ! Paso de integración
REAL, PARAMETER :: a = 0.0     ! Límite inferior de integración
REAL, PARAMETER :: b = 1.0     ! Límite superior de integración
INTEGER, PARAMETER :: n = 100 ! Número de puntos de integración

REAL :: x(n+1)             ! Vector de valores de x
REAL :: y(n+1)             ! Vector de valores de y
REAL :: k1, k2, k3, k4      ! Coeficientes del método de Runge-Kutta

! Condiciones iniciales
x(1) = a
y(1) = 1.0

! Bucle de integración
DO i = 1, n
    ! Cálculo de los coeficientes del método de Runge-Kutta
    k1 = h * f(x(i), y(i))
    k2 = h * f(x(i) + h/2.0, y(i) + k1/2.0)
    k3 = h * f(x(i) + h/2.0, y(i) + k2/2.0)
    k4 = h * f(x(i) + h, y(i) + k3)

    ! Actualización de los valores de x e y
    x(i+1) = x(i) + h
    y(i+1) = y(i) + (k1 + 2.0*k2 + 2.0*k3 + k4) / 6.0
END DO

! Imprimir los resultados
DO i = 1, n+1
    WRITE (*, *) x(i), y(i)
END DO

CONTAINS

REAL FUNCTION f(x, y)
    f = x + y
END FUNCTION f

END PROGRAM Calcula_Ecuacion_Diferencial
```

Este código resuelve una ecuación diferencial de primer orden utilizando el método de Runge-Kutta. La ecuación a resolver es:

```
dy/dx = x + y
```

con las condiciones iniciales:

```
y(0) = 1
```

El código utiliza un paso de integración de 0.01 y un número de puntos de integración de 100. El método de Runge-Kutta es un método de integración numérico que utiliza una serie de coeficientes para aproximar la solución de una ecuación diferencial.

El código primero inicializa los valores de x e y. A continuación, entra en un bucle de integración que itera sobre los puntos de integración. En cada iteración, el código calcula los coeficientes del método de Runge-Kutta y los utiliza para actualizar los valores de x e y.

Finalmente, el código imprime los resultados de la integración.