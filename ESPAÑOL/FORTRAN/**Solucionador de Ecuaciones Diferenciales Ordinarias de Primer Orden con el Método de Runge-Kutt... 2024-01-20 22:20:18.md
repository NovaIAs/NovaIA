```fortran
PROGRAMA SOLUCIONADOR DE ECUACIONES DIFERENCIALES ORDINARIAS DE PRIMER ORDEN

IMPLICIT NONE

! Declaración de variables
REAL :: t, y, h, t_final
INTEGER :: i
REAL, DIMENSION(100) :: k1, k2, k3, k4

! Valores iniciales
t = 0.0
y = 1.0
h = 0.1
t_final = 1.0

! Bucle de integración
DO i = 1, 1000
  ! Cálculo de los coeficientes de Runge-Kutta
  k1 = h * f(t, y)
  k2 = h * f(t + h/2.0, y + k1/2.0)
  k3 = h * f(t + h/2.0, y + k2/2.0)
  k4 = h * f(t + h, y + k3)

  ! Actualización de la solución
  t = t + h
  y = y + (k1 + 2.0*k2 + 2.0*k3 + k4) / 6.0

  ! Impresión de la solución
  PRINT *, t, y
END DO

CONTAINS

! Función que define la ecuación diferencial
REAL FUNCTION f(t, y)
  f = -y
END FUNCTION f

END PROGRAM SOLUCIONADOR DE ECUACIONES DIFERENCIALES ORDINARIAS DE PRIMER ORDEN
```

Este código es un solucionador numérico de ecuaciones diferenciales ordinarias de primer orden usando el método de Runge-Kutta de cuarto orden. El código está escrito en FORTRAN y utiliza las siguientes variables:

* `t`: el tiempo actual
* `y`: el valor de la solución en el tiempo actual
* `h`: el paso de tiempo
* `t_final`: el tiempo final de la integración
* `k1`, `k2`, `k3`, `k4`: los coeficientes de Runge-Kutta

El código comienza inicializando las variables con los valores iniciales dados. Luego, entra en un bucle `DO` que se ejecuta 1000 veces. En cada iteración del bucle, el código calcula los coeficientes de Runge-Kutta y actualiza la solución. Finalmente, imprime la solución en la consola.

La función `f` define la ecuación diferencial que se está resolviendo. En este caso, la ecuación diferencial es `y' = -y`, que es la ecuación diferencial de una función exponencial decreciente.

El método de Runge-Kutta de cuarto orden es un método numérico para resolver ecuaciones diferenciales ordinarias de primer orden. El método utiliza una serie de pasos para aproximar la solución de la ecuación diferencial. En cada paso, el método calcula una serie de coeficientes que se utilizan para actualizar la solución. El método de Runge-Kutta de cuarto orden es un método muy preciso y se utiliza ampliamente para resolver ecuaciones diferenciales ordinarias de primer orden.