```fortran
PROGRAMA CALCULO_NUMERICO  

! Este programa resuelve ecuaciones diferenciales parciales utilizando el método de diferencias finitas.

! Se definen las variables necesarias para el cálculo:
INTEGER, PARAMETER :: N = 100  ! Número de puntos en la malla espacial
REAL(KIND=8) :: h = 1.0 / REAL(N-1)  ! Paso de malla espacial
REAL(KIND=8) :: k = 0.5 * h**2  ! Paso de malla temporal
REAL(KIND=8) :: t = 0.0  ! Tiempo actual
REAL(KIND=8), DIMENSION(N,N) :: u  ! Solución numérica de la ecuación diferencial parcial
REAL(KIND=8), DIMENSION(N,N) :: f  ! Función fuente

! Se inicializan las condiciones de contorno:
u(1,:) = 0.0
u(N,:) = 1.0
u(:,1) = 0.0
u(:,N) = 0.0

! Se inicializa la función fuente:
f = 0.0

! Se realiza el cálculo numérico:
DO i = 1,1000  ! Se itera sobre el tiempo
  t = t + k  ! Se actualiza el tiempo actual

  ! Se calcula la solución numérica de la ecuación diferencial parcial utilizando el método de diferencias finitas:
  DO j = 2,N-1
    DO k = 2,N-1
      u(j,k) = u(j,k) + k * (
        (u(j+1,k) - 2.0*u(j,k) + u(j-1,k)) / h**2 +
        (u(j,k+1) - 2.0*u(j,k) + u(j,k-1)) / h**2 +
        f(j,k)
      )
    END DO
  END DO

  ! Se imprimen los resultados en la consola:
  PRINT *, "Solución numérica en el tiempo t = ", t
  PRINT *, u

END PROGRAM CALCULO_NUMERICO
```

Este código resuelve ecuaciones diferenciales parciales utilizando el método de diferencias finitas. Se define una malla espacial y temporal discreta, y se calcula la solución numérica de la ecuación diferencial parcial en cada punto de la malla. El código utiliza el método de diferencias finitas para calcular la solución numérica, que es un método numérico para resolver ecuaciones diferenciales parciales. El método de diferencias finitas consiste en aproximar la derivada de una función en un punto mediante la diferencia entre el valor de la función en ese punto y el valor de la función en un punto cercano. Esta aproximación se utiliza para crear una ecuación diferencial ordinaria que se puede resolver utilizando métodos numéricos estándar.

El código primero define las variables necesarias para el cálculo, incluyendo el número de puntos en la malla espacial, el paso de malla espacial, el paso de malla temporal, el tiempo actual y la solución numérica de la ecuación diferencial parcial. A continuación, se inicializan las condiciones de contorno y la función fuente.

El código entonces realiza el cálculo numérico. Itera sobre el tiempo, y para cada paso de tiempo, calcula la solución numérica de la ecuación diferencial parcial utilizando el método de diferencias finitas. La solución numérica se imprime en la consola después de cada paso de tiempo.

Este código es un ejemplo de cómo se pueden utilizar los métodos numéricos para resolver ecuaciones diferenciales parciales. Los métodos numéricos son una herramienta poderosa para resolver problemas matemáticos complejos, y se utilizan en una amplia variedad de aplicaciones, incluyendo la ingeniería, la física y la economía.