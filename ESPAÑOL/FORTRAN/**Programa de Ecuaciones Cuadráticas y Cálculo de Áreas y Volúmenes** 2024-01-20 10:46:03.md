```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
IMPLICITO NINGUNO  ! Habilita la declaración explícita de tipos
REAL(KIND=8), INTENT(IN) :: a, b, c  ! Variables de entrada reales de precisión doble
REAL(KIND=8) :: discriminante  ! Discriminante para la selección de casos
INTEGER :: opcion  ! Variable para almacenar la opción seleccionada

! Ingreso de datos
ESCRIBIR (*,1000) "Ingrese los valores de a, b y c separados por espacios:"
1000 FORMAT (A)
LEER (*,*) a, b, c

! Cálculo del discriminante
discriminante = b**2 - 4.0 * a * c

! Selección de casos según el discriminante
SELECCIONAR CASE (discriminante)
  CASO (< 0.0)
    ESCRIBIR (*,2000) "La ecuación no tiene soluciones reales."
2000 FORMAT (A)
  CASO (0.0)
    ESCRIBIR (*,3000) "La ecuación tiene una solución real:", -b / (2.0 * a)
3000 FORMAT (A,F20.10)
  CASO (> 0.0)
    ESCRIBIR (*,4000) "La ecuación tiene dos soluciones reales:"
4000 FORMAT (A)
    x1 = (-b + SQRT(discriminante)) / (2.0 * a)
    x2 = (-b - SQRT(discriminante)) / (2.0 * a)
    ESCRIBIR (*,5000) "x1 =", x1, ", x2 =", x2
5000 FORMAT (A,F20.10,A,F20.10)
  FIN DE CASO
FIN DE SELECCIONAR

! Selección de casos según la opción ingresada
ESCRIBIR (*,6000) "Elija una opción:"
6000 FORMAT (A)
ESCRIBIR (*,7000) "1. Calcular el área de un círculo"
7000 FORMAT (A)
ESCRIBIR (*,8000) "2. Calcular el volumen de un cubo"
8000 FORMAT (A)
LEER (*,*) opcion

SELECCIONAR CASE (opcion)
  CASO (1)
    ESCRIBIR (*,9000) "Ingrese el radio del círculo:"
9000 FORMAT (A)
    LEER (*,*) radio
    area = PI * radio**2
    ESCRIBIR (*,10000) "El área del círculo es:", area
10000 FORMAT (A,F20.10)
  CASO (2)
    ESCRIBIR (*,11000) "Ingrese la longitud del lado del cubo:"
11000 FORMAT (A)
    LEER (*,*) lado
    volumen = lado**3
    ESCRIBIR (*,12000) "El volumen del cubo es:", volumen
12000 FORMAT (A,F20.10)
  FIN DE CASO
FIN DE SELECCIONAR

FIN DE PROGRAMA PRINCIPAL
```

Explicación del código:

* El programa comienza con una declaración IMPLICITO NINGUNO, que obliga a declarar explícitamente el tipo de todas las variables.
* Se declaran las siguientes variables:
    * `a`, `b` y `c` son variables reales de precisión doble para almacenar los coeficientes de la ecuación cuadrática.
    * `discriminante` es una variable real de precisión doble para almacenar el discriminante de la ecuación cuadrática.
    * `opcion` es una variable entera para almacenar la opción seleccionada por el usuario.
* El usuario ingresa los valores de `a`, `b` y `c`.
* Se calcula el discriminante de la ecuación cuadrática.
* Se utiliza una selección de casos para determinar el número de soluciones reales de la ecuación cuadrática en función del discriminante.
    * Si el discriminante es negativo, no hay soluciones reales.
    * Si el discriminante es cero, hay una solución real.
    * Si el discriminante es positivo, hay dos soluciones reales.
* Se calculan las soluciones reales de la ecuación cuadrática (si existen) y se muestran en la pantalla.
* El usuario elige una opción para calcular el área de un círculo o el volumen de un cubo.
* Se utiliza una selección de casos para calcular el área del círculo o el volumen del cubo en función de la opción seleccionada.
* Se muestran los resultados en la pantalla.