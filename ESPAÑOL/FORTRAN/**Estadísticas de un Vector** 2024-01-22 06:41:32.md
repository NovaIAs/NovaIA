```fortran
PROGRAMA PRINCIPAL

! Declaración de variables

IMPLICIT NONE

INTEGER, INTENT(IN) :: N   ! Número de elementos del vector
REAL, DIMENSION(N) :: A   ! Vector de entrada

! Lectura de los datos

OPEN(UNIT=1,FILE='vector.dat',STATUS='OLD')
READ(1,*) N
READ(1,*) A

! Cálculo del máximo

MAXIMO = A(1)
DO I = 2, N
  IF (A(I) > MAXIMO) THEN
    MAXIMO = A(I)
  END IF
END DO

! Cálculo del mínimo

MINIMO = A(1)
DO I = 2, N
  IF (A(I) < MINIMO) THEN
    MINIMO = A(I)
  END IF
END DO

! Cálculo de la media

MEDIA = 0.0
DO I = 1, N
  MEDIA = MEDIA + A(I)
END DO
MEDIA = MEDIA / N

! Cálculo de la desviación estándar

DESVIACION = 0.0
DO I = 1, N
  DESVIACION = DESVIACION + (A(I) - MEDIA)**2
END DO
DESVIACION = SQRT(DESVIACION / (N - 1))

! Impresión de los resultados

OPEN(UNIT=2,FILE='resultados.txt',STATUS='NEW')
WRITE(2,*) 'El máximo del vector es:', MAXIMO
WRITE(2,*) 'El mínimo del vector es:', MINIMO
WRITE(2,*) 'La media del vector es:', MEDIA
WRITE(2,*) 'La desviación estándar del vector es:', DESVIACION

CLOSE(1)
CLOSE(2)

END PROGRAMA PRINCIPAL
```

Este programa calcula el máximo, el mínimo, la media y la desviación estándar de un vector de números reales.

El programa comienza declarando las variables que se van a utilizar. La variable `N` es un entero que almacena el número de elementos del vector. La variable `A` es un vector de números reales de tamaño `N`.

A continuación, el programa lee los datos del vector de un archivo llamado `vector.dat`. El archivo debe contener una línea con el número de elementos del vector, seguida de una línea con los elementos del vector separados por espacios.

Una vez que el programa ha leído los datos, calcula el máximo, el mínimo, la media y la desviación estándar del vector.

Por último, el programa imprime los resultados en un archivo llamado `resultados.txt`. El archivo contiene cuatro líneas, una para cada una de las estadísticas calculadas.

Aquí hay una explicación detallada de cada parte del programa:

* **Declaración de variables:**

```fortran
IMPLICIT NONE

INTEGER, INTENT(IN) :: N   ! Número de elementos del vector
REAL, DIMENSION(N) :: A   ! Vector de entrada
```

La declaración `IMPLICIT NONE` indica al compilador que no se permite el uso de variables implícitas. Esto significa que todas las variables deben ser declaradas explícitamente antes de ser utilizadas.

La declaración `INTEGER, INTENT(IN) :: N` declara una variable entera llamada `N` que se utilizará para almacenar el número de elementos del vector. La palabra clave `INTENT(IN)` indica que la variable `N` sólo se puede utilizar como argumento de entrada a una subrutina o función.

La declaración `REAL, DIMENSION(N) :: A` declara un vector de números reales llamado `A` con tamaño `N`. La palabra clave `DIMENSION(N)` indica que el tamaño del vector `A` se determinará por el valor de la variable `N`.

* **Lectura de los datos:**

```fortran
OPEN(UNIT=1,FILE='vector.dat',STATUS='OLD')
READ(1,*) N
READ(1,*) A
```

La instrucción `OPEN` abre el archivo `vector.dat` para lectura. La palabra clave `UNIT=1` especifica que el archivo `vector.dat` se asociará con la unidad lógica 1. La palabra clave `STATUS='OLD'` indica que el archivo `vector.dat` ya existe y que el programa sólo puede leerlo.

La instrucción `READ` lee una línea del archivo `vector.dat` y la almacena en la variable `N`. La instrucción `READ` siguiente lee la siguiente línea del archivo `vector.dat` y la almacena en el vector `A`.

* **Cálculo del máximo:**

```fortran
MAXIMO = A(1)
DO I = 2, N
  IF (A(I) > MAXIMO) THEN
    MAXIMO = A(I)
  END IF
END DO
```

La variable `MAXIMO` se inicializa con el primer elemento del vector `A`. El bucle `DO` recorre todos los elementos del vector `A` desde el segundo hasta el último. En cada iteración del bucle, la instrucción `IF` comprueba si el elemento actual del vector `A` es mayor que `MAXIMO`. Si lo es, la variable `MAXIMO` se actualiza con el valor del elemento actual.

* **Cálculo del mínimo:**

```fortran
MINIMO = A(1)
DO I = 2, N
  IF (A(I) < MINIMO) THEN
    MINIMO = A(I)
  END IF
END DO
```

La variable `MINIMO` se inicializa con el primer elemento del vector `A`. El bucle `DO` recorre todos los elementos del vector `A` desde el segundo hasta el último. En cada iteración del bucle, la instrucción `IF` comprueba si el elemento actual del vector `A` es menor que `MINIMO`. Si lo es, la variable `MINIMO` se actualiza con el valor del elemento actual.

* **Cálculo de la media:**

```fortran
MEDIA = 0.0
DO I = 1, N
  MEDIA = MEDIA + A(I)
END DO
MEDIA = MEDIA / N
```

La variable `MEDIA` se inicializa con 0.0. El bucle `DO` recorre todos los elementos del vector `A`. En cada iteración del bucle, la variable `MEDIA` se actualiza con la suma de su valor actual y el valor del elemento actual del vector `A`. Después de que el bucle termine, la variable `MEDIA` se divide por `N` para obtener la media del vector `A`.

* **Cálculo de la desviación estándar:**

```fortran
DESVIACION = 0.0
DO I = 1, N
  DESVIACION = DESVIACION + (A(I) - MEDIA)**2
END DO
DESVIACION = SQRT(DESVIACION / (N - 1))
```

La variable `DESVIACION` se inicializa con 0.0. El bucle `DO` recorre todos los elementos del vector `A`. En cada iteración del bucle, la variable `DESVIACION` se actualiza con la suma de su valor actual y el cuadrado de la diferencia entre el elemento actual del vector `A` y la media del vector `A`. Después de que el bucle termine, la variable `DESVIACION` se divide por `(N - 1)` y luego se toma la raíz cuadrada para obtener la desviación estándar del vector `A`.

* **Impresión de los resultados:**

```fortran
OPEN(UNIT=2,FILE='resultados.txt',STATUS='NEW')
WRITE(2,*) 'El máximo del vector es:', MAXIMO
WRITE(2,*) 'El mínimo del vector es:', MINIMO
WRITE(2,*) 'La media del vector es:', MEDIA
WRITE(2,*) 'La desviación estándar del vector es:', DESVIACION
```

La instrucción `OPEN` abre el archivo `resultados.txt` para escritura. La palabra clave `UNIT=2` especifica que el archivo `resultados.txt` se asociará con la unidad lógica 2. La palabra clave `STATUS='NEW'` indica que el archivo `resultados.txt` se creará si no existe y que se sobrescribirá si ya existe.

Las instrucciones `WRITE` escriben los resultados en el archivo `resultados.txt`. La primera instrucción `WRITE` escribe el máximo del vector `A`. La segunda instrucción `WRITE` escribe el mínimo del vector `A`. La tercera instrucción `WRITE` escribe la media del vector `A`. La cuarta instrucción `WRITE` escribe la desviación estándar del vector `A`.

Finalmente, las instrucciones `CLOSE` cierran los archivos `vector.dat` y `resultados.txt`.