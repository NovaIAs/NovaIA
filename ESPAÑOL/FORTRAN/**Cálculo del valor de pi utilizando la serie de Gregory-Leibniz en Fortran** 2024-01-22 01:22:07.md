```fortran
PROGRAMA PRINCIPAL CALCULA_PI

! Declaración de variables
REAL :: pi, aproximacion_pi, diferencia, error_relativo
INTEGER :: n, i, numero_veces

! Valor inicial de n
n = 1000

! Bucle para calcular pi
DO i = 1, numero_veces
   aproximacion_pi = 0.0
   DO i = 1, n
      aproximacion_pi = aproximacion_pi + 4.0 / (8.0 * i - 4.0) - 2.0 / (8.0 * i - 3.0)
   END DO
   diferencia = ABS(pi - aproximacion_pi)
   error_relativo = diferencia / pi
   PRINT *, "n =", n, "pi =", aproximacion_pi, "diferencia =", diferencia, "error relativo =", error_relativo
END DO

END PROGRAMA PRINCIPAL CALCULA_PI

```

Este código calcula el valor de pi utilizando la serie de Gregory-Leibniz. La serie de Gregory-Leibniz es una serie infinita que converge al valor de pi. El código utiliza un bucle para calcular la aproximación de pi utilizando un número creciente de términos de la serie. El código imprime el valor de pi, la diferencia entre el valor de pi y la aproximación, y el error relativo.

El código está escrito en Fortran, un lenguaje de programación de alto nivel que se utiliza a menudo para cálculos científicos. El código está dividido en dos módulos: el programa principal y una subrutina. El programa principal llama a la subrutina para calcular el valor de pi.

La subrutina utiliza un bucle para calcular la aproximación de pi. El bucle se ejecuta un número de veces especificado por el usuario. Cada vez que el bucle se ejecuta, la subrutina calcula la aproximación de pi utilizando un número creciente de términos de la serie.

Una vez que el bucle ha terminado, la subrutina imprime el valor de pi, la diferencia entre el valor de pi y la aproximación, y el error relativo.

El código es complejo porque utiliza una serie infinita para calcular el valor de pi. La serie es difícil de converger, por lo que el código debe utilizar un número grande de términos para obtener una aproximación precisa de pi. El código también es complejo porque utiliza un bucle para calcular la aproximación de pi. El bucle se ejecuta un número de veces especificado por el usuario, lo que puede hacer que el código sea muy lento.