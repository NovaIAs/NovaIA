```fortran
PROGRAMA PRINCIPAL

IMPRIMIR TÍTULO DEL PROGRAMA
IMPRIMIR(1,100)
100 FORMAT(1X, 'PROGRAMA PRINCIPAL')

INICIALIZAR VARIABLES
SUMA = 0.0
PRODUCTO = 1.0

LEER DATOS
IMPRIMIR(1,110)
110 FORMAT(1X, 'INTRODUZCA LOS DATOS:')
LEER(1,120) NUM_DATOS, VALOR_INICIAL
120 FORMAT(I5,F10.2)

ITERAR SOBRE LOS DATOS
DO I = 1, NUM_DATOS

    SUMAR EL VALOR ACTUAL A LA SUMA
    SUMA = SUMA + VALOR_INICIAL

    MULTIPLICAR EL VALOR ACTUAL POR EL PRODUCTO
    PRODUCTO = PRODUCTO * VALOR_INICIAL

    INCREMENTAR EL VALOR INICIAL
    VALOR_INICIAL = VALOR_INICIAL + 1.0

FIN DE LA ITERACIÓN

IMPRIMIR RESULTADOS
IMPRIMIR(1,130) SUMA, PRODUCTO
130 FORMAT(1X, 'LA SUMA DE LOS DATOS ES:', F10.2)
140 FORMAT(1X, 'EL PRODUCTO DE LOS DATOS ES:', F10.2)

FIN DEL PROGRAMA
END PROGRAM PRINCIPAL

```

Este programa lee una serie de números introducidos por el usuario y calcula la suma y el producto de dichos números.

El programa comienza imprimiendo el título del programa y solicitando al usuario que introduzca los datos.

A continuación, se inicializan las variables `SUMA` y `PRODUCTO` a 0.0 y 1.0, respectivamente.

El programa entra en un bucle en el que se itera sobre los datos introducidos por el usuario. En cada iteración, se suma el valor actual al valor de `SUMA` y se multiplica el valor actual por el valor de `PRODUCTO`. Además, se incrementa el valor de `VALOR_INICIAL` en 1.0.

Una vez que se han procesado todos los datos, el programa imprime los valores de `SUMA` y `PRODUCTO`.