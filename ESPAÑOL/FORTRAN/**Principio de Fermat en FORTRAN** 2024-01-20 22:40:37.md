```fortran
PROGRAMA PRINCIPIO_DE_FERMAT
IMPRIMIR, "INTRODUZCA UN NÚMERO ENTERO POSITIVO:"
LEER, N
SI (N .LE. 0) ENTONCES
    IMPRIMIR, "EL NÚMERO INTRODUCIDO NO ES POSITIVO. INTÉNTELO DE NUEVO."
    FINALIZAR
FIN SI
PARA I = 2 A N
    IMPRIMIR, "BUSCANDO SOLUCIONES PARA N = ", N
    IMPRIMIR, "INTENTANDO: ", I
    PARA J = 1 A N - I
        PARA K = J + 1 A N - I
            SI (I**2 + J**2 .EQ. K**2) ENTONCES
                IMPRIMIR, "SOLUCIÓN ENCONTRADA: ", I, "+", J, "+", K, "=" , N
                FINALIZAR
            FIN SI
        FINALIZAR PARA
    FINALIZAR PARA
FINALIZAR PARA
IMPRIMIR, "NO SE HAN ENCONTRADO SOLUCIONES."
FINALIZAR
```

Este código es una implementación en FORTRAN del principio de Fermat, que establece que para cualquier número entero positivo N, no existe una solución entera positiva a la ecuación a^n + b^n = c^n para n mayor que 2.

El código comienza pidiendo al usuario que introduzca un número entero positivo. Si el número introducido es 0 o negativo, el programa se detiene.

A continuación, el programa itera sobre todos los números enteros positivos desde 2 hasta N, buscando soluciones a la ecuación a^n + b^n = c^n. Para cada número i, el programa itera sobre todos los números enteros positivos desde 1 hasta N - i, y para cada número j, itera sobre todos los números enteros positivos desde j + 1 hasta N - i.

Si se encuentra una solución, el programa la imprime y se detiene. Si no se encuentra ninguna solución, el programa imprime un mensaje y se detiene.

El código está bien documentado y es fácil de entender. Utiliza una estructura de control clara y simple, y el código está bien indentado.