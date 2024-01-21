```forth

: GCD ( n1 n2 -- gcd )

    2swap < if
        :Loop
        dup 2>r
        rtuck /mod
        swap r> @ swap
        loop
        r> drop
        tuck
        ;Loop
    else
        swap
    then
    Loop
    drop ;GCD

: higher-order-GCD
    : GCD2 ( g n -- gcd of g and n, written as list )

        dup >r
        2dup = if
            r> drop
            0
        else
            r> @ 2dup < if
                swap
            then
            GCD2
            r> @
        then
    ;GCD2

    : GCD3 ( g1 g2 g3 -- gcd of all 3, written as list )

        2dup swap GCD2
        3dup >r 3dup <r if
            swap
        then
        GCD2

    ;GCD3

    : GCD4 ( g1 g2 g3 g4 -- gcd of all 4, written as list )

        2dup swap GCD3
        3dup >r 3dup <r if
            swap
        then
        GCD2
    ;GCD4

    : GCD5 ( g1 g2 g3 g4 g5 -- gcd of all 5, written as list )

        2dup swap GCD4
        3dup >r 3dup <r if
            swap
        then
        GCD2

    ;GCD5

    : GCD6 ( g1 g2 g3 g4 g5 g6 -- gcd of all 6, written as list )

        2dup swap GCD5
        3dup >r 3dup <r if
            swap
        then
        GCD2

    ;GCD6

    : highest-common-divisor ( n1 n2 n3 n4 n5 n6 -- gcd of all 6, as a number )

        GCD6 0 .
        2dup >r drop while
            r> @ 2dup < if
                r> @ .
                r> @ 2drop
                loop
            else
                r> drop
            then
        loop
        drop ;highest-common-divisor

    highest-common-divisor 35 15 20 25 30 40

;higher-order-GCD
```

Este código Forth implementa una función recursiva de orden superior para calcular el máximo común divisor (MCD) de una lista de enteros. La función `GCD` calcula el MCD de dos enteros, y la función `higher-order-GCD` define una serie de funciones `GCD2`, `GCD3`, `GCD4`, `GCD5` y `GCD6` para calcular el MCD de listas de 2, 3, 4, 5 y 6 enteros, respectivamente.

La función `highest-common-divisor` utiliza la función `GCD6` para calcular el MCD de una lista de 6 enteros y luego imprime el resultado.

El código utiliza el pila como una pila de datos, y las funciones utilizan `2dup`, `2swap` y `tuck` para manipular los datos en la pila.

El código también utiliza el bucle `while` para iterar sobre la lista de enteros y calcular el MCD de cada par de enteros.

El código utiliza el bucle `loop` para calcular el MCD de una lista de 2 enteros. El bucle `loop` utiliza el bucle `2>r` para dividir el primer entero por el segundo entero y almacenar el resultado en el registro `r`. El bucle `loop` utiliza el bucle `/mod` para calcular el resto de la división y almacenar el resultado en el registro `r`. El bucle `loop` utiliza el bucle `swap` para intercambiar el primer y el segundo entero. El bucle `loop` utiliza el bucle `r> @` para obtener el valor del registro `r` y almacenarlo en el primer entero. El bucle `loop` utiliza el bucle `r> @` para obtener el valor del registro `r` y almacenarlo en el segundo entero. El bucle `loop` utiliza el bucle `loop` para repetir el bucle hasta que el segundo entero sea 0. El bucle `loop` utiliza el bucle `r> drop` para eliminar el valor del registro `r` de la pila. El bucle `loop` utiliza el bucle `tuck` para almacenar el primer entero en la pila. El bucle `loop` utiliza el bucle `drop` para eliminar el segundo entero de la pila.