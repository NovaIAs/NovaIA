```forth
: calcular-suma
    dup
    2dup
    >r
    r>
    +r
    swap
    r>
    +r
    drop
    drop ;

: mostrar-suma
    "La suma es: " type
    swap
    . ;

: mostrar-promedio
    "El promedio es: " type
    swap
    2
    /
    . ;

: calcular-maximo
    over
    max ;

: mostrar-maximo
    "El máximo es: " type
    swap
    . ;

: calcular-minimo
    over
    min ;

: mostrar-minimo
    "El mínimo es: " type
    swap
    . ;

: procesar-lista
    begin
        dup
        while
            dup
            swap
            calcular-suma
            mostrar-suma
            calcular-promedio
            mostrar-promedio
            calcular-maximo
            mostrar-maximo
            calcular-minimo
            mostrar-minimo
            nip
            drop
        repeat
        drop ;


10
15
20
30
40
50
60
70
80
90
100
create lista

dup
procesar-lista
drop

bye
```

Este código Forth es un programa complejo que calcula y muestra la suma, el promedio, el máximo y el mínimo de una lista de números. El programa utiliza una serie de palabras definidas por el usuario (UDW) para realizar estas tareas.

La UDW `calcular-suma` calcula la suma de dos números. La UDW `mostrar-suma` muestra la suma en la pila. La UDW `calcular-promedio` calcula el promedio de dos números. La UDW `mostrar-promedio` muestra el promedio en la pila. La UDW `calcular-maximo` calcula el máximo de dos números. La UDW `mostrar-maximo` muestra el máximo en la pila. La UDW `calcular-minimo` calcula el mínimo de dos números. La UDW `mostrar-minimo` muestra el mínimo en la pila.

La UDW `procesar-lista` itera sobre una lista de números y llama a las UDW anteriores para calcular y mostrar la suma, el promedio, el máximo y el mínimo de cada número en la lista.

El programa principal crea una lista de números, llama a la UDW `procesar-lista` para procesar la lista y luego termina.