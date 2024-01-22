```modula-2

MODULE OrdenarLista;

FROM Terminal IMPORT
    LeerEntero, EscribirCadena, EscribirSalto;

TYPE
    Enteros = ARRAY OF INTEGER;           -- Un array de enteros

    -- Procedimiento para ordenar una lista de enteros
    PROCEDURE Ordenar(VAR v: Enteros);
    BEGIN
        VAR i, j: INTEGER;              -- Índices para el bucle
        VAR temp: INTEGER;               -- Variable temporal para intercambio
        FOR i := 1 TO UPB v DO
            FOR j := i + 1 TO UPB v DO
                IF v[i] > v[j] THEN
                    temp := v[i];
                    v[i] := v[j];
                    v[j] := temp;
                END;
            END;
        END;
    END Ordenar;

VAR
    v: Enteros;                      -- La lista de enteros
    n: INTEGER;                     -- El número de elementos en la lista

BEGIN
    -- Lee el número de elementos en la lista
    EscribirCadena("¿Cuántos elementos tiene la lista? ");
    n := LeerEntero;

    -- Asigna espacio para la lista
    v := ARRAY(1 .. n) OF INTEGER;

    -- Lee los elementos de la lista
    FOR i := 1 TO n DO
        EscribirCadena("Elemento " & INTEGER'IMAGE(i) & ": ");
        v[i] := LeerEntero;
    END;

    -- Ordena la lista
    Ordenar(v);

    -- Muestra la lista ordenada
    EscribirSalto;
    EscribirCadena("Lista ordenada: ");
    FOR i := 1 TO n DO
        EscribirCadena(INTEGER'IMAGE(v[i]) & " ");
    END;
    EscribirSalto;
END OrdenarLista.

```

Explicación:

El código anterior es un programa en MODULA-2 que ordena una lista de enteros ingresada por el usuario.

El programa primero declara el tipo `Enteros` como un array de enteros. Luego, declara el procedimiento `Ordenar` que toma un array de enteros y lo ordena en orden ascendente.

El procedimiento `Ordenar` utiliza un algoritmo de ordenamiento de burbujas para ordenar el array. El algoritmo funciona comparando cada elemento del array con sus vecinos y intercambiando los elementos si están en el orden incorrecto.

Después de declarar el tipo `Enteros` y el procedimiento `Ordenar`, el programa declara las variables `v`, `n` y `i`. La variable `v` es un array que almacenará la lista de enteros ingresada por el usuario. La variable `n` es un entero que almacenará el número de elementos en la lista. La variable `i` es un entero que se utilizará como índice para recorrer el array.

El programa luego solicita al usuario que ingrese el número de elementos en la lista. Luego, asigna espacio para el array `v` y solicita al usuario que ingrese los elementos de la lista.

Después de que el usuario ha ingresado todos los elementos de la lista, el programa llama al procedimiento `Ordenar` para ordenar la lista.

Finalmente, el programa muestra la lista ordenada en la consola.