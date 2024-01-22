```cool
-- Programa para calcular el factorial de un número.

-- Definición del tipo de dato entero.
type Entero = número completo;

-- Definición de la función factorial.
function Factorial(número entero n: Entero): Entero =
    if n = 0 then
        1
    else
        n * Factorial(n - 1)
    fi;

-- Definición de la función principal.
main(): Entero =
    begin
        -- Declaración de variables.
        entero n: Entero;

        -- Lee el número del usuario.
        read(n);

        -- Calcula el factorial del número.
        entero factorial: Entero := Factorial(n);

        -- Imprime el resultado.
        write(factorial);

        -- Retorna 0.
        0
    end;
```

Explicación del código:

* El programa está escrito en el lenguaje COOL.
* La primera línea del código define el tipo de dato entero.
* La segunda línea define la función factorial. La función factorial toma un entero como argumento y devuelve el factorial del entero.
* La tercera línea define la función principal. La función principal es la función que se ejecuta cuando el programa se inicia.
* La cuarta línea declara una variable entera llamada n.
* La quinta línea lee el número del usuario.
* La sexta línea calcula el factorial del número.
* La séptima línea imprime el resultado en la consola.
* La octava línea devuelve 0.

El programa funciona de la siguiente manera:

1. El programa lee el número del usuario.
2. El programa calcula el factorial del número.
3. El programa imprime el resultado en la consola.
4. El programa termina.