```php
<?php

// Definir una función recursiva para calcular el factorial de un número.

function factorial($numero) {

    // Si el número es igual a 0 o 1, devolver 1.

    if ($numero == 0 || $numero == 1) {
        return 1;
    }

    // Si el número es mayor que 1, calcular el factorial del número llamando a la
    // función recursivamente y multiplicarlo por el número.

    else {
        return $numero * factorial($numero - 1);
    }

}

// Obtener el número del usuario.

$numero = readline('Ingrese un número para calcular su factorial: ');

// Comprobar si el número es válido.

if (!is_numeric($numero) || $numero < 0) {
    echo 'El número ingresado no es válido.' . PHP_EOL;
    exit;
}

// Calcular el factorial del número.

$factorial = factorial($numero);

// Mostrar el resultado.

echo 'El factorial de ' . $numero . ' es ' . $factorial . PHP_EOL;


```

Explicación del código:

1. Definimos una función recursiva llamada `factorial` que calcula el factorial de un número.

2. La función `factorial` tiene un parámetro `numero` que es el número del cual queremos calcular el factorial.

3. Dentro de la función, comprobamos si el número es igual a 0 o 1. Si es así, devolvemos 1 porque el factorial de 0 y 1 es 1.

4. Si el número es mayor que 1, calculamos el factorial del número llamando a la función `factorial` recursivamente con el número menos 1. Multiplicamos el resultado por el número para obtener el factorial.

5. Obtenemos el número del usuario usando la función `readline`.

6. Comprobamos si el número ingresado es válido. Para ello, comprobamos si es un número y si es mayor o igual a 0. Si no es válido, mostramos un mensaje de error y salimos del programa.

7. Calculamos el factorial del número llamando a la función `factorial` con el número ingresado como argumento.

8. Mostramos el resultado usando la función `echo`.