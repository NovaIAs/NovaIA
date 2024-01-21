```php
<?php

// Definimos una función recursiva para calcular el factorial de un número.
function factorial($numero) {
  if ($numero == 0) {
    return 1;
  } else {
    return $numero * factorial($numero - 1);
  }
}

// Obtenemos la entrada del usuario.
$numero = readline("Introduce un número para calcular su factorial: ");

// Calculamos el factorial del número.
$factorial = factorial($numero);

// Imprimimos el resultado.
echo "El factorial de $numero es $factorial.\n";

?>
```

Explicación del código:

* La función `factorial()` calcula el factorial de un número usando recursividad. Si el número es 0, la función devuelve 1. De lo contrario, la función multiplica el número por el factorial del número menos uno.
* La función `factorial()` se llama a sí misma, con un argumento cada vez más pequeño, hasta que llega a 0.
* La función `factorial()` es recursiva, lo que significa que se llama a sí misma.
* La función `factorial()` tiene un caso base, que es cuando el número es 0.
* La función `factorial()` tiene un caso recursivo, que es cuando el número es mayor que 0.
* La función `factorial()` usa el operador `*` para multiplicar dos números.
* La función `factorial()` usa el operador `-` para restar un número de otro.
* La función `factorial()` usa el operador `==` para comparar dos números.
* La función `readline()` obtiene una entrada del usuario.
* La función `echo()` imprime una cadena en la consola.