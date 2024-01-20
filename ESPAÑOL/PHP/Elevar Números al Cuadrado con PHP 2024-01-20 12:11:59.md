```php
<?php

// Definir la función principal
function principal() {
    $lista_numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    // Usar array_map para aplicar la función "elevar_al_cuadrado" a cada elemento de la lista de números
    $lista_numeros_cuadrados = array_map("elevar_al_cuadrado", $lista_numeros);

    // Definir una función auxiliar para elevar un número al cuadrado
    function elevar_al_cuadrado($numero) {
        return $numero * $numero;
    }

    // Usar foreach para iterar sobre la lista de números cuadrados y mostrarlos en pantalla
    foreach ($lista_numeros_cuadrados as $numero_cuadrado) {
        echo "El número $numero_cuadrado es el cuadrado de un número de la lista original." . PHP_EOL;
    }
}

// Llamar a la función principal
principal();

?>
```

Explicación del código:

- La función principal() define una lista de números y luego usa la función array_map() para aplicar la función elevar_al_cuadrado() a cada elemento de la lista.
- La función auxiliar elevar_al_cuadrado() calcula el cuadrado de un número.
- La función principal() usa un foreach para iterar sobre la lista de números cuadrados y mostrarlos en pantalla.