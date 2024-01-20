```perl
#!/usr/bin/perl

use strict;
use warnings;

# Definir los datos de entrada
my $lista_de_numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
my $numero_a_buscar = 5;

# Definir la función de búsqueda binaria
sub busqueda_binaria {
    my ($lista_de_numeros, $numero_a_buscar) = @_;

    # Comprobar si la lista está vacía
    if (scalar @$lista_de_numeros == 0) {
        return -1;
    }

    # Definir los índices de inicio y fin de la búsqueda
    my $inicio = 0;
    my $fin = scalar @$lista_de_numeros - 1;

    # Realizar la búsqueda binaria
    while ($inicio <= $fin) {
        # Calcular el índice medio de la lista
        my $medio = int(($inicio + $fin) / 2);

        # Comprobar si el número buscado es igual al número medio
        if ($lista_de_numeros->[$medio] == $numero_a_buscar) {
            return $medio;
        }

        # Si el número buscado es menor que el número medio, actualizar el índice fin
        if ($lista_de_numeros->[$medio] > $numero_a_buscar) {
            $fin = $medio - 1;
        }

        # Si el número buscado es mayor que el número medio, actualizar el índice inicio
        if ($lista_de_numeros->[$medio] < $numero_a_buscar) {
            $inicio = $medio + 1;
        }
    }

    # Si no se encuentra el número buscado, devolver -1
    return -1;
}

# Llamar a la función de búsqueda binaria y mostrar el resultado
my $indice_del_numero = busqueda_binaria($lista_de_numeros, $numero_a_buscar);
if ($indice_del_numero == -1) {
    print "El número $numero_a_buscar no se encuentra en la lista.\n";
} else {
    print "El número $numero_a_buscar se encuentra en el índice $indice_del_numero de la lista.\n";
}
```

Explicación del código:

* El código define una lista de números y un número a buscar.
* Define una función llamada `busqueda_binaria` que realiza una búsqueda binaria en la lista de números para encontrar el índice del número buscado.
* La función `busqueda_binaria` toma dos argumentos: la lista de números y el número a buscar.
* La función comprueba si la lista está vacía y, si lo está, devuelve -1.
* Si la lista no está vacía, la función define los índices de inicio y fin de la búsqueda.
* La función realiza un bucle mientras el índice de inicio sea menor o igual que el índice fin.
* Dentro del bucle, la función calcula el índice medio de la lista y comprueba si el número buscado es igual al número medio.
* Si el número buscado es igual al número medio, la función devuelve el índice medio.
* Si el número buscado es menor que el número medio, la función actualiza el índice fin.
* Si el número buscado es mayor que el número medio, la función actualiza el índice inicio.
* Si el bucle termina sin encontrar el número buscado, la función devuelve -1.
* El código llama a la función `busqueda_binaria` y muestra el resultado.