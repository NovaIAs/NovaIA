```perl
use strict;
use warnings;

# Crear una función que tome una lista de números y devuelva la suma de los números pares.
sub suma_pares {
    my @numeros = @_;
    my $suma = 0;
    foreach my $numero (@numeros) {
        if ($numero % 2 == 0) {
            $suma += $numero;
        }
    }
    return $suma;
}

# Obtener una lista de números del usuario.
print "Ingrese una lista de números separados por espacios: ";
my $input = <STDIN>;
chomp $input;

# Dividir la cadena de números en una lista de números.
my @numeros = split /\s+/, $input;

# Llamar a la función suma_pares para obtener la suma de los números pares.
my $suma_pares = suma_pares(@numeros);

# Imprimir la suma de los números pares.
print "La suma de los números pares es: $suma_pares\n";
```

Explicación del código:

* La función `suma_pares` toma una lista de números como argumento y devuelve la suma de los números pares en la lista.
* La función itera sobre la lista de números recibida como argumento y comprueba si cada número es par.
* Si un número es par, la función lo suma a la variable `$suma`.
* La función devuelve el valor de la variable `$suma` después de iterar sobre todos los números en la lista.
* El código principal obtiene una lista de números del usuario y divide la cadena de números en una lista de números.
* El código principal llama a la función `suma_pares` para obtener la suma de los números pares en la lista.
* El código principal imprime la suma de los números pares.