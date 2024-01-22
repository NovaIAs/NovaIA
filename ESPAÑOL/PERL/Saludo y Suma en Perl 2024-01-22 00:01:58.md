```perl
#! /usr/bin/perl

use strict;
use warnings;
use utf8;

# Definición de subrutinas

sub saludar {
    my $persona = shift;
    print "Hola, $persona!\n";
}

sub sumar {
    my @numeros = @_;
    my $suma = 0;
    foreach my $numero (@numeros) {
        $suma += $numero;
    }
    return $suma;
}

# Creación de una lista de nombres

my @nombres = ("Juan", "María", "Pedro", "Ana");

# Iteración sobre la lista de nombres

foreach my $nombre (@nombres) {
    saludar($nombre);
}

# Cálculo de la suma de una lista de números

my @numeros = (1, 2, 3, 4, 5);
my $suma = sumar(@numeros);

# Impresión del resultado

print "La suma de los números es: $suma\n";
```

Explicación del código:

* La primera línea es la línea shebang, que indica al sistema operativo que el script debe ser ejecutado con el intérprete de Perl.
* Las siguientes tres líneas son directivas de uso, que indican al compilador de Perl que utilice las bibliotecas estrictas, de advertencias y de UTF-8.
* A continuación se definen dos subrutinas: `saludar` y `sumar`. La subrutina `saludar` simplemente imprime un saludo a la persona especificada como argumento. La subrutina `sumar` suma una lista de números y devuelve el resultado.
* La siguiente línea crea una lista de nombres.
* La siguiente línea itera sobre la lista de nombres y llama a la subrutina `saludar` para cada nombre.
* La siguiente línea crea una lista de números.
* La siguiente línea llama a la subrutina `sumar` para calcular la suma de la lista de números.
* La última línea imprime el resultado en la consola.