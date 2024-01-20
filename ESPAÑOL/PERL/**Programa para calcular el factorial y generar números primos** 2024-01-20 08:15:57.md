```perl
use strict;
use warnings;
use utf8;

# Definir una subrutina para calcular el factorial de un número

sub factorial {
    my $n = $_[0];
    if ($n <= 1) {
        return 1;
    } else {
        return $n * factorial($n - 1);
    }
}

# Definir una subrutina para generar una lista de números primos

sub generar_primos {
    my $n = $_[0];
    my @primos;
    my $i = 2;

    while (@primos < $n) {
        if (es_primo($i)) {
            push @primos, $i;
        }
        $i++;
    }

    return @primos;
}

# Definir una subrutina para comprobar si un número es primo

sub es_primo {
    my $n = $_[0];
    if ($n <= 1) {
        return 0;
    }
    if ($n == 2) {
        return 1;
    }
    if ($n % 2 == 0) {
        return 0;
    }

    my $i = 3;
    while ($i <= sqrt($n)) {
        if ($n % $i == 0) {
            return 0;
        }
        $i += 2;
    }

    return 1;
}

# Pedir al usuario que introduzca un número

print "Introduzca un número: ";
my $numero = <STDIN>;
chomp $numero;

# Comprobar si el número es válido

if ($numero !~ /^\d+$/) {
    print "El número introducido no es válido";
    exit;
}

# Calcular el factorial del número

my $factorial = factorial($numero);
print "El factorial de $numero es $factorial\n";

# Generar una lista de números primos hasta el número introducido

my @primos = generar_primos($numero);
print "Los números primos hasta $numero son @primos\n";

```

Este código en PERL es un programa que calcula el factorial de un número y genera una lista de números primos hasta ese número.

El programa comienza importando las bibliotecas necesarias, que son `strict`, `warnings` y `utf8`.

A continuación, se definen tres subrutinas: `factorial`, `generar_primos` y `es_primo`.

La subrutina `factorial` calcula el factorial de un número utilizando la recursión.

La subrutina `generar_primos` genera una lista de números primos hasta un determinado número utilizando un algoritmo que se conoce como la criba de Eratóstenes.

La subrutina `es_primo` comprueba si un número es primo o no.

Después de definir las subrutinas, el programa pide al usuario que introduzca un número.

A continuación, se comprueba si el número introducido es válido (es decir, si es un número entero positivo).

Si el número es válido, se calcula el factorial del número y se genera una lista de números primos hasta ese número.

Finalmente, se muestran los resultados en la consola.