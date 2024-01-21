```perl
use strict;
use warnings;

# Definimos una función para calcular el factorial de un número.
sub factorial {
    my $num = shift;
    my $factorial = 1;

    for (my $i = 2; $i <= $num; $i++) {
        $factorial *= $i;
    }

    return $factorial;
}

# Creamos un array con los 10 primeros números naturales.
my @numeros = (1 .. 10);

# Calculamos el factorial de cada número del array y lo imprimimos.
foreach my $numero (@numeros) {
    my $factorial = factorial($numero);
    print "$numero! = $factorial\n";
}

# Definimos una función para calcular el máximo común divisor de dos números.
sub mcd {
    my $a = shift;
    my $b = shift;

    while ($b != 0) {
        my $temp = $b;
        $b = $a % $b;
        $a = $temp;
    }

    return $a;
}

# Calculamos el máximo común divisor de 12 y 18 y lo imprimimos.
my $mcd = mcd(12, 18);
print "El máximo común divisor de 12 y 18 es $mcd\n";

# Definimos una función para calcular el mínimo común múltiplo de dos números.
sub mcm {
    my $a = shift;
    my $b = shift;

    my $mcd = mcd($a, $b);

    return ($a * $b) / $mcd;
}

# Calculamos el mínimo común múltiplo de 12 y 18 y lo imprimimos.
my $mcm = mcm(12, 18);
print "El mínimo común múltiplo de 12 y 18 es $mcm\n";

# Definimos una función para comprobar si un número es primo.
sub es_primo {
    my $num = shift;

    return 1 if $num <= 1;

    for (my $i = 2; $i <= $num / 2; $i++) {
        if ($num % $i == 0) {
            return 0;
        }
    }

    return 1;
}

# Comprobamos si el número 17 es primo y lo imprimimos.
my $primo = es_primo(17);
print "17 es primo: ", ($primo ? "Sí" : "No"), "\n";

# Definimos una función para encontrar todos los números primos entre 1 y 100.
sub encontrar_primos {
    my @primos;

    for (my $i = 2; $i <= 100; $i++) {
        if (es_primo($i)) {
            push @primos, $i;
        }
    }

    return @primos;
}

# Encontramos todos los números primos entre 1 y 100 y los imprimimos.
my @primos = encontrar_primos();
print "Números primos entre 1 y 100:\n";
print join(", ", @primos), "\n";

# Definimos una función para ordenar un array de números de mayor a menor.
sub ordenar_mayor_a_menor {
    my @array = @_;

    @array = sort { $b <=> $a } @array;

    return @array;
}

# Ordenamos el array @numeros de mayor a menor y lo imprimimos.
my @numeros_ordenados = ordenar_mayor_a_menor(@numeros);
print "Números ordenados de mayor a menor:\n";
print join(", ", @numeros_ordenados), "\n";

# Definimos una función para crear un hash con las letras del alfabeto como claves y sus posiciones en el alfabeto como valores.
sub crear_hash_alfabeto {
    my %hash;

    my $posicion = 1;
    foreach my $letra ('a' .. 'z') {
        $hash{$letra} = $posicion++;
    }

    return %hash;
}

# Creamos un hash con las letras del alfabeto y lo imprimimos.
my %hash_alfabeto = crear_hash_alfabeto();
print "Hash con las letras del alfabeto:\n";
foreach my $letra (sort keys %hash_alfabeto) {
    print "$letra: $hash_alfabeto{$letra}\n";
}
```

Explicación del código:

* Se definen las funciones `factorial`, `mcd`, `mcm`, `es_primo`, `encontrar_primos`, `ordenar_mayor_a_menor` y `crear_hash_alfabeto`.
* Se crea un array con los 10 primeros números naturales.
* Se llama a la función `factorial` para calcular el factorial de cada número del array y se imprime.
* Se llama a la función `mcd` para calcular el máximo común divisor de 12 y 18 y se imprime.
* Se llama a la función `mcm` para calcular el mínimo común múltiplo de 12 y 18 y se imprime.
* Se llama a la función `es_primo` para comprobar si el número 17 es primo y se imprime.
* Se llama a la función `encontrar_primos` para encontrar todos los números primos entre 1 y 100 y se imprime.
* Se llama a la función `ordenar_mayor_a_menor` para ordenar el array @numeros de mayor a menor y se imprime.
* Se llama a la función `crear_hash_alfabeto` para crear un hash con las letras del alfabeto y se imprime.