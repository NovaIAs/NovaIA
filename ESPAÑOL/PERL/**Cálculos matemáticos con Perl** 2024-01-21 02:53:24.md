```perl
use strict;
use warnings;

# Definición de constantes
use constant PI => 3.141592653589793;
use constant E  => 2.718281828459045;

# Definición de funciones
sub factorial {
    my $n = shift;
    return 1 if $n == 0;
    return $n * factorial($n - 1);
}

sub fibonacci {
    my $n = shift;
    return 0 if $n == 0;
    return 1 if $n == 1;
    return fibonacci($n - 1) + fibonacci($n - 2);
}

sub es_primo {
    my $n = shift;
    return 1 if $n == 2;
    return 0 if $n <= 1 || $n % 2 == 0;
    for (my $i = 3; $i <= sqrt($n); $i += 2) {
        return 0 if $n % $i == 0;
    }
    return 1;
}

# Lectura de entrada
print "Ingrese un número: ";
my $n = <STDIN>;
chomp $n;

# Procesamiento de la entrada
$n = int($n);

# Salida de resultados
print "Factorial de $n: ", factorial($n), "\n";
print "Número de Fibonacci en la posición $n: ", fibonacci($n), "\n";
print "$n es primo: ", es_primo($n) ? "Sí" : "No", "\n";
```

Explicación del código:

1. Definición de constantes: Se definen dos constantes, PI y E, usando la función `use constant`. Estas constantes se pueden utilizar en todo el programa.

2. Definición de funciones: Se definen tres funciones:

    * `factorial`: Esta función calcula el factorial de un número dado.
    * `fibonacci`: Esta función calcula el número de Fibonacci en una posición dada.
    * `es_primo`: Esta función comprueba si un número dado es primo.

3. Lectura de entrada: Se utiliza la función `print` para mostrar un mensaje al usuario y la función `<STDIN>` para leer la entrada del usuario. La entrada se asigna a la variable `$n`.

4. Procesamiento de la entrada: Se utiliza la función `int()` para convertir la entrada de texto a un número entero.

5. Salida de resultados: Se utilizan las funciones `print` y `es_primo` para mostrar los resultados de los cálculos al usuario.