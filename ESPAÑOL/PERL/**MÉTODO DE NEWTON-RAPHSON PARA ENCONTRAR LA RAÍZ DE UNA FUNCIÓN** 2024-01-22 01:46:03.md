```perl
#!/usr/bin/perl

use strict;
use warnings;

# Definir constantes
my $MAX_ITERACIONES = 100;
my $TOL_ERROR = 1e-6;

# Definir subrutinas
sub f {
    my ($x) = @_;
    return $x*$x - 2;
}

sub df {
    my ($x) = @_;
    return 2*$x;
}

# Obtener el valor inicial
print "Introduzca el valor inicial: ";
my $x0 = <STDIN>;
chomp $x0;

# Iterar hasta que se alcance la tolerancia de error o se llegue al número máximo de iteraciones
my $iteracion = 0;
my $error = 1;
while ($error > $TOL_ERROR && $iteracion < $MAX_ITERACIONES) {
    # Calcular el siguiente valor de x
    my $x1 = $x0 - f($x0)/df($x0);

    # Calcular el error
    $error = abs($x1 - $x0);

    # Actualizar el valor de x0
    $x0 = $x1;

    # Incrementar el número de iteraciones
    $iteracion++;
}

# Imprimir el resultado
if ($error <= $TOL_ERROR) {
    print "La raíz de la función es: $x0\n";
} else {
    print "No se ha encontrado la raíz de la función en el número máximo de iteraciones.\n";
}
```

Este código implementa el método de Newton-Raphson para encontrar la raíz de una función. El método de Newton-Raphson es un método iterativo que se basa en la derivada de la función para encontrar la siguiente aproximación a la raíz. El código primero define las constantes que se utilizarán en el código, como el número máximo de iteraciones y la tolerancia de error. Luego, el código solicita al usuario que introduzca el valor inicial. A continuación, el código itera hasta que se alcanza la tolerancia de error o se llega al número máximo de iteraciones. En cada iteración, el código calcula el siguiente valor de x utilizando la fórmula de Newton-Raphson y calcula el error. Si el error es menor que la tolerancia de error, el código imprime el resultado y termina. De lo contrario, el código actualiza el valor de x0 y continúa iterando.