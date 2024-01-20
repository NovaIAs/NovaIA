```perl
use strict;
use warnings;
use utf8;
use open qw(:std :utf8);

# Definir una función para saludar a alguien:
sub saludar {
    my ($nombre) = @_;
    print "Hola, $nombre!\n";
}

# Definir una función para sumar dos números:
sub sumar {
    my ($a, $b) = @_;
    return $a + $b;
}

# Definir una función para restar dos números:
sub restar {
    my ($a, $b) = @_;
    return $a - $b;
}

# Definir una función para multiplicar dos números:
sub multiplicar {
    my ($a, $b) = @_;
    return $a * $b;
}

# Definir una función para dividir dos números:
sub dividir {
    my ($a, $b) = @_;
    return $a / $b;
}

# Definir una función para calcular el factorial de un número:
sub factorial {
    my ($n) = @_;
    if ($n == 0) {
        return 1;
    } else {
        return $n * factorial($n - 1);
    }
}

# Definir una función para calcular el máximo común divisor de dos números:
sub mcd {
    my ($a, $b) = @_;
    if ($b == 0) {
        return abs($a);
    } else {
        return mcd($b, $a % $b);
    }
}

# Definir una función para calcular el mínimo común múltiplo de dos números:
sub mcm {
    my ($a, $b) = @_;
    return abs($a * $b) / mcd($a, $b);
}

# Definir una función para calcular la potencia de un número:
sub potencia {
    my ($a, $b) = @_;
    if ($b == 0) {
        return 1;
    } else {
        return $a * potencia($a, $b - 1);
    }
}

# Definir una función para calcular la raíz cuadrada de un número:
sub raiz_cuadrada {
    my ($n) = @_;
    my $x = $n / 2;
    while (abs($x * $x - $n) > 1e-6) {
        $x = ($x + $n / $x) / 2;
    }
    return $x;
}

# Definir una función para calcular el logaritmo de un número en una base determinada:
sub logaritmo {
    my ($a, $b) = @_;
    return log($a) / log($b);
}

# Definir una función para calcular el seno de un ángulo:
sub seno {
    my ($angulo) = @_;
    return sin($angulo * pi / 180);
}

# Definir una función para calcular el coseno de un ángulo:
sub coseno {
    my ($angulo) = @_;
    return cos($angulo * pi / 180);
}

# Definir una función para calcular la tangente de un ángulo:
sub tangente {
    my ($angulo) = @_;
    return tan($angulo * pi / 180);
}

# Definir una función para calcular el arcoseno de un número:
sub arcoseno {
    my ($x) = @_;
    return asin($x) * 180 / pi;
}

# Definir una función para calcular el arcocoseno de un número:
sub arcocoseno {
    my ($x) = @_;
    return acos($x) * 180 / pi;
}

# Definir una función para calcular la arcotangente de un número:
sub arcotangente {
    my ($x) = @_;
    return atan($x) * 180 / pi;
}

# Definir una función para generar una lista de números primos:
sub primos {
    my ($n) = @_;
    my @primos;
    for (my $i = 2; $i <= $n; $i++) {
        my $es_primo = 1;
        for (my $j = 2; $j < $i; $j++) {
            if ($i % $j == 0) {
                $es_primo = 0;
                last;
            }
        }
        if ($es_primo) {
            push @primos, $i;
        }
    }
    return @primos;
}

# Definir una función para generar una lista de números pares:
sub pares {
    my ($n) = @_;
    my @pares;
    for (my $i = 1; $i <= $n; $i++) {
        if ($i % 2 == 0) {
            push @pares, $i;