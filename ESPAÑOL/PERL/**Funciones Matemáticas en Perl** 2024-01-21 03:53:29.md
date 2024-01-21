```perl
use strict;
use warnings;
use utf8;

# Función para obtener el factorial de un número
sub factorial {
    my $n = shift;
    my $result = 1;
    for (my $i = 2; $i <= $n; $i++) {
        $result *= $i;
    }
    return $result;
}

# Función para generar una lista de números primos
sub is_prime {
    my $n = shift;
    if ($n <= 1) {
        return 0;
    }
    for (my $i = 2; $i <= int(sqrt($n)); $i++) {
        if ($n % $i == 0) {
            return 0;
        }
    }
    return 1;
}

sub generar_primos {
    my $limit = shift;
    my @primos;
    for (my $i = 2; $i <= $limit; $i++) {
        if (is_prime($i)) {
            push @primos, $i;
        }
    }
    return @primos;
}

# Función para calcular el máximo común divisor de dos números
sub mcd {
    my $a = shift;
    my $b = shift;
    while ($b != 0) {
        ($a, $b) = ($b, $a % $b);
    }
    return $a;
}

# Función para calcular el mínimo común múltiplo de dos números
sub mcm {
    my $a = shift;
    my $b = shift;
    my $mcd = mcd($a, $b);
    return ($a * $b) / $mcd;
}

# Función para generar una lista de números perfectos
sub generar_perfectos {
    my $limit = shift;
    my @perfectos;
    for (my $i = 2; $i <= $limit; $i++) {
        my $suma_divisores = 0;
        for (my $j = 1; $j < $i; $j++) {
            if ($i % $j == 0) {
                $suma_divisores += $j;
            }
        }
        if ($suma_divisores == $i) {
            push @perfectos, $i;
        }
    }
    return @perfectos;
}

# Función para generar una lista de números de Fi
sub generar_fi {
    my $limit = shift;
    my @fi;
    for (my $i = 1; $i <= $limit; $i++) {
        my $fi_i = 0;
        for (my $j = 1; $j < $i; $j++) {
            if (mcd($i, $j) == 1) {
                $fi_i++;
            }
        }
        push @fi, $fi_i;
    }
    return @fi;
}

# Función para generar una lista de números de Catalan
sub generar_catalan {
    my $limit = shift;
    my @catalan;
    $catalan[0] = 1;
    for (my $i = 1; $i <= $limit; $i++) {
        $catalan[$i] = 0;
        for (my $j = 0; $j < $i; $j++) {
            $catalan[$i] += $catalan[$j] * $catalan[$i - $j - 1];
        }
    }
    return @catalan;
}

# Función para generar una lista de números de Fibonacci
sub generar_fibonacci {
    my $limit = shift;
    my @fibonacci;
    $fibonacci[0] = 0;
    $fibonacci[1] = 1;
    for (my $i = 2; $i <= $limit; $i++) {
        $fibonacci[$i] = $fibonacci[$i - 1] + $fibonacci[$i - 2];
    }
    return @fibonacci;
}

# Función para generar