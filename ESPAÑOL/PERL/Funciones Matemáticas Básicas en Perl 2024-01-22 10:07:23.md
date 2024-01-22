```perl
use strict;
use warnings;

# Definir una función para saludar
sub saludar {
    my ($nombre) = @_;
    print "Hola, $nombre!\n";
}

# Definir una función para sumar dos números
sub sumar {
    my ($a, $b) = @_;
    return $a + $b;
}

# Definir una función para restar dos números
sub restar {
    my ($a, $b) = @_;
    return $a - $b;
}

# Definir una función para multiplicar dos números
sub multiplicar {
    my ($a, $b) = @_;
    return $a * $b;
}

# Definir una función para dividir dos números
sub dividir {
    my ($a, $b) = @_;
    return $a / $b;
}

# Definir una función para calcular el factorial de un número
sub factorial {
    my ($n) = @_;
    if ($n == 0) {
        return 1;
    } else {
        return $n * factorial($n - 1);
    }
}

# Definir una función para calcular el máximo común divisor de dos números
sub mcd {
    my ($a, $b) = @_;
    if ($b == 0) {
        return $a;
    } else {
        return mcd($b, $a % $b);
    }
}

# Definir una función para calcular el mínimo común múltiplo de dos números
sub mcm {
    my ($a, $b) = @_;
    return ($a * $b) / mcd($a, $b);
}

# Definir una función para convertir un número decimal a binario
sub decimal_a_binario {
    my ($n) = @_;
    my $binario = "";
    while ($n > 0) {
        $binario = ($n % 2) . $binario;
        $n = int($n / 2);
    }
    return $binario;
}

# Definir una función para convertir un número binario a decimal
sub binario_a_decimal {
    my ($binario) = @_;
    my $decimal = 0;
    my $potencia = 1;
    for (my $i = length($binario) - 1; $i >= 0; $i--) {
        $decimal += substr($binario, $i, 1) * $potencia;
        $potencia *= 2;
    }
    return $decimal;
}

# Definir una función para generar una lista de números primos
sub primos {
    my ($n) = @_;
    my @primos;
    for (my $i = 2; $i <= $n; $i++) {
        my $primo = 1;
        for (my $j = 2; $j < $i; $j++) {
            if ($i % $j == 0) {
                $primo = 0;
                last;
            }
        }
        if ($primo) {
            push @primos, $i;
        }
    }
    return @primos;
}

# Definir una función para generar una lista de números pares
sub pares {
    my ($n) = @_;
    my @pares;
    for (my $i = 2; $i <= $n; $i += 2) {
        push @pares, $i;
    }
    return @pares;
}

# Definir una función para generar una lista de números impares
sub impares {
    my ($n) = @_;
    my @impares;
    for (my $i = 1; $i <= $n; $i += 2) {
        push @impares, $i;
    }
    return @impares;
}

# Definir una función para ordenar una lista de números
sub ordenar {
    my (@lista) = @_;
    @lista.sort { $a <=> $b };