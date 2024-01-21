```perl
use strict;
use warnings;

# Definición de la función "suma" que recibe dos números y devuelve su suma
sub suma {
    my ($num1, $num2) = @_;
    return $num1 + $num2;
}

# Definición de la función "resta" que recibe dos números y devuelve su resta
sub resta {
    my ($num1, $num2) = @_;
    return $num1 - $num2;
}

# Definición de la función "multiplicación" que recibe dos números y devuelve su multiplicación
sub multiplicacion {
    my ($num1, $num2) = @_;
    return $num1 * $num2;
}

# Definición de la función "división" que recibe dos números y devuelve su división
sub division {
    my ($num1, $num2) = @_;
    return $num1 / $num2;
}

# Definición de la función "potencia" que recibe un número y un exponente y devuelve el resultado de elevar el número al exponente
sub potencia {
    my ($num, $exp) = @_;
    return $num ** $exp;
}

# Definición de la función "raiz" que recibe un número y un índice y devuelve la raíz de índice del número
sub raiz {
    my ($num, $indice) = @_;
    return $num ** (1 / $indice);
}

# Definición de la función "factorial" que recibe un número y devuelve su factorial
sub factorial {
    my $num = @_;
    my $factorial = 1;
    while ($num > 0) {
        $factorial *= $num;
        $num--;
    }
    return $factorial;
}

# Definición de la función "fibonacci" que recibe un número y devuelve el n-ésimo número de Fibonacci
sub fibonacci {
    my $num = @_;
    my @fib = (0, 1);
    while ($num > 1) {
        my $next = $fib[-1] + $fib[-2];
        push @fib, $next;
        $num--;
    }
    return $fib[$num];
}

# Definición de la función "primo" que recibe un número y devuelve si es primo o no
sub primo {
    my $num = @_;
    if ($num <= 1) {
        return 0;
    }
    for (my $i = 2; $i <= sqrt($num); $i++) {
        if ($num % $i == 0) {
            return 0;
        }
    }
    return 1;
}

# Definición de la función "mayor" que recibe una lista de números y devuelve el mayor de ellos
sub mayor {
    my @numeros = @_;
    my $mayor = $numeros[0];
    for (my $i = 1; $i < scalar @numeros; $i++) {
        if ($numeros[$i] > $mayor) {
            $mayor = $numeros[$i];
        }
    }
    return $mayor;
}

# Definición de la función "menor" que recibe una lista de números y devuelve el menor de ellos
sub menor {
    my @numeros = @_;
    my $menor = $numeros[0];
    for (my $i = 1; $i < scalar @numeros; $i++) {
        if ($numeros[$i] < $menor) {
            $menor = $numeros[$i