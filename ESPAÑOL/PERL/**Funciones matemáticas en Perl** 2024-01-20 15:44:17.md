```perl
use utf8;
use strict;
use warnings;

# Definimos una función para crear una tabla de multiplicar.
sub tabla_multiplicar {
    my ($numero) = @_;

    print "Tabla de multiplicar del $numero:\n";

    for (my $i = 1; $i <= 10; $i++) {
        my $resultado = $numero * $i;

        print "$numero x $i = $resultado\n";
    }

    print "\n";
}

# Definimos una función para calcular el factorial de un número.
sub factorial {
    my ($numero) = @_;

    my $resultado = 1;

    for (my $i = 1; $i <= $numero; $i++) {
        $resultado *= $i;
    }

    return $resultado;
}

# Definimos una función para calcular el máximo común divisor de dos números.
sub maximo_comun_divisor {
    my ($numero1, $numero2) = @_;

    my $mcd = 1;

    for (my $i = 1; $i <= $numero1 && $i <= $numero2; $i++) {
        if ($numero1 % $i == 0 && $numero2 % $i == 0) {
            $mcd = $i;
        }
    }

    return $mcd;
}

# Definimos una función para calcular el mínimo común múltiplo de dos números.
sub minimo_comun_multiplo {
    my ($numero1, $numero2) = @_;

    my $mcm = $numero1 * $numero2;

    for (my $i = 1; $i <= $numero1 && $i <= $numero2; $i++) {
        if ($numero1 % $i == 0 && $numero2 % $i == 0) {
            $mcm /= $i;
        }
    }

    return $mcm;
}

# Definimos una función para comprobar si un número es primo.
sub es_primo {
    my ($numero) = @_;

    if ($numero <= 1) {
        return 0;
    }

    for (my $i = 2; $i <= $numero / 2; $i++) {
        if ($numero % $i == 0) {
            return 0;
        }
    }

    return 1;
}

# Definimos una función para encontrar todos los números primos hasta un número dado.
sub encontrar_primos {
    my ($numero) = @_;

    my @primos;

    for (my $i = 2; $i <= $numero; $i++) {
        if (es_primo($i)) {
            push @primos, $i;
        }
    }

    return @primos;
}

# Definimos una función para calcular la suma de los dígitos de un número.
sub suma_digitos {
    my ($numero) = @_;

    my $suma = 0;

    while ($numero > 0) {
        my $digito = $numero % 10;

        $suma += $digito;

        $numero = int($numero / 10);
    }

    return $suma;
}

# Definimos una función para invertir un número.
sub invertir_numero {
    my ($numero) = @_;

    my $numero_invertido = 0;

    while ($numero > 0) {
        my $digito = $numero % 10;

        $numero_invertido = $numero_invertido * 10 + $digito;

        $numero = int($numero / 10);
    }

    return $numero_invertido;
}

# Definimos una función para comprobar si un número es capicúa.
sub es_capicua {
    my ($numero) = @_;

    return $numero == invertir_numero($numero);
}

# Definimos una función para encontrar todos los números capicúas hasta un número dado.
sub encontrar_capicuas {
    my ($numero) = @_;

    my @capicuas;

    for (my $i = 1; $i <= $numero; $i++) {
        if (es_capicua($i)) {
            push @capicuas, $i;
        }
    }

    return @capicuas;
}

# Definimos una función para calcular la distancia entre dos puntos en un plano.
sub distancia {
    my ($punto1, $punto2) = @_;

    my $dx = $punto1->{x} - $punto2->{x};
    my $dy = $punto1->{y} - $punto2->{y};

    return sqrt($dx * $dx + $dy * $dy);
}

# Definimos una función para encontrar el punto más cercano a un punto dado entre un conjunto de puntos.
sub punto_mas_cercano {
    my ($punto, @puntos) = @_;

    my $punto_mas_cercano;
    my $distancia_minima = undef;

    foreach my $punto_actual (@puntos) {
        my $distancia_actual = distancia($punto, $punto_actual);

        if (!defined($distancia_minima) || $distancia_actual < $distancia_minima) {
            $punto_mas_cercano = $punto_actual;
            $distancia_minima = $distancia_actual;
        }
    }

    return $punto_mas_cercano;
}

# Definimos una función para encontrar el área de un triángulo.
sub area_triangulo {
    my ($lado1, $lado2, $lado3) = @_;

    my $semiperimetro = ($lado1 + $lado2 + $lado3) / 2;

    return sqrt($semiperimetro * ($semiperimetro - $lado1) * ($semiperimetro - $lado2) * ($semiperimetro - $lado3));
}

# Definimos una función para encontrar el área de un círculo.
sub area_circulo {
    my ($radio) = @_;

    return pi * $radio * $radio;
}

# Definimos una función para encontrar el volumen de una esfera.
sub volumen_esfera {
    my ($radio) = @_;

    return (4 / 3) * pi * $radio * $radio * $radio;
}

# Definimos una función para encontrar el área de una superficie cúbica.
sub area_cubo {
    my ($lado) = @_;

    return 6 * $lado * $lado;
}

# Definimos una función para encontrar el volumen de un cubo.
sub volumen_cubo {
    my ($lado) = @_;

    return $lado * $lado * $lado;
}

# Definimos una función para encontrar el área de una pirámide cuadrada.
sub area_piramide_cuadrada {
    my ($lado_base, $altura) = @_;

    return $lado_base *