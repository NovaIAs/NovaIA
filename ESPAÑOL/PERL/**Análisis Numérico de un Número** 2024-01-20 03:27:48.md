```perl
#!/usr/bin/perl

use strict;
use warnings;

# Definir subroutine para convertir número a romano
sub num_a_romano {
    my $num = shift;

    my @romanos = (
        "M"  => 1000,
        "CM" => 900,
        "D"  => 500,
        "CD" => 400,
        "C"  => 100,
        "XC" => 90,
        "L"  => 50,
        "XL" => 40,
        "X"  => 10,
        "IX" => 9,
        "V"  => 5,
        "IV" => 4,
        "I"  => 1,
    );

    my $romano = "";
    foreach my $simbolo (keys @romanos) {
        while ($num >= $romanos{$simbolo}) {
            $romano .= $simbolo;
            $num -= $romanos{$simbolo};
        }
    }

    return $romano;
}

# Definir subroutine para obtener divisores de un número
sub divisores {
    my $num = shift;

    my @divisores = ();
    for (my $i = 1; $i <= $num/2; $i++) {
        if ($num % $i == 0) {
            push @divisores, $i;
        }
    }

    return @divisores;
}

# Definir subroutine para obtener el mayor común divisor de dos números
sub mcd {
    my $a = shift;
    my $b = shift;

    while ($b) {
        my $temp = $b;
        $b = $a % $b;
        $a = $temp;
    }

    return $a;
}

# Definir subroutine para obtener el menor común múltiplo de dos números
sub mcm {
    my $a = shift;
    my $b = shift;

    return ($a * $b) / mcd($a, $b);
}

# Obtener un número del usuario
print "Ingrese un número: ";
my $num = <STDIN>;
chomp $num;

# Utilizar las subrutinas para convertir, hallar divisores y calcular MCD y MCM
my $romano = num_a_romano($num);
my @divisores = divisores($num);
my $divisor_mayor = max(@divisores);
my $divisor_menor = min(@divisores);
my $mcd = mcd($num, $divisor_mayor);
my $mcm = mcm($num, $divisor_menor);

# Mostrar los resultados
print "Número romano: $romano\n";
print "Divisores: @divisores\n";
print "Divisor mayor: $divisor_mayor\n";
print "Divisor menor: $divisor_menor\n";
print "Máximo común divisor: $mcd\n";
print "Mínimo común múltiplo: $mcm\n";
```

**Explicación:**

Este código es una combinación de diferentes funciones matemáticas y de conversión de números. El objetivo es tomar un número entero ingresado por el usuario y proporcionar información sobre el número, incluyendo su representación romana, sus divisores, el mayor divisor común (MCD) y el menor común múltiplo (MCM).

El código utiliza tres subrutinas definidas por el usuario:

* `num_a_romano()` convierte un número entero a su representación romana utilizando un diccionario de símbolos romanos y sus valores numéricos correspondientes.
* `divisores()` toma un número entero y devuelve una lista de sus divisores.
* `mcd()` calcula el máximo común divisor de dos números.
* `mcm()` calcula el menor común múltiplo de dos números.

El código primero solicita al usuario que ingrese un número entero. Luego, utiliza las subrutinas definidas anteriormente para calcular los valores deseados y los muestra en la pantalla.

Este código es un ejemplo de cómo Perl se puede utilizar para realizar cálculos matemáticos y convertir números entre diferentes representaciones. También es un ejemplo de cómo las subrutinas pueden ser utilizadas para definir y organizar el código de manera modular.