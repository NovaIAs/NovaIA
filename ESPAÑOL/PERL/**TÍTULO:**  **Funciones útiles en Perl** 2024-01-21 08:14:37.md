```perl
use strict;
use warnings;
use utf8;

# Esta es una función recursiva que calcula el factorial de un número.
sub factorial {
    my $num = shift;
    return 1 if $num <= 1;
    return $num * factorial($num - 1);
}

# Esta es una función que imprime un patrón de asteriscos.
sub print_asterisk_pattern {
    my $num_rows = shift;
    for (my $i = 0; $i < $num_rows; $i++) {
        print " " x ($num_rows - $i - 1);
        print "*" x ($i + 1);
        print "\n";
    }
}

# Esta es una función que imprime una tabla de multiplicar.
sub print_multiplication_table {
    my $num_rows = shift;
    my $num_cols = shift;

    print "      ";
    for (my $i = 1; $i <= $num_cols; $i++) {
        printf "%3d ", $i;
    }
    print "\n";
    print "----";
    for (my $i = 0; $i < $num_cols * 3 + 1; $i++) {
        print "-";
    }
    print "\n";

    for (my $i = 1; $i <= $num_rows; $i++) {
        printf "%3d |", $i;
        for (my $j = 1; $j <= $num_cols; $j++) {
            printf "%3d ", $i * $j;
        }
        print "\n";
    }
}

# Esta es una función que imprime un histograma de los datos.
sub print_histogram {
    my $data = shift;

    my $max_value = 0;
    foreach my $value (@$data) {
        $max_value = $value if $value > $max_value;
    }

    for (my $i = $max_value; $i > 0; $i--) {
        print "$i |";
        foreach my $value (@$data) {
            print $value >= $i ? " *" : "  ";
        }
        print "\n";
    }

    print "----";
    foreach my $value (@$data) {
        print "===";
    }
    print "\n";
}

# Esta es una función principal del programa.
sub main {
    my $num = 5;
    print "Factorial de $num: ", factorial($num), "\n";

    print_asterisk_pattern(5);

    print_multiplication_table(10, 10);

    my @data = (1, 3, 5, 7, 9, 11, 13, 15, 17, 19);
    print_histogram(\@data);
}

main();
```

* **Explicación del código:**

    * La primera parte del código define las funciones `factorial`, `print_asterisk_pattern`, `print_multiplication_table` y `print_histogram`.
    * La función `factorial` calcula el factorial de un número.
    * La función `print_asterisk_pattern` imprime un patrón de asteriscos.
    * La función `print_multiplication_table` imprime una tabla de multiplicar.
    * La función `print_histogram` imprime un histograma de los datos.

* **La segunda parte del código es la función `main`**. Esta función llama a las funciones definidas en la primera parte del código para imprimir los resultados.

* **El código completo es un ejemplo de cómo utilizar las funciones en Perl para realizar tareas complejas**. El código también es un ejemplo de cómo utilizar los bucles, las matrices y los condicionales en Perl.