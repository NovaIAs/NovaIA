```perl
use strict;
use warnings;
use utf8;

# Definir una función para imprimir un mensaje de bienvenida.
sub print_bienvenida {
    my $nombre = $_[0];
    print "Hola, $nombre!\n";
}

# Definir una función para imprimir un mensaje de despedida.
sub print_despedida {
    my $nombre = $_[0];
    print "Adiós, $nombre!\n";
}

# Definir una función para calcular el área de un triángulo.
sub calcular_area_triangulo {
    my $base = $_[0];
    my $altura = $_[1];
    my $area = 0.5 * $base * $altura;
    return $area;
}

# Definir una función para calcular el volumen de un cilindro.
sub calcular_volumen_cilindro {
    my $radio = $_[0];
    my $altura = $_[1];
    my $volumen = π * $radio**2 * $altura;
    return $volumen;
}

# Definir una matriz de nombres.
my @nombres = ("Juan", "María", "Pedro", "Ana");

# Iterar sobre la matriz de nombres e imprimir un mensaje de bienvenida para cada nombre.
foreach my $nombre (@nombres) {
    print_bienvenida($nombre);
}

# Iterar sobre la matriz de nombres e imprimir un mensaje de despedida para cada nombre.
foreach my $nombre (@nombres) {
    print_despedida($nombre);
}

# Calcular el área de un triángulo con una base de 10 y una altura de 5.
my $area_triangulo = calcular_area_triangulo(10, 5);
print "El área del triángulo es $area_triangulo\n";

# Calcular el volumen de un cilindro con un radio de 3 y una altura de 10.
my $volumen_cilindro = calcular_volumen_cilindro(3, 10);
print "El volumen del cilindro es $volumen_cilindro\n";
```

Explicación del código:

* El código utiliza la directiva `use strict` para habilitar la comprobación de tipos y la directiva `use warnings` para habilitar los avisos de errores.
* El código utiliza la codificación UTF-8 con la directiva `use utf8`.
* El código define tres funciones: `print_bienvenida`, `print_despedida` y `calcular_area_triangulo`. La función `print_bienvenida` imprime un mensaje de bienvenida a un nombre dado como argumento. La función `print_despedida` imprime un mensaje de despedida a un nombre dado como argumento. La función `calcular_area_triangulo` calcula el área de un triángulo dada su base y altura.
* El código define una variable de matriz `@nombres` con los nombres "Juan", "María", "Pedro" y "Ana".
* El código utiliza un bucle `foreach` para iterar sobre la matriz de nombres e imprimir un mensaje de bienvenida para cada nombre.
* El código utiliza otro bucle `foreach` para iterar sobre la matriz de nombres e imprimir un mensaje de despedida para cada nombre.
* El código calcula el área de un triángulo con una base de 10 y una altura de 5.
* El código calcula el volumen de un cilindro con un radio de 3 y una altura de 10.