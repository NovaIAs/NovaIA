```perl
use strict;
use warnings;

# Definición de los datos
my %frutas = (
    'manzana' => 'verde',
    'pera' => 'amarilla',
    'naranja' => 'naranja',
    'plátano' => 'amarillo',
    'fresa' => 'roja',
    'sandía' => 'verde',
    'uva' => 'morada',
);

# Definición de las funciones
sub obtener_color {
    my $fruta = shift;
    return $frutas{$fruta} if exists $frutas{$fruta};
    return 'Desconocido';
}

sub imprimir_frutas {
    my %frutas = @_;
    foreach my $fruta (keys %frutas) {
        print "$fruta: $frutas{$fruta}\n";
    }
}

# Uso de los datos y las funciones
my $fruta = 'manzana';
my $color = obtener_color($fruta);
print "La $fruta es de color $color.\n";

my %frutas_ordenadas = sort { $frutas{$a} <=> $frutas{$b} } %frutas;
print "Lista de frutas ordenadas por color:\n";
imprimir_frutas(%frutas_ordenadas);
```

Explicación del código:

1. En primer lugar, definimos los datos que vamos a utilizar en el programa. En este caso, hemos creado un hash llamado `%frutas` que contiene el nombre de cada fruta como clave y su color correspondiente como valor.

2. A continuación, definimos las funciones que vamos a utilizar en el programa. En este caso, hemos creado dos funciones: `obtener_color()` y `imprimir_frutas()`.

3. La función `obtener_color()` recibe como argumento el nombre de una fruta y devuelve el color de esa fruta. Si la fruta no existe en el hash `%frutas`, devuelve el valor `Desconocido`.

4. La función `imprimir_frutas()` recibe como argumento un hash de frutas y lo imprime en la pantalla.

5. A continuación, utilizamos los datos y las funciones que hemos definido anteriormente para realizar las siguientes operaciones:

    * Obtenemos el color de la manzana utilizando la función `obtener_color()`.
    * Imprimimos el color de la manzana en la pantalla.
    * Ordenamos el hash `%frutas` por el color de las frutas utilizando la función `sort()`.
    * Imprimimos el hash `%frutas` ordenado en la pantalla utilizando la función `imprimir_frutas()`.