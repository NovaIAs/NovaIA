```perl
use utf8;
use strict;
use warnings;

# Definir una matriz de datos
my @datos = (
    ["Nombre", "Apellido", "Edad"],
    ["Juan", "García", 20],
    ["María", "López", 25],
    ["Pedro", "Sánchez", 30],
);

# Definir una subrutina para imprimir una tabla
sub imprimir_tabla {
    my $matriz = shift;

    # Obtener el ancho máximo de cada columna
    my @anchos = map { max(map { length($_) } @$matriz->{$_}) } 0 .. $#$matriz->{$_};

    # Imprimir el encabezado de la tabla
    print "-" x (sum(@anchos) + 3 * ($#anchos + 1)) . "\n";
    printf "| %-${anchos[0]}s | %-${anchos[1]}s | %-${anchos[2]}s |\n", @{$matriz->{$_}}[0] for 0 .. $#$matriz->{$_};
    print "-" x (sum(@anchos) + 3 * ($#anchos + 1)) . "\n";

    # Imprimir las filas de la tabla
    for my $fila (@{$matriz->{$_}}) {
        printf "| %-${anchos[0]}s | %-${anchos[1]}s | %-${anchos[2]}s |\n", @{$fila};
    }

    print "-" x (sum(@anchos) + 3 * ($#anchos + 1)) . "\n";
}

# Imprimir la tabla de datos
imprimir_tabla(\@datos);
```

Explicación:

* La primera línea del código utiliza la función `use utf8` para habilitar el soporte para caracteres Unicode.
* La segunda línea utiliza la función `use strict` para activar el modo estricto de Perl, que ayuda a detectar errores en el código.
* La tercera línea utiliza la función `use warnings` para habilitar las advertencias del compilador.
* La cuarta línea define una matriz de datos llamada `@datos`. Esta matriz contiene los datos que se mostrarán en la tabla.
* La quinta línea define una subrutina llamada `imprimir_tabla`. Esta subrutina toma una matriz de datos como argumento y la imprime en forma de tabla.
* La sexta línea de la subrutina utiliza la función `map` para obtener el ancho máximo de cada columna de la tabla.
* La séptima línea de la subrutina utiliza la función `printf` para imprimir el encabezado de la tabla.
* La octava línea de la subrutina utiliza la función `print` para imprimir una línea de separación.
* La novena línea de la subrutina utiliza la función `for` para iterar sobre las filas de la tabla.
* La décima línea de la subrutina utiliza la función `printf` para imprimir cada fila de la tabla.
* La undécima línea de la subrutina utiliza la función `print` para imprimir una línea de separación.
* La duodécima línea de la subrutina utiliza la función `imprimir_tabla` para imprimir la tabla de datos.