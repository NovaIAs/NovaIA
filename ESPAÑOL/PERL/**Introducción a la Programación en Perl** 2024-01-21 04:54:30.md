```perl
use strict;
use warnings;
use utf8;
use feature 'say';

my %diccionario = (
    "perro" => "dog",
    "gato" => "cat",
    "mesa" => "table",
    "silla" => "chair",
    "ventana" => "window",
);

my $palabra = "perro";

if (exists $diccionario{$palabra}) {
    say "La traducción de $palabra es $diccionario{$palabra}.";
} else {
    say "La palabra $palabra no está en el diccionario.";
}

my @lista = (1, 2, 3, 4, 5);

foreach my $elemento (@lista) {
    say "El elemento actual es $elemento.";
}

my $matriz = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];

for my $fila (@$matriz) {
    for my $columna (@$fila) {
        say "El elemento actual es $columna.";
    }
}

sub sumar($a, $b) {
    return $a + $b;
}

my $resultado = sumar(1, 2);

say "El resultado de sumar 1 y 2 es $resultado.";

my %hash = (
    "nombre" => "Juan",
    "apellido" => "Pérez",
    "edad" => 25,
);

foreach my $clave (keys %hash) {
    say "La clave $clave tiene el valor $hash{$clave}.";
}

open(my $archivo, '<', 'archivo.txt');

while (my $linea = <$archivo>) {
    say $linea;
}

close $archivo;

my $expresion_regular = qr/[0-9]+/;

if ($expresion_regular =~ "12345") {
    say "La cadena 12345 coincide con la expresión regular.";
} else {
    say "La cadena 12345 no coincide con la expresión regular.";
}

my $referencia = \$resultado;

say "El valor de la variable $resultado es $resultado.";
say "El valor de la variable $referencia es $referencia.";
say "El valor de la variable a la que apunta $referencia es $$referencia.";
```

Este código es un ejemplo de cómo utilizar varias características del lenguaje PERL. El código incluye:

* Un diccionario para traducir palabras del español al inglés.
* Un bucle `foreach` para iterar sobre una lista.
* Un bucle `for` para iterar sobre una matriz.
* Una subrutina para sumar dos números.
* Un hash para almacenar pares clave-valor.
* Una función `open()` para abrir un archivo.
* Una función `while()` para iterar sobre las líneas de un archivo.
* Una función `close()` para cerrar un archivo.
* Una expresión regular para buscar una secuencia de dígitos.
* Una referencia a una variable.