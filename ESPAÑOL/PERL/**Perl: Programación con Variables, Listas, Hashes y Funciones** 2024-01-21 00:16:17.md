```perl
use strict;
use utf8;
use warnings;

# Declaración de variables y constantes
my $nombre = "Juan Pérez";
my $edad = 25;
my $hobbies = ["Leer", "Escribir", "Escuchar música"];
my $mascotas = {
    "Perro" => "Firulais",
    "Gato" => "Michi",
    "Pez" => "Nemo"
};

# Impresión de resultados
print "Nombre: $nombre\n";
print "Edad: $edad\n";
print "Hobbies:\n";
for my $hobby (@hobbies) {
    print "  - $hobby\n";
}
print "Mascotas:\n";
for my $tipo (keys %mascotas) {
    print "  - $tipo: $mascotas{$tipo}\n";
}

# Funciones auxiliares
sub saludar {
    my $nombre = shift;
    print "Hola, $nombre!\n";
}

sub sumar {
    my ($a, $b) = @_;
    return $a + $b;
}

# Uso de las funciones auxiliares
saludar("María");
my $resultado = sumar(10, 15);
print "La suma de 10 y 15 es: $resultado\n";
```

Este código Perl es más complejo que el anterior y realiza varias tareas:

* Declara variables y constantes de diferentes tipos: cadenas de texto, números, listas y hashes.
* Imprime los valores de las variables y constantes en la salida estándar.
* Define dos funciones auxiliares: `saludar` y `sumar`.
* Utiliza las funciones auxiliares para imprimir un saludo y sumar dos números.

El código está bien documentado con comentarios que explican su propósito y cada una de sus partes. También se utiliza el estándar de codificación Perl moderno, como el uso de `strict`, `use warnings` y `use utf8`.