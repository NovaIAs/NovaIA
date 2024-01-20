```perl
use utf8;
use strict;
use warnings;

# Definición de variables
my $nombre = "Juan Pérez";
my $edad = 30;
my @hobbies = ("Leer", "Viajar", "Cocinar");
my %habilidades = (
    "Lenguaje" => "Español",
    "Programación" => "Perl",
    "Diseño" => "Photoshop"
);

# Función para imprimir información
sub imprimir_informacion {
    my $nombre = $_[0];
    my $edad = $_[1];
    my @hobbies = $_[2];
    my %habilidades = $_[3];

    print "Nombre: $nombre\n";
    print "Edad: $edad\n";
    print "Hobbies:\n";
    foreach my $hobby (@hobbies) {
        print " - $hobby\n";
    }
    print "Habilidades:\n";
    foreach my $habilidad (keys %habilidades) {
        print " - $habilidad: $habilidades{$habilidad}\n";
    }
}

# Uso de la función
imprimir_informacion($nombre, $edad, @hobbies, %habilidades);

# Uso de expresiones regulares
my $cadena = "Hola, mundo!";

if ($cadena =~ /mundo/) {
    print "La cadena contiene la palabra 'mundo'\n";
} else {
    print "La cadena no contiene la palabra 'mundo'\n";
}

# Uso de referencias
my $referencia_a_nombre = \$nombre;

print "El nombre es: $$referencia_a_nombre\n";

# Uso de closures
my $suma = sub {
    my $a = $_[0];
    my $b = $_[1];

    return $a + $b;
};

print "La suma de 1 y 2 es: " . $suma->(1, 2) . "\n";
```

Explicación del código:

* Se definen variables con diferentes tipos de datos: una cadena, un número, una lista y un hash.
* Se define una función para imprimir información sobre una persona, que recibe como argumentos el nombre, la edad, los hobbies y las habilidades.
* Se usa la función para imprimir información sobre una persona.
* Se usa una expresión regular para buscar una palabra en una cadena.
* Se usa una referencia para acceder al valor de una variable.
* Se usa un closure para definir una función que suma dos números.
* Se llama al closure para sumar dos números y se imprime el resultado.