```perl
use strict;
use warnings;
use utf8;

# Declaración de variables
my $nombre = "Juan Pérez";
my $edad = 25;
my @hobbies = ("Leer", "Viajar", "Correr");
my %contactos = (
    "Teléfono" => "555-555-5555",
    "Correo electrónico" => "juan.perez@example.com",
);

# Impresión de datos
print "Nombre: $nombre\n";
print "Edad: $edad\n";
print "Hobbies: @hobbies\n";

# Iteración sobre un hash
print "Contactos:\n";
foreach my $tipo (keys %contactos) {
    print "$tipo: $contactos{$tipo}\n";
}

# Funciones
sub saludar {
    my $nombre = shift;
    print "Hola, $nombre!\n";
}

saludar("María");

# Clases
package Persona;

sub new {
    my ($class, %args) = @_;
    bless {
        nombre => $args{nombre},
        edad => $args{edad},
        hobbies => [],
    }, $class;
}

sub nombre {
    my ($self) = @_;
    return $self->{nombre};
}

sub edad {
    my ($self) = @_;
    return $self->{edad};
}

sub hobbies {
    my ($self) = @_;
    return $self->{hobbies};
}

sub añadir_hobby {
    my ($self, $hobby) = @_;
    push @{$self->{hobbies}}, $hobby;
}

package main;

my $persona = Persona->new(nombre => "Juan Pérez", edad => 25);

print "Nombre: " . $persona->nombre() . "\n";
print "Edad: " . $persona->edad() . "\n";
print "Hobbies: " . join(", ", $persona->hobbies()) . "\n";

$persona->añadir_hobby("Leer");
$persona->añadir_hobby("Viajar");

print "Hobbies: " . join(", ", $persona->hobbies()) . "\n";
```

Explicación del código:

1. Declaración de variables: Se declaran varias variables, incluyendo una cadena de caracteres `$nombre`, un número entero `$edad`, una matriz `@hobbies` y un hash `%contactos`.
2. Impresión de datos: Se utilizan las funciones `print` y `printf` para imprimir los valores de las variables declaradas anteriormente.
3. Iteración sobre un hash: Se utiliza un bucle `foreach` para iterar sobre el hash `%contactos`, imprimiendo la clave y el valor de cada elemento.
4. Funciones: Se define una función `saludar`, que toma un nombre como parámetro y lo imprime en la consola.
5. Clases: Se define una clase `Persona` que contiene los atributos `nombre`, `edad` y `hobbies`. La clase también define los métodos `nombre`, `edad`, `hobbies` y `añadir_hobby`.
6. Uso de la clase: Se crea una instancia de la clase `Persona` llamada `$persona` y se inicializan sus atributos. A continuación, se utilizan los métodos de la clase para imprimir los valores de los atributos y para añadir un nuevo hobby a la lista de hobbies.

Este código es un ejemplo de código complejo en Perl que utiliza una variedad de características del lenguaje, incluyendo variables, operadores, bucles, funciones y clases.