```perl
use strict;
use warnings;

# Definición de variables
my $nombre = "Juan Pérez";
my $edad = 25;
my @hobbies = ("Leer", "Escribir", "Viajar");
my %habilidades = (
    "Lenguaje de programación" => "Perl",
    "Base de datos" => "MySQL",
    "Sistema operativo" => "Linux"
);

# Imprime el nombre y la edad
print "Nombre: $nombre\n";
print "Edad: $edad\n";

# Imprime los hobbies
print "Hobbies:\n";
foreach my $hobby (@hobbies) {
    print "  * $hobby\n";
}

# Imprime las habilidades
print "Habilidades:\n";
foreach my $habilidad (keys %habilidades) {
    print "  * $habilidad: $habilidades{$habilidad}\n";
}

# Crea una función para calcular el área de un círculo
sub area_circulo {
    my $radio = shift;
    my $area = pi * $radio ** 2;
    return $area;
}

# Calcula y muestra el área de un círculo con radio 5
my $radio = 5;
my $area = area_circulo($radio);
print "Área del círculo con radio $radio: $area\n";

# Crea una clase para representar a una persona
package Persona;

sub new {
    my $class = shift;
    my $self = {
        nombre => shift,
        edad => shift,
        hobbies => shift,
        habilidades => shift
    };
    bless $self, $class;
    return $self;
}

sub nombre {
    my $self = shift;
    return $self->{nombre};
}

sub edad {
    my $self = shift;
    return $self->{edad};
}

sub hobbies {
    my $self = shift;
    return $self->{hobbies};
}

sub habilidades {
    my $self = shift;
    return $self->{habilidades};
}

package main;

# Crea un objeto de la clase Persona
my $persona = Persona->new($nombre, $edad, \@hobbies, \%habilidades);

# Imprime el nombre, la edad, los hobbies y las habilidades de la persona
print "Nombre: ", $persona->nombre, "\n";
print "Edad: ", $persona->edad, "\n";
print "Hobbies:\n";
foreach my $hobby ($persona->hobbies) {
    print "  * $hobby\n";
}
print "Habilidades:\n";
foreach my $habilidad (keys $persona->habilidades) {
    print "  * $habilidad: $persona->habilidades{$habilidad}\n";
}
```

Explicación del código:

* El código utiliza el módulo `strict` para garantizar que las variables se declaren explícitamente y el módulo `warnings` para mostrar advertencias sobre problemas potenciales en el código.
* Se definen varias variables, incluyendo el nombre, la edad, los hobbies y las habilidades de una persona.
* Se imprime el nombre y la edad de la persona.
* Se imprime una lista de los hobbies de la persona.
* Se imprime una lista de las habilidades de la persona.
* Se crea una función llamada `area_circulo` que calcula el área de un círculo dado su radio.
* Se calcula y muestra el área de un círculo con radio 5.
* Se crea una clase llamada `Persona` que representa a una persona. La clase tiene los siguientes métodos:
    * `new`: Crea un nuevo objeto de la clase `Persona`.
    * `nombre`: Devuelve el nombre de la persona.
    * `edad`: Devuelve la edad de la persona.
    * `hobbies`: Devuelve los hobbies de la persona.
    * `habilidades`: Devuelve las habilidades de la persona.
* Se crea un objeto de la clase `Persona` y se imprimen el nombre, la edad, los hobbies y las habilidades de la persona.