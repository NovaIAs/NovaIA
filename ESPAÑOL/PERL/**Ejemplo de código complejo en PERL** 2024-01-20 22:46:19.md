```perl
use strict;
use warnings;

# Definir variables
my $nombre = "Juan Pérez";
my $edad = 30;
my $profesión = "Ingeniero";

# Crear un hash con los datos de la persona
my %persona = (
    nombre => $nombre,
    edad => $edad,
    profesión => $profesión
);

# Imprimir los datos de la persona en un formato bonito
print "Nombre: $persona{nombre}\n";
print "Edad: $persona{edad}\n";
print "Profesión: $persona{profesión}\n";

# Crear un array con los nombres de tres colores
my @colores = ("rojo", "verde", "azul");

# Imprimir los nombres de los colores en un bucle
print "Colores:\n";
foreach my $color (@colores) {
    print "$color\n";
}

# Crear una función que calcule el área de un triángulo
sub area_triangulo {
    my ($base, $altura) = @_;
    return 0.5 * $base * $altura;
}

# Calcular el área de un triángulo con base 10 y altura 5
my $area = area_triangulo(10, 5);

# Imprimir el área del triángulo
print "Área del triángulo: $area\n";

# Crear un objeto de la clase Persona
my $persona2 = new Persona("María García", 25, "Doctora");

# Imprimir los datos de la persona
print "Nombre: $persona2->{nombre}\n";
print "Edad: $persona2->{edad}\n";
print "Profesión: $persona2->{profesión}\n";

# Crear una clase Persona
package Persona;

sub new {
    my ($class, %args) = @_;
    my $self = bless {
        nombre => $args{nombre},
        edad => $args{edad},
        profesión => $args{profesión}
    }, $class;
    return $self;
}

sub nombre {
    my ($self) = @_;
    return $self->{nombre};
}

sub edad {
    my ($self) = @_;
    return $self->{edad};
}

sub profesión {
    my ($self) = @_;
    return $self->{profesión};
}

1;
```

Este código es un ejemplo de código complejo en PERL. El código crea un hash con los datos de una persona, un array con los nombres de tres colores, una función que calcula el área de un triángulo y un objeto de la clase Persona. El código también crea una clase Persona con tres métodos: nombre, edad y profesión.

El código está bien documentado y utiliza muchas de las características del lenguaje PERL, como los hashes, los arrays, las funciones y las clases.