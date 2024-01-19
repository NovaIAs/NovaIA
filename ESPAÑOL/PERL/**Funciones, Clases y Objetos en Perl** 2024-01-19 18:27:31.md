```perl
use strict;
use warnings;

# Definición de las funciones

sub Saludar {
    my ($nombre) = @_;
    print "Hola, $nombre!\n";
}

sub Despedir {
    my ($nombre) = @_;
    print "Adiós, $nombre!\n";
}

sub Sumar {
    my ($a, $b) = @_;
    return $a + $b;
}

sub Restar {
    my ($a, $b) = @_;
    return $a - $b;
}

sub Multiplicar {
    my ($a, $b) = @_;
    return $a * $b;
}

sub Dividir {
    my ($a, $b) = @_;
    return $a / $b;
}

# Uso de las funciones

Saludar("Juan");
Despedir("María");
print "La suma de 1 y 2 es ", Sumar(1, 2), "\n";
print "La resta de 3 y 4 es ", Restar(3, 4), "\n";
print "La multiplicación de 5 y 6 es ", Multiplicar(5, 6), "\n";
print "La división de 7 y 8 es ", Dividir(7, 8), "\n";

# Definición de la clase Persona

package Persona;

sub new {
    my ($class, %args) = @_;
    my $self = bless {
        nombre => $args{nombre},
        edad => $args{edad},
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

sub saludar {
    my ($self) = @_;
    print "Hola, mi nombre es ", $self->nombre, " y tengo ", $self->edad, " años.\n";
}

# Uso de la clase Persona

my $persona1 = Persona->new(nombre => "Juan", edad => 20);
my $persona2 = Persona->new(nombre => "María", edad => 25);

$persona1->saludar;
$persona2->saludar;
```

Explicación del código:

* El código define varias funciones, incluyendo `Saludar`, `Despedir`, `Sumar`, `Restar`, `Multiplicar` y `Dividir`. Estas funciones realizan operaciones básicas como saludar, despedir, sumar, restar, multiplicar y dividir dos números.
* El código también define una clase llamada `Persona` que tiene dos atributos: `nombre` y `edad`. La clase `Persona` tiene tres métodos: `new`, `nombre`, `edad` y `saludar`. El método `new` crea una nueva instancia de la clase `Persona`, el método `nombre` devuelve el nombre de la persona, el método `edad` devuelve la edad de la persona y el método `saludar` saluda a la persona.
* El código crea dos instancias de la clase `Persona` llamadas `persona1` y `persona2`. Luego, el código llama al método `saludar` de cada instancia para saludar a las personas.

Este código es complejo porque:

* Utiliza funciones y clases, que son conceptos avanzados en programación.
* Realiza cálculos matemáticos utilizando las funciones `Sumar`, `Restar`, `Multiplicar` y `Dividir`.
* Crea instancias de una clase y llama a los métodos de esas instancias.

Este código es útil porque:

* Puede utilizarse para crear programas que realicen cálculos matemáticos.
* Puede utilizarse para crear programas que gestionen objetos.

Es importante que el código esté bien escrito y comentado para que sea fácil de leer y entender.