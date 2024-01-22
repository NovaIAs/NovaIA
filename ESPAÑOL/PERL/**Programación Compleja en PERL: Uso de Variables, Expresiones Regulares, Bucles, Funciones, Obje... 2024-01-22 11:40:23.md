```perl
use strict;
use warnings;

# Definición de variables
my $nombre = 'Juan';
my $edad = 25;
my $ciudad = 'Madrid';

# Uso de expresiones regulares
if ($nombre =~ /Juan/) {
    print "El nombre es Juan.\n";
}

# Uso de bucles
for ($i = 0; $i < 10; $i++) {
    print "$i\n";
}

# Uso de funciones
sub saludar {
    my $nombre = $_[0];
    print "Hola, $nombre!\n";
}

saludar('María');

# Uso de objetos
package Persona;

sub new {
    my $class = shift;
    my $self = {
        _nombre => shift,
        _edad => shift,
        _ciudad => shift,
    };
    bless $self, $class;
    return $self;
}

sub nombre {
    my $self = shift;
    return $self->{_nombre};
}

sub edad {
    my $self = shift;
    return $self->{_edad};
}

sub ciudad {
    my $self = shift;
    return $self->{_ciudad};
}

package main;

my $persona = Persona->new('Juan', 25, 'Madrid');

print $persona->nombre() . "\n";
print $persona->edad() . "\n";
print $persona->ciudad() . "\n";

# Uso de módulos
use Math::Trig;

my $resultado = sin(45);

print $resultado . "\n";
```

Este código es un ejemplo de código complejo en PERL que incluye el uso de variables, expresiones regulares, bucles, funciones, objetos y módulos. El código está organizado en paquetes y utiliza la sintaxis orientada a objetos de PERL. El código crea un objeto de la clase Persona y lo utiliza para mostrar el nombre, la edad y la ciudad de la persona. El código también utiliza el módulo Math::Trig para calcular el seno de 45 grados.