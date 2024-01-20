```perl
use strict;
use warnings;

# Definir variables
my $nombre = "Juan";
my $edad = 30;
my @hobbies = ("Leer", "Escribir", "Viajar");
my %contactos = (
    "casa" => "123-456-7890",
    "móvil" => "987-654-3210",
    "fax" => "555-1212"
);

# Utilizar condicionales para realizar una acción
if ($edad >= 18) {
    print "Eres mayor de edad.\n";
} else {
    print "Eres menor de edad.\n";
}

# Utilizar bucles para iterar sobre colecciones
foreach (@hobbies) {
    print "Un hobby de $nombre es $_.\n";
}

# Utilizar expresiones regulares para buscar y reemplazar texto
my $texto = "El día de hoy es el 5 de mayo de 2023";
$texto =~ s/5 de mayo/Cinco de Mayo/g;
print "$texto\n";

# Crear una función para reutilizar código
sub saludar {
    my $nombre = shift;
    print "Hola, $nombre!\n";
}

saludar("María");

# Utilizar módulos para extender la funcionalidad del programa
use Math::Trig;
my $angulo = 30;
my $seno = sin($angulo);
print "El seno de $angulo es $seno.\n";

# Utilizar orientación a objetos para crear objetos personalizados
package Persona;

sub new {
    my $class = shift;
    my $self = {
        nombre => shift,
        edad => shift
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

package main;

my $persona = Persona->new("Ana", 25);
print "El nombre de la persona es $persona->nombre.\n";
print "La edad de la persona es $persona->edad.\n";
```

Explicación:

* El código comienza definiendo variables para almacenar datos simples.
* A continuación, utiliza condicionales para tomar decisiones en función de los valores de las variables.
* También utiliza bucles para iterar sobre colecciones de datos.
* Emplea expresiones regulares para buscar y reemplazar texto.
* Crea una función para reutilizar código.
* Importa un módulo para extender la funcionalidad del programa.
* Utiliza orientación a objetos para crear objetos personalizados.

Este código es bastante complejo y contiene una variedad de características diferentes de Perl. Es un buen ejemplo de cómo se puede utilizar Perl para resolver problemas del mundo real.