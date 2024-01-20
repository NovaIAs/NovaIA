```perl
use strict;
use warnings;
use utf8;

# Declaración de variables
my $nombre = 'Juan';
my $apellido = 'Pérez';
my $edad = 30;
my @idiomas = qw/Español Inglés Francés/;
my %países = (
    'España' => 'Madrid',
    'Estados Unidos' => 'Washington D.C.',
    'Francia' => 'París',
);

# Funciones

# Función para saludar
sub saludar {
    my $nombre = shift;
    print "Hola, $nombre!\n";
}

# Función para despedirse
sub despedirse {
    my $nombre = shift;
    print "Adiós, $nombre!\n";
}

# Función para mostrar la información de una persona
sub mostrar_información {
    my $nombre = shift;
    my $apellido = shift;
    my $edad = shift;
    my @idiomas = shift;

    print "Nombre: $nombre\n";
    print "Apellido: $apellido\n";
    print "Edad: $edad\n";
    print "Idiomas:\n";
    foreach my $idioma (@idiomas) {
        print " - $idioma\n";
    }
}

# Función para mostrar un mapa de países
sub mostrar_mapa {
    my %países = shift;

    print "Mapa de países:\n";
    foreach my $país (keys %países) {
        my $capital = $países{$país};
        print " - $país: $capital\n";
    }
}

# Programa principal

# Saludo
saludar($nombre);

# Despedida
despedirse($nombre);

# Información personal
mostrar_información($nombre, $apellido, $edad, @idiomas);

# Mapa de países
mostrar_mapa(%países);
```

Este código en PERL realiza las siguientes acciones:

* Declara variables para almacenar el nombre, apellido, edad, idiomas y países de una persona.
* Define funciones para saludar, despedirse, mostrar la información de una persona y mostrar un mapa de países.
* Ejecuta el programa principal, que llama a las funciones definidas anteriormente para saludar, mostrar la información personal y mostrar un mapa de países.