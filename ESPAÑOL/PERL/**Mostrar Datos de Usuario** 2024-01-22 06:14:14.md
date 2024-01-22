```perl
use strict;
use warnings;

# Definimos un hash para almacenar los datos de los usuarios
my %usuarios = (
    "juan" => {
        nombre => "Juan García",
        edad => 30,
        ciudad => "Madrid"
    },
    "maría" => {
        nombre => "María Sánchez",
        edad => 25,
        ciudad => "Barcelona"
    },
    "pedro" => {
        nombre => "Pedro López",
        edad => 40,
        ciudad => "Valencia"
    }
);

# Definimos una subrutina para mostrar los datos de un usuario
sub mostrar_usuario {
    my ($nombre) = @_;

    # Comprobamos si el usuario existe
    if (exists $usuarios{$nombre}) {
        # Obtenemos los datos del usuario
        my $usuario = $usuarios{$nombre};

        # Mostramos los datos del usuario
        print "Nombre: $usuario->{nombre}\n";
        print "Edad: $usuario->{edad}\n";
        print "Ciudad: $usuario->{ciudad}\n\n";
    } else {
        # El usuario no existe
        print "El usuario $nombre no existe.\n\n";
    }
}

# Pedimos al usuario que introduzca un nombre
print "Introduce el nombre de un usuario: ";
my $nombre = <STDIN>;

# Eliminamos los espacios en blanco del principio y del final del nombre
$nombre =~ s/^\s+|\s+$//g;

# Mostramos los datos del usuario
mostrar_usuario($nombre);
```

Explicación:

* El código define un hash llamado `%usuarios` para almacenar los datos de los usuarios.
* Define una subrutina llamada `mostrar_usuario()` que toma un nombre de usuario como argumento y muestra los datos del usuario.
* Pide al usuario que introduzca un nombre de usuario.
* Elimina los espacios en blanco del principio y del final del nombre de usuario.
* Llama a la subrutina `mostrar_usuario()` con el nombre de usuario introducido por el usuario.