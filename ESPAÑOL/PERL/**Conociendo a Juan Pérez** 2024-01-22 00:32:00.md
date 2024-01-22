```perl
use strict;
use warnings;

# Definición de variables
my $nombre = 'Juan Pérez';
my $edad = 25;
my @amigos = ('María', 'José', 'Pedro');
my %familiares = (
    'padre' => 'José Pérez',
    'madre' => 'María López',
    'hermano' => 'Carlos Pérez',
);

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

# Función principal
sub main {
    # Llamar a la función saludar
    saludar($nombre);

    # Mostrar la edad
    print "Tu edad es $edad años.\n";

    # Iterar sobre el array de amigos
    foreach my $amigo (@amigos) {
        print "Uno de tus amigos es $amigo.\n";
    }

    # Iterar sobre el hash de familiares
    foreach my $relacion (keys %familiares) {
        my $familiar = $familiares{$relacion};
        print "Tu $relacion es $familiar.\n";
    }

    # Llamar a la función despedirse
    despedirse($nombre);
}

# Ejecutar la función principal
main();
```

Explicación del código:

* La primera línea define el uso de las directivas `strict` y `warnings`, que ayudan a detectar y evitar errores en el código.
* A continuación se definen las variables utilizadas en el código, incluyendo un nombre, una edad, un array de amigos y un hash de familiares.
* Se definen dos funciones, `saludar` y `despedirse`, que reciben un nombre como argumento y muestran un mensaje de saludo o despedida, respectivamente.
* La función `main` es la función principal del programa, y es la que se ejecuta cuando se ejecuta el script.
* En la función `main`, se llaman a las funciones `saludar` y `despedirse`, y se muestran la edad, la lista de amigos y la lista de familiares.
* Por último, se ejecuta la función `main` llamando a `main()`.