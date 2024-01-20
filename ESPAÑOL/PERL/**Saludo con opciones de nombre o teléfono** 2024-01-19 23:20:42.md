```perl
use strict;
use warnings;
use utf8;
use feature 'say';

# Definimos una lista de nombres de personas
my @nombres = qw(Juan Pedro Ana María José Lucía Manuel Sofía Antonio Elena);

# Definimos un mapa de números de teléfono a nombres de personas
my %telefonos = (
    '123456789' => 'Juan',
    '987654321' => 'Pedro',
    '1234567890' => 'Ana',
    '9876543210' => 'María',
);

# Definimos una función para generar un mensaje de bienvenida
sub generar_bienvenida {
    my ($nombre) = @_;
    return "Hola, $nombre. Bienvenido a nuestro sistema.";
}

# Definimos una función para generar un mensaje de despedida
sub generar_despedida {
    my ($nombre) = @_;
    return "Gracias por usar nuestro sistema, $nombre. ¡Hasta la próxima!";
}

# Pedimos al usuario que introduzca su nombre
print "Por favor, introduzca su nombre: ";
my $nombre = <STDIN>;
chomp $nombre;

# Comprobamos si el nombre del usuario está en la lista de nombres de personas
if (grep { $_ eq $nombre } @nombres) {
    # Si el nombre del usuario está en la lista, generamos un mensaje de bienvenida
    my $mensaje_bienvenida = generar_bienvenida($nombre);
    say $mensaje_bienvenida;
} else {
    # Si el nombre del usuario no está en la lista, pedimos al usuario que introduzca su número de teléfono
    print "Lo siento, no reconozco ese nombre. Por favor, introduzca su número de teléfono: ";
    my $telefono = <STDIN>;
    chomp $telefono;

    # Comprobamos si el número de teléfono del usuario está en el mapa de números de teléfono a nombres de personas
    if (exists $telefonos{$telefono}) {
        # Si el número de teléfono del usuario está en el mapa, generamos un mensaje de bienvenida
        my $nombre_usuario = $telefonos{$telefono};
        my $mensaje_bienvenida = generar_bienvenida($nombre_usuario);
        say $mensaje_bienvenida;
    } else {
        # Si el número de teléfono del usuario no está en el mapa, generamos un mensaje de error
        say "Lo siento, no reconozco ese número de teléfono.";
    }
}

# Generamos un mensaje de despedida
my $mensaje_despedida = generar_despedida($nombre);
say $mensaje_despedida;
```

Este código es un ejemplo de cómo se puede utilizar el lenguaje de programación Perl para crear una aplicación sencilla que interactúa con el usuario. La aplicación pide al usuario que introduzca su nombre o su número de teléfono, y genera un mensaje de bienvenida o un mensaje de error en función de la información proporcionada por el usuario.

El código está dividido en varias funciones, cada una de las cuales tiene una responsabilidad específica. La función `generar_bienvenida()` genera un mensaje de bienvenida para el usuario, la función `generar_despedida()` genera un mensaje de despedida para el usuario, y la función `grep()` comprueba si un elemento de una lista coincide con un determinado patrón.

El código también utiliza un mapa, que es una colección de pares clave-valor. En este caso, el mapa `%telefonos` almacena los números de teléfono de los usuarios como claves y sus nombres como valores.

El código utiliza una serie de construcciones de control, como `if` y `else`, para determinar qué código debe ejecutarse en función de las condiciones determinadas.

Este código es sólo un ejemplo de lo que se puede hacer con Perl. Perl es un lenguaje de programación muy versátil que puede utilizarse para crear una amplia variedad de aplicaciones.