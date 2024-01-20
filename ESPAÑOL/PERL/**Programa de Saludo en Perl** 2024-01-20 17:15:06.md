```perl
use strict;
use warnings;

# Declaración de variables
my $nombre = "Juan";
my $edad = 25;
my @hobbies = ("Leer", "Viajar", "Programar");
my %contactos = (
    "correo" => "juan@ejemplo.com",
    "teléfono" => "1234567890",
    "dirección" => "Calle Principal 123",
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

    # Imprimir la edad
    print "Edad: $edad\n";

    # Imprimir los hobbies
    print "Hobbies: ";
    foreach my $hobby (@hobbies) {
        print "$hobby, ";
    }
    print "\n";

    # Imprimir los contactos
    print "Contactos:\n";
    foreach my $contacto (keys %contactos) {
        print "$contacto: $contactos{$contacto}\n";
    }

    # Llamar a la función despedirse
    despedirse($nombre);
}

# Llamar a la función principal
main();
```

Explicación del código:

* La primera línea `use strict;` indica que se debe usar el modo estricto de Perl, que ayuda a detectar errores de programación comunes.
* La segunda línea `use warnings;` indica que se deben mostrar las advertencias de Perl, que pueden ayudar a identificar problemas potenciales en el código.
* Las siguientes líneas declaran las variables que se utilizarán en el programa.
* La subrutina `saludar` se utiliza para saludar a una persona. Toma un argumento, que es el nombre de la persona a saludar.
* La subrutina `despedirse` se utiliza para despedirse de una persona. Toma un argumento, que es el nombre de la persona a despedirse.
* La subrutina `main` es la función principal del programa. Llama a las subrutinas `saludar` y `despedirse`, e imprime la edad, los hobbies y los contactos de la persona.
* La última línea del programa llama a la subrutina `main`.