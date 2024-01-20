```perl
use strict;
use warnings;
use utf8;

# Definición de variables
my $nombre = "Juan";
my $edad = 30;
my @hobbies = ("Leer", "Viajar", "Programar");
my %contactos = (
    "email" => "juan@example.com",
    "telefono" => "123-456-7890",
    "direccion" => "Calle 123, Ciudad XYZ"
);

# Función para imprimir la información de una persona
sub imprimir_info {
    my $persona = $_[0];

    print "Nombre: $persona->{nombre}\n";
    print "Edad: $persona->{edad}\n";
    print "Hobbies:\n";
    foreach my $hobby (@{$persona->{hobbies}}) {
        print " - $hobby\n";
    }
    print "Contactos:\n";
    foreach my $tipo (keys %{$persona->{contactos}}) {
        print " - $tipo: $persona->{contactos}{$tipo}\n";
    }
}

# Creación de una lista de personas
my @personas = (
    {
        nombre => "Juan",
        edad => 30,
        hobbies => ["Leer", "Viajar", "Programar"],
        contactos => {
            email => "juan@example.com",
            telefono => "123-456-7890",
            direccion => "Calle 123, Ciudad XYZ"
        }
    },
    {
        nombre => "María",
        edad => 25,
        hobbies => ["Cocinar", "Bailar", "Cantar"],
        contactos => {
            email => "maria@example.com",
            telefono => "456-789-0123",
            direccion => "Calle 456, Ciudad ABC"
        }
    },
    {
        nombre => "Pedro",
        edad => 40,
        hobbies => ["Deportes", "Música", "Fotografía"],
        contactos => {
            email => "pedro@example.com",
            telefono => "789-012-3456",
            direccion => "Calle 789, Ciudad DEF"
        }
    }
);

# Recorrido de la lista de personas e impresión de su información
foreach my $persona (@personas) {
    imprimir_info($persona);
    print "\n";
}
```

Explicación del código:

* Se definen las variables necesarias para almacenar la información de las personas: `$nombre`, `$edad`, `@hobbies` y `%contactos`.
* Se define una función `imprimir_info` que recibe un argumento que es un hash con la información de una persona e imprime dicha información en la consola.
* Se crea una lista de personas, cada una de las cuales es un hash con las claves `nombre`, `edad`, `hobbies` y `contactos`.
* Se recorre la lista de personas y se imprime la información de cada una de ellas llamando a la función `imprimir_info`.