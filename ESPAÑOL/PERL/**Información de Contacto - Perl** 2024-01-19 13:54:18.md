```perl
use strict;
use warnings;

# Definir variables
my $nombre = "Juan";
my $apellido = "Pérez";
my $edad = 30;

# Definir una lista de hobbies
my @hobbies = ("Leer", "Escribir", "Viajar");

# Definir un hash de contactos
my %contactos = (
    "casa" => "123-456-7890",
    "móvil" => "987-654-3210",
    "correo electrónico" => "juanperez@ejemplo.es",
);

# Mostrar la información de la persona
print "Nombre: $nombre\n";
print "Apellido: $apellido\n";
print "Edad: $edad\n";

# Mostrar la lista de hobbies
print "Hobbies:\n";
foreach my $hobby (@hobbies) {
    print "  $hobby\n";
}

# Mostrar el hash de contactos
print "Contactos:\n";
foreach my $tipo_contacto (keys %contactos) {
    print "  $tipo_contacto: $contactos{$tipo_contacto}\n";
}
```

Este código crea una variable para cada uno de los datos de la persona: nombre, apellido, edad y hobbies. También crea un hash para los contactos, con claves para cada tipo de contacto (casa, móvil y correo electrónico) y valores para los números de teléfono y la dirección de correo electrónico.

Luego, el código muestra la información de la persona, la lista de hobbies y el hash de contactos usando bucles foreach.

Este código es complejo porque utiliza varias estructuras de datos (variables, listas y hashes) y bucles para mostrar la información. También utiliza el operador foreach para iterar sobre las listas y los hashes.

El código también está bien documentado, con comentarios que explican lo que hace cada parte del código. Esto hace que el código sea más fácil de entender y mantener.