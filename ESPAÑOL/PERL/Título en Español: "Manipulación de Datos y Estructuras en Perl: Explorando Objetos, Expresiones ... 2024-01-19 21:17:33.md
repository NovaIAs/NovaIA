```perl
use strict;
use warnings;

# Declaración de variables
my $nombre = "Juan";
my $apellido = "Pérez";
my $edad = 30;
my @hobbies = ("Leer", "Escribir", "Viajar");
my %contactos = (
    "Teléfono" => "123-456-7890",
    "Correo electrónico" => "juan.perez@ejemplo.com",
);

# Creación de un objeto persona
my $persona = {
    nombre => $nombre,
    apellido => $apellido,
    edad => $edad,
    hobbies => \@hobbies,
    contactos => \%contactos,
};

# Impresión de la información de la persona
print "Nombre: $persona->{nombre}\n";
print "Apellido: $persona->{apellido}\n";
print "Edad: $persona->{edad}\n";
print "Hobbies:\n";
foreach my $hobby (@{$persona->{hobbies}}) {
    print "  * $hobby\n";
}
print "Contactos:\n";
foreach my $contacto (keys %{$persona->{contactos}}) {
    print "  $contacto: $persona->{contactos}{$contacto}\n";
}

# Uso de una expresión regular para buscar una palabra en una cadena
my $cadena = "El perro es un animal doméstico";
my $palabra = "perro";
if ($cadena =~ /$palabra/) {
    print "La palabra \"$palabra\" se encuentra en la cadena.\n";
} else {
    print "La palabra \"$palabra\" no se encuentra en la cadena.\n";
}

# Uso de una función para calcular el factorial de un número
sub factorial {
    my $numero = shift;
    my $resultado = 1;
    for (my $i = 2; $i <= $numero; $i++) {
        $resultado *= $i;
    }
    return $resultado;
}

my $numero = 5;
my $factorial = factorial($numero);
print "El factorial de $numero es $factorial.\n";

# Uso de un módulo para acceder a la información del sistema
use Sys::Hostname;

my $hostname = hostname();
print "El nombre del host es $hostname.\n";
```

Explicación del código:

* El código crea un objeto persona con los atributos nombre, apellido, edad, hobbies y contactos.
* Luego imprime la información de la persona en la consola.
* Utiliza una expresión regular para buscar una palabra en una cadena.
* Define una función para calcular el factorial de un número.
* Utiliza un módulo para acceder a la información del sistema, en este caso el nombre del host.