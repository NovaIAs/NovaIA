```perl
use utf8;
use strict;
use warnings;

# Declaración de variables
my $nombre = "Juan Pérez";
my $edad = 30;
my $estatura = 1.80;
my @hobbies = ("Leer", "Escribir", "Viajar");
my %contactos = (
    "correo" => "juan.perez@correo.com",
    "teléfono" => "123-456-7890",
    "dirección" => "Calle 123, Ciudad ABC",
);

# Convertir la edad a una cadena de caracteres
my $edad_str = sprintf("%d", $edad);

# Imprimir la información en la consola
print "Nombre: $nombre\n";
print "Edad: $edad_str años\n";
print "Estatura: $estatura metros\n";
print "Hobbies:\n";
foreach my $hobby (@hobbies) {
    print " - $hobby\n";
}
print "Contactos:\n";
foreach my $tipo (keys %contactos) {
    print " - $tipo: $contactos{$tipo}\n";
}

# Función para calcular el área de un círculo
sub area_circulo {
    my $radio = $_[0];
    my $area = pi * $radio ** 2;
    return $area;
}

# Calcular el área de un círculo con radio 5
my $area_circulo = area_circulo(5);

# Imprimir el área del círculo
print "Área del círculo: $area_circulo\n";

# Regular expression para validar una dirección de correo electrónico
my $regex_correo = qr/^([a-zA-Z0-9_\-\.]+)@([a-zA-Z0-9_\-\.]+)\.([a-zA-Z]{2,5})$/;

# Validar la dirección de correo electrónico del usuario
if ($regex_correo->=~ $contactos{"correo"}) {
    print "La dirección de correo electrónico es válida\n";
} else {
    print "La dirección de correo electrónico no es válida\n";
}
```

Explicación del código:

* Se declaran una serie de variables con diferentes tipos de datos, incluyendo cadenas de caracteres, números, arrays y hashes.
* Se utiliza la función `sprintf()` para convertir la edad a una cadena de caracteres.
* Se utiliza el bucle `foreach` para recorrer el array de hobbies y el hash de contactos, y se imprime la información en la consola.
* Se define una función llamada `area_circulo()` que calcula el área de un círculo a partir de su radio.
* Se calcula el área de un círculo con radio 5 y se imprime el resultado.
* Se define una expresión regular para validar una dirección de correo electrónico.
* Se valida la dirección de correo electrónico del usuario y se imprime un mensaje indicando si es válida o no.