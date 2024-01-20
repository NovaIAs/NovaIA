```perl
use strict;
use utf8;

# Definición de variables.
my $nombre = "Juan";
my $edad = 30;

# Definición de una estructura.
my %persona = (
    nombre => $nombre,
    edad => $edad
);

# Definición de una lista.
my @lista = (1, 2, 3, 4, 5);

# Definición de una función.
sub saludar {
    my $nombre = shift;
    print "¡Hola, $nombre!\n";
}

# Uso de la función saludar.
saludar($nombre);

# Uso de la estructura persona.
print "Nombre: $persona{nombre}\n";
print "Edad: $persona{edad}\n";

# Uso de la lista.
foreach my $numero (@lista) {
    print "$numero\n";
}

# Definición de un hash.
my %hash = (
    "uno" => 1,
    "dos" => 2,
    "tres" => 3
);

# Uso del hash.
print "El valor de 'uno' es: $hash{'uno'}\n";

# Definición de una expresión regular.
my $regex = qr/[0-9]+/;

# Uso de la expresión regular.
if ($regex =~ "12345") {
    print "La cadena contiene sólo dígitos.\n";
}

# Definición de una referencia.
my $referencia = \$nombre;

# Uso de la referencia.
print "El valor de la referencia es: $$referencia\n";

# Definición de un paquete.
package MiPaquete;

sub saludar {
    print "¡Hola, mundo!\n";
}

# Uso del paquete MiPaquete.
use MiPaquete;

MiPaquete::saludar();

# Fin del código.
```

Este código es un ejemplo de un código complejo en Perl. El código utiliza muchas de las características del lenguaje, incluyendo variables, estructuras, listas, funciones, hashes, expresiones regulares, referencias y paquetes. El código también incluye un ejemplo de cómo usar el módulo "use" para importar funciones y variables de otros módulos.

El código es el siguiente:

* Se definen varias variables, incluyendo una variable escalar ($nombre), una variable escalar con un valor numérico ($edad), una variable de tipo estructura (%persona), una variable de tipo lista (@lista), una variable de tipo función (sub saludar) y una variable de tipo hash (%hash).
* Se utiliza la función saludar para imprimir un mensaje de saludo.
* Se utiliza la estructura persona para imprimir el nombre y la edad de la persona.
* Se utiliza la lista para imprimir los números del 1 al 5.
* Se utiliza el hash para imprimir el valor de la clave "uno".
* Se define una expresión regular para comprobar si una cadena contiene sólo dígitos.
* Se utiliza la expresión regular para comprobar si la cadena "12345" contiene sólo dígitos.
* Se define una referencia a la variable $nombre.
* Se utiliza la referencia para imprimir el valor de la variable $nombre.
* Se define un paquete llamado MiPaquete.
* Se define una función saludar en el paquete MiPaquete.
* Se utiliza el módulo "use" para importar la función saludar del paquete MiPaquete.
* Se llama a la función saludar del paquete MiPaquete.

Este código es un ejemplo de cómo utilizar muchas de las características del lenguaje Perl. El código es complejo, pero también es muy potente y flexible.