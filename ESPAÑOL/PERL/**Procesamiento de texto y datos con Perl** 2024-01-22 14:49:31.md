```perl
#!/usr/bin/perl

use strict;
use warnings;

# Definir los datos de entrada

my $texto = 'Esto es un ejemplo de texto';
my $patrón = 'un';

# Buscar el patrón en el texto usando expresiones regulares

my $coincidencia = $texto =~ /$patrón/;

# Imprimir el resultado

if ($coincidencia) {
  print "El patrón '$patrón' se encontró en el texto.\n";
} else {
  print "El patrón '$patrón' no se encontró en el texto.\n";
}

# Crear una lista de números

my @numeros = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

# Iterar sobre la lista de números usando un foreach

foreach my $número (@numeros) {
  print "$número ";
}

print "\n";

# Crear un hash de datos

my %datos = (
  'nombre' => 'Juan',
  'apellido' => 'Pérez',
  'edad' => 30,
);

# Imprimir los datos del hash usando un foreach

foreach my $clave (keys %datos) {
  print "$clave: $datos{$clave}\n";
}

# Crear una referencia a una función

my $función = \&saludar;

# Llamar a la función a través de la referencia

$función->();

sub saludar {
  print "Hola, mundo!\n";
}
```

Este código Perl es un ejemplo de cómo realizar diversas tareas comunes en el lenguaje. El código se explica a continuación:

* **Uso de expresiones regulares:** El código utiliza la función `=~` para buscar el patrón "un" en el texto. La función `=~` utiliza expresiones regulares para realizar la búsqueda.
* **Uso de foreach:** El código utiliza la instrucción `foreach` para iterar sobre una lista de números y un hash de datos. La instrucción `foreach` es una forma sencilla de iterar sobre una colección de datos.
* **Uso de referencias:** El código utiliza una referencia a una función para llamar a la función `saludar`. Las referencias son una forma de almacenar una referencia a un valor o una función.
* **Definición de una subrutina:** El código define una subrutina llamada `saludar`. La subrutina `saludar` imprime el mensaje "Hola, mundo!".