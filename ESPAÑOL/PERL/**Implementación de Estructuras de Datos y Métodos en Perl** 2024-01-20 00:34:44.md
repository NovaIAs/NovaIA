```perl
#!/usr/bin/perl

use strict;
use warnings;

# Declaración de variables
my $nombre = "Juan";
my $apellido = "Pérez";
my $edad = 30;

# Imprimir el nombre, el apellido y la edad
print "Nombre: $nombre\n";
print "Apellido: $apellido\n";
print "Edad: $edad\n";

# Crear un array con los números del 1 al 10
my @numeros = (1..10);

# Imprimir el contenido del array
print "Números del 1 al 10: @numeros\n";

# Crear un hash con los días de la semana
my %dias_de_la_semana = (
    "Lunes" => 1,
    "Martes" => 2,
    "Miércoles" => 3,
    "Jueves" => 4,
    "Viernes" => 5,
    "Sábado" => 6,
    "Domingo" => 7
);

# Imprimir el contenido del hash
print "Días de la semana:\n";
foreach my $dia (sort keys %dias_de_la_semana) {
    print "$dia: $dias_de_la_semana{$dia}\n";
}

# Crear una subrutina para calcular el factorial de un número
sub factorial {
    my $numero = shift;

    if ($numero == 0) {
        return 1;
    } else {
        return $numero * factorial($numero - 1);
    }
}

# Imprimir el factorial de un número
print "Factorial de 5: " . factorial(5) . "\n";

# Crear una clase para representar a una persona
package Persona;

sub new {
    my $class = shift;
    my $self = {
        nombre => shift,
        apellido => shift,
        edad => shift
    };
    bless $self, $class;
    return $self;
}

sub nombre {
    my $self = shift;
    return $self->{nombre};
}

sub apellido {
    my $self = shift;
    return $self->{apellido};
}

sub edad {
    my $self = shift;
    return $self->{edad};
}

sub saludar {
    my $self = shift;
    print "Hola, mi nombre es " . $self->{nombre} . " " . $self->{apellido} . " y tengo " . $self->{edad} . " años.\n";
}

package main;

# Crear un objeto de la clase Persona
my $persona = Persona->new("Juan", "Pérez", 30);

# Imprimir el nombre, el apellido y la edad de la persona
print "Nombre: " . $persona->nombre() . "\n";
print "Apellido: " . $persona->apellido() . "\n";
print "Edad: " . $persona->edad() . "\n";

# Llamar al método saludar de la persona
$persona->saludar();
```

Explicación del código:

1. Declaración de variables: Se declaran las variables `$nombre`, `$apellido` y `$edad` para almacenar el nombre, el apellido y la edad de una persona, respectivamente.
2. Impresión del nombre, el apellido y la edad: Se imprimen el nombre, el apellido y la edad de la persona usando la función `print`.
3. Creación de un array: Se crea un array llamado `@numeros` que contiene los números del 1 al 10 usando el rango `(1..10)`.
4. Impresión del contenido del array: Se imprime el contenido del array `@numeros` usando la función `print`.
5. Creación de un hash: Se crea un hash llamado `%dias_de_la_semana` que contiene los días de la semana y sus números correspondientes.
6. Impresión del contenido del hash: Se imprime el contenido del hash `%dias_de_la_semana` iterando sobre sus claves ordenadas.
7. Creación de una subrutina: Se crea una subrutina llamada `factorial` que calcula el factorial de un número.
8. Impresión del factorial de un número: Se imprime el factorial del número 5 llamando a la subrutina `factorial`.
9. Creación de una clase: Se crea una clase llamada `Persona` que representa a una persona.
10. Implementación de métodos: Se implementan los métodos `nombre`, `apellido`, `edad` y `saludar` en la clase `Persona`.
11. Creación de un objeto: Se crea un objeto de la clase `Persona` llamado `$persona` usando el constructor `new` y se le pasan los argumentos "Juan", "Pérez" y 30 como nombre, apellido y edad, respectivamente.
12. Impresión del nombre, el apellido y la edad del objeto: Se imprimen el nombre, el apellido y la edad del objeto `$persona` llamando a los métodos correspondientes.
13. Llamada al método `saludar`: Se llama al método `saludar` del objeto `$persona` para imprimir un mensaje de saludo.