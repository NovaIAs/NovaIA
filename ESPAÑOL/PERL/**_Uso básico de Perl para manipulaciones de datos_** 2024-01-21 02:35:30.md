```perl
use strict;
use warnings;

# Definir variables
my $nombre = "Juan Pérez";
my @edades = (20, 30, 40, 50);
my %ciudades = (
    "Madrid" => 3200000,
    "Barcelona" => 1600000,
    "Valencia" => 800000,
);

# Definir una función
sub saludar {
    my $nombre = shift;
    print "Hola, $nombre!\n";
}

# Llamar a la función
saludar($nombre);

# Iterar sobre un array
foreach my $edad (@edades) {
    print "Edad: $edad\n";
}

# Iterar sobre un hash
foreach my $ciudad (keys %ciudades) {
    print "Ciudad: $ciudad, Población: $ciudades{$ciudad}\n";
}

# Crear un objeto
my $persona = {
    nombre => "Juan Pérez",
    edad => 20,
    ciudad => "Madrid",
};

# Acceder a los atributos de un objeto
print "Nombre: $persona->{nombre}\n";
print "Edad: $persona->{edad}\n";
print "Ciudad: $persona->{ciudad}\n";

# Cambiar los atributos de un objeto
$persona->{ciudad} = "Barcelona";

# Eliminar un atributo de un objeto
delete $persona->{edad};

# Comprobar si un objeto tiene un atributo
if (exists $persona->{nombre}) {
    print "El objeto tiene el atributo 'nombre'\n";
} else {
    print "El objeto no tiene el atributo 'nombre'\n";
}
```

Explicación del código:

* Se definen tres variables: $nombre, @edades y %ciudades. $nombre es una variable escalar que contiene una cadena de caracteres, @edades es un array que contiene cuatro números enteros y %ciudades es un hash que contiene pares clave-valor, donde las claves son cadenas de caracteres y los valores son números enteros.
* Se define una función llamada saludar que recibe un argumento y lo imprime.
* Se llama a la función saludar pasándole como argumento $nombre.
* Se itera sobre el array @edades y se imprime cada elemento.
* Se itera sobre el hash %ciudades y se imprime cada clave y su valor asociado.
* Se crea un objeto llamado $persona que contiene tres atributos: nombre, edad y ciudad.
* Se accede a los atributos del objeto $persona y se imprimen sus valores.
* Se cambia el valor del atributo ciudad del objeto $persona.
* Se elimina el atributo edad del objeto $persona.
* Se comprueba si el objeto $persona tiene el atributo nombre.