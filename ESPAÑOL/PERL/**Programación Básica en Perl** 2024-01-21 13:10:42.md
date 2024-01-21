```perl
use strict;
use warnings;
use utf8;

# Definición de variables
my $nombre = "Juan";
my $edad = 30;
my @hobbies = ("leer", "escribir", "viajar");
my %mascotas = ("perro" => "Toby", "gato" => "Misi");

# Función para saludar
sub saludar {
    my $nombre = shift;
    print "Hola, mi nombre es $nombre.\n";
}

# Función para sumar dos números
sub sumar {
    my $a = shift;
    my $b = shift;
    return $a + $b;
}

# Función para imprimir una lista de elementos
sub imprimir_lista {
    my @lista = @_;
    foreach my $elemento (@lista) {
        print "$elemento\n";
    }
}

# Función para imprimir un diccionario de elementos
sub imprimir_diccionario {
    my %diccionario = %_;
    foreach my $clave (keys %diccionario) {
        print "$clave: $diccionario{$clave}\n";
    }
}

# Llamada a las funciones
saludar($nombre);
my $resultado = sumar(10, 20);
print "El resultado de la suma es $resultado.\n";
imprimir_lista(@hobbies);
imprimir_diccionario(%mascotas);
```

Explicación del código:

* La primera línea `use strict;` indica al compilador que se debe utilizar el modo estricto, lo que significa que se deben seguir ciertas reglas adicionales para evitar errores comunes.
* La segunda línea `use warnings;` indica al compilador que se deben mostrar advertencias sobre posibles problemas en el código.
* La tercera línea `use utf8;` indica al compilador que se debe utilizar la codificación UTF-8, que es una codificación de caracteres que permite representar una amplia gama de caracteres, incluidos caracteres no ASCII.
* Las siguientes líneas definen variables. La variable `$nombre` es una variable escalar que contiene el nombre de una persona. La variable `$edad` es una variable escalar que contiene la edad de una persona. La variable `@hobbies` es un array que contiene una lista de hobbies. La variable `%mascotas` es un diccionario que contiene una lista de mascotas y sus nombres.
* Las siguientes líneas definen funciones. La función `saludar` saluda a una persona por su nombre. La función `sumar` suma dos números y devuelve el resultado. La función `imprimir_lista` imprime una lista de elementos. La función `imprimir_diccionario` imprime un diccionario de elementos.
* Las últimas líneas del código llaman a las funciones. La función `saludar` se llama con el argumento `$nombre`. La función `sumar` se llama con los argumentos `10` y `20`. El resultado de la llamada a la función `sumar` se imprime en la pantalla. La función `imprimir_lista` se llama con el argumento `@hobbies`. La función `imprimir_diccionario` se llama con el argumento `%mascotas`.