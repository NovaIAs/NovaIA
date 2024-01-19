```
use utf8;
use strict;
use warnings;

# Definición de variables
my $nombre = "Juan";
my $edad = 30;
my @hobbies = ("Leer", "Viajar", "Cocinar");

# Función para saludar
sub saludar {
    my $nombre = $_[0];
    print "Hola, $nombre!\n";
}

# Función para imprimir la edad
sub imprimir_edad {
    my $edad = $_[0];
    print "Tu edad es $edad años.\n";
}

# Función para imprimir los hobbies
sub imprimir_hobbies {
    my @hobbies = @_[0];
    print "Tus hobbies son:\n";
    foreach my $hobby (@hobbies) {
        print "  * $hobby\n";
    }
}

# Llamada a las funciones
saludar($nombre);
imprimir_edad($edad);
imprimir_hobbies(@hobbies);
```

Explicación del código:

* La primera línea, `use utf8;`, habilita el uso de caracteres Unicode en el código.
* La segunda línea, `use strict;`, activa el modo estricto de Perl, que comprueba la sintaxis y la semántica del código.
* La tercera línea, `use warnings;`, activa las advertencias, que le avisan de posibles problemas en el código.

* La siguiente línea define una variable escalar, `$nombre`, y le asigna el valor "Juan".
* La siguiente línea define una variable escalar, `$edad`, y le asigna el valor 30.
* La siguiente línea define una matriz, `@hobbies`, y le asigna los valores "Leer", "Viajar" y "Cocinar".

* La siguiente línea define una subrutina, `saludar`, que toma un parámetro, `$nombre`, e imprime el mensaje "Hola, $nombre!\n".
* La siguiente línea define una subrutina, `imprimir_edad`, que toma un parámetro, `$edad`, e imprime el mensaje "Tu edad es $edad años.\n".
* La siguiente línea define una subrutina, `imprimir_hobbies`, que toma un parámetro, `@hobbies`, e imprime una lista de los valores de la matriz.

* La siguiente línea llama a la subrutina `saludar` y le pasa el valor de la variable `$nombre` como argumento.
* La siguiente línea llama a la subrutina `imprimir_edad` y le pasa el valor de la variable `$edad` como argumento.
* La siguiente línea llama a la subrutina `imprimir_hobbies` y le pasa el valor de la matriz `@hobbies` como argumento.

El código imprimirá el siguiente resultado:

```
Hola, Juan!
Tu edad es 30 años.
Tus hobbies son:
  * Leer
  * Viajar
  * Cocinar
```