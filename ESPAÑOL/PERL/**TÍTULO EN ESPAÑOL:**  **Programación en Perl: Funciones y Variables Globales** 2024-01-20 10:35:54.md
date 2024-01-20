```perl
use strict;
use warnings;

# Definir variables globales
my $nombre = 'Juan Pérez';
my $edad = 30;
my @hobbies = ('Leer', 'Escribir', 'Viajar');

# Crear una función para saludar a alguien
sub saludar {
    my $persona = shift;
    print "Hola, $persona!\n";
}

# Crear una función para calcular el área de un rectángulo
sub area_rectangulo {
    my ($largo, $ancho) = @_;
    return $largo * $ancho;
}

# Crear una función para imprimir un array
sub imprimir_array {
    my $array = shift;
    foreach my $elemento (@$array) {
        print "$elemento\n";
    }
}

# Llamar a las funciones
saludar($nombre);
my $area = area_rectangulo(10, 20);
print "El área del rectángulo es $area\n";
imprimir_array(@hobbies);
```

Explicación:

* El código comienza importando los módulos `strict` y `warnings`, que ayudan a detectar errores en el código.
* Se definen tres variables globales: una cadena de caracteres (`nombre`), un número entero (`edad`) y un array de cadenas de caracteres (`hobbies`).
* Se crean tres funciones: `saludar()`, `area_rectangulo()` e `imprimir_array()`.
* La función `saludar()` recibe un argumento, el nombre de la persona a quien saludar, y lo imprime en pantalla.
* La función `area_rectangulo()` recibe dos argumentos, el largo y el ancho del rectángulo, y devuelve el área del rectángulo.
* La función `imprimir_array()` recibe un argumento, un array, y lo imprime en pantalla, un elemento por línea.
* Finalmente, se llaman a las funciones para probarlas. Se saluda a la persona cuyo nombre está almacenado en la variable `nombre`, se calcula el área de un rectángulo con un largo de 10 y un ancho de 20, y se imprime el array de hobbies almacenado en la variable `hobbies`.