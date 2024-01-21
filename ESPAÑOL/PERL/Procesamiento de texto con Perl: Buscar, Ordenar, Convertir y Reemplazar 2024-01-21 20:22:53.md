```perl
use strict;
use warnings;

# Definir variables y constantes
my $texto = 'Este es un ejemplo de texto para procesar.';
my $patrón = 'ejemplo';
my @palabras = split / /, $texto;

# Buscar el patrón en el texto
my $resultado = index($texto, $patrón);
if ($resultado != -1) {
    print "El patrón $patrón se encontró en la posición $resultado.\n";
} else {
    print "El patrón $patrón no se encontró en el texto.\n";
}

# Recorrer las palabras del texto
foreach my $palabra (@palabras) {
    print "Palabra: $palabra\n";
}

# Ordenar las palabras del texto
@palabras = sort @palabras;
print "Palabras ordenadas: @palabras\n";

# Convertir el texto a mayúsculas
$texto = uc($texto);
print "Texto en mayúsculas: $texto\n";

# Convertir el texto a minúsculas
$texto = lc($texto);
print "Texto en minúsculas: $texto\n";

# Eliminar espacios en blanco del texto
$texto =~ s/\s+//g;
print "Texto sin espacios en blanco: $texto\n";

# Reemplazar todas las ocurrencias del patrón con otra cadena
$texto =~ s/$patrón/NUEVO_PATRÓN/g;
print "Texto con el patrón reemplazado: $texto\n";
```

Explicación del código:

* El código comienza importando los módulos `strict` y `warnings`. Estos módulos ayudan a detectar errores en el código y proporcionan advertencias sobre posibles problemas.
* A continuación, se definen las variables y constantes utilizadas en el código.
* La función `index` se utiliza para buscar el patrón en el texto. Si el patrón se encuentra, la función devuelve la posición en la que aparece. De lo contrario, devuelve -1.
* El bucle `foreach` se utiliza para recorrer las palabras del texto y mostrarlas en la consola.
* La función `sort` se utiliza para ordenar las palabras del texto en orden alfabético.
* Las funciones `uc` y `lc` se utilizan para convertir el texto a mayúsculas y minúsculas, respectivamente.
* La función `s///g` se utiliza para realizar operaciones de búsqueda y reemplazo en el texto. En este caso, se utiliza para eliminar espacios en blanco y reemplazar todas las ocurrencias del patrón con otra cadena.

Este código es un ejemplo complejo de cómo se pueden utilizar las funciones y operadores de Perl para procesar texto. Puede utilizarse como base para crear programas más complejos que procesen y manipulen datos de texto.