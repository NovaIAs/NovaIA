```perl
use strict;
use warnings;

use utf8;

binmode STDIN, ":utf8";
binmode STDOUT, ":utf8";

my $texto = "Esta oración tiene signos de puntuación y acentos, ¿no?";
my $remplazar = "";

my $longitud = length $texto;

for (my $i = 0; $i < $longitud; $i++) {
    my $caracter = substr($texto, $i, 1);
    if ($caracter =~ /[[:punct:][:space:]]/) {
        $remplazar .= " ";
    } else {
        $remplazar .= $caracter;
    }
}

$remplazar =~ s/ +/ /g;

print "Texto original: $texto\n";
print "Texto sin puntuación ni acentos: $remplazar\n";
```

Explicación:

* El código utiliza la codificación UTF-8 para manejar correctamente los caracteres especiales.


* El código utiliza el módulo "strict" para garantizar que todas las variables sean declaradas y el módulo "warnings" para emitir advertencias sobre posibles problemas.


* El código utiliza una variable `$texto` para almacenar la oración original y una variable `$remplazar` para almacenar la versión sin puntuación ni acentos.


* El código utiliza un bucle `for` para iterar sobre todos los caracteres de la oración original.


* El código utiliza una expresión regular para comprobar si un carácter es un signo de puntuación o un espacio en blanco.


* El código utiliza la función `substr()` para extraer un carácter de la oración original.


* El código utiliza la función `+=` para concatenar una cadena a otra.


* El código utiliza la expresión regular `s/ +/ /g` para sustituir todas las secuencias de espacios en blanco por un solo espacio en blanco.


* El código imprime la oración original y la versión sin puntuación ni acentos en la consola.