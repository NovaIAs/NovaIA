```perl
use strict;
use warnings;

use utf8;
use open qw( :std :utf8 );

use feature 'say';

my @frases = (
    "La lluvia cae sobre el techo.",
    "El sol brilla en el cielo.",
    "El viento sopla entre los árboles.",
    "Las olas rompen en la orilla.",
    "Los pájaros cantan en las ramas.",
);

my @palabras = grep { length > 5 } map { split /\W+/ } @frases;

my %frecuencias;

foreach my $palabra (@palabras) {
    $frecuencias{$palabra}++;
}

my @palabras_ordenadas = sort { $frecuencias{$b} <=> $frecuencias{$a} } keys %frecuencias;

say "Las 10 palabras más frecuentes son:";
say join "\n", @palabras_ordenadas[0..9];
```

Este código Perl realiza las siguientes tareas:

1. Importa los módulos necesarios para manejar cadenas Unicode, manejo de errores y otras características.

2. Define un array @frases con varias frases en español.

3. Define un array @palabras que contiene todas las palabras de las frases, divididas en palabras individuales y eliminando los caracteres de puntuación.

4. Define un hash %frecuencias que almacena la frecuencia de cada palabra en las frases.

5. Utiliza un bucle foreach para iterar sobre las palabras en @palabras y actualizar las frecuencias en %frecuencias.

6. Define un array @palabras_ordenadas que contiene las palabras ordenadas por su frecuencia, de mayor a menor.

7. Imprime las 10 palabras más frecuentes en las frases.

Este código es complejo porque utiliza varias características avanzadas de Perl, incluyendo arrays, hashes, bucles, manejo de cadenas y ordenación. También utiliza módulos externos para manejar cadenas Unicode y manejo de errores.