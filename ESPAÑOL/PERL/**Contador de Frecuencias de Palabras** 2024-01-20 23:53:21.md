```perl
use strict;
use warnings;

my $texto = "Este es un texto de ejemplo";

# Crear una expresión regular para extraer las palabras del texto
my $expresion_regular = qr/\w+/g;

# Extraer las palabras del texto utilizando la expresión regular
my @palabras = $texto =~ /$expresión_regular/;

# Crear un hash para almacenar las palabras y sus frecuencias
my %frecuencias;

# Iterar sobre las palabras extraídas y actualizar las frecuencias en el hash
foreach my $palabra (@palabras) {
    $frecuencias{$palabra}++;
}

# Crear una lista de pares palabra-frecuencia ordenada por frecuencia descendente
my @pares_palabra_frecuencia = sort { $b->{frecuencia} <=> $a->{frecuencia} } map { { palabra => $_, frecuencia => $frecuencias{$_} } } keys %frecuencias;

# Imprimir la lista de pares palabra-frecuencia
print "Palabras y sus frecuencias:\n";
foreach my $par (@pares_palabra_frecuencia) {
    print "$par->{palabra}: $par->{frecuencia}\n";
}
```

Este código PERL lee un texto de entrada, extrae las palabras del texto utilizando una expresión regular, cuenta la frecuencia de aparición de cada palabra y finalmente imprime una lista de palabras y sus frecuencias ordenada por frecuencia descendente.

A continuación se explica el código en detalle:

1. Se incluyen las directivas `use strict` y `use warnings` para garantizar un código más seguro y detectar posibles errores.

2. Se define una variable `$texto` que contiene el texto de entrada.

3. Se define una expresión regular `$expresión_regular` que se utiliza para extraer las palabras del texto. Esta expresión regular coincide con cualquier secuencia de caracteres alfanuméricos (letras y números) o guiones bajos (_).

4. Se extraen las palabras del texto utilizando la expresión regular y se almacenan en la lista `@palabras`.

5. Se crea un hash `%frecuencias` para almacenar las palabras y sus frecuencias. Las claves del hash son las palabras y los valores son las frecuencias.

6. Se itera sobre las palabras extraídas y se actualiza la frecuencia de cada palabra en el hash `%frecuencias`.

7. Se crea una lista `@pares_palabra_frecuencia` de pares palabra-frecuencia ordenada por frecuencia descendente. Cada par es un hash que contiene la palabra y su frecuencia.

8. Se imprime la lista de pares palabra-frecuencia.