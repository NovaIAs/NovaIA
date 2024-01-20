```perl
use strict;
use warnings;

# Definir una lista de palabras en español
my @palabras = qw(
    hola
    mundo
    cómo
    estás
    bien
    gracias
    adiós
);

# Definir una función para traducir una palabra del español al inglés
sub traducir {
    my $palabra = shift;

    # Usar un diccionario para traducir la palabra
    my %diccionario = (
        hola    => 'hello',
        mundo   => 'world',
        cómo    => 'how',
        estás   => 'are',
        bien    => 'well',
        gracias => 'thank',
        adiós   => 'goodbye',
    );

    # Devolver la traducción de la palabra
    return $diccionario{$palabra};
}

# Crear un bucle para traducir cada palabra de la lista
foreach my $palabra (@palabras) {
    # Llamar a la función traducir para obtener la traducción de la palabra
    my $traduccion = traducir($palabra);

    # Imprimir la palabra y su traducción
    print "$palabra: $traduccion\n";
}
```

Este código crea una lista de palabras en español, define una función para traducir una palabra del español al inglés utilizando un diccionario, y luego crea un bucle para traducir cada palabra de la lista e imprimir la palabra y su traducción.

El código utiliza las siguientes características de Perl:

* **Listas:** Las listas son una forma de almacenar una colección de valores en Perl. Se definen utilizando el operador `qw()` y se separan los valores por espacios.
* **Hashes:** Los hashes son una forma de almacenar pares clave-valor en Perl. Se definen utilizando el operador `%()` y los pares clave-valor se separan por dos puntos.
* **Funciones:** Las funciones son bloques de código reutilizables que se pueden llamar desde otras partes del programa. Se definen utilizando la palabra clave `sub` seguida del nombre de la función y una lista de parámetros.
* **Bucles:** Los bucles son una forma de ejecutar un bloque de código varias veces. Hay diferentes tipos de bucles en Perl, incluyendo el bucle `foreach`, que se utiliza en este código para iterar sobre una lista.