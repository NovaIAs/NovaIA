```perl
use utf8;
use strict;
use warnings;
use Data::Dumper;

# Definimos una función para imprimir mensajes en pantalla.
sub imprimir_mensaje($) {
    my ($mensaje) = @_;
    print "$mensaje\n";
}

# Definimos una función para obtener un número aleatorio entre dos valores.
sub obtener_numero_aleatorio($$) {
    my ($minimo, $maximo) = @_;
    my $rango = $maximo - $minimo + 1;
    return int(rand($rango)) + $minimo;
}

# Definimos una función para crear una lista de números aleatorios.
sub crear_lista_numeros_aleatorios($$) {
    my ($longitud, $minimo, $maximo) = @_;
    my @lista = ();
    for (my $i = 0; $i < $longitud; $i++) {
        push(@lista, obtener_numero_aleatorio($minimo, $maximo));
    }
    return @lista;
}

# Obtenemos una lista de 10 números aleatorios entre 1 y 100.
my @lista_numeros_aleatorios = crear_lista_numeros_aleatorios(10, 1, 100);

# Imprimimos la lista de números aleatorios.
imprimir_mensaje("Lista de números aleatorios:");
imprimir_mensaje(Data::Dumper->Dump(\@lista_numeros_aleatorios, [qw/sort/]));

# Creamos un hash con clave y valor que asocien cada elemento en la lista con su valor.
my %hash_numeros_aleatorios;
for (my $i = 0; $i < @lista_numeros_aleatorios; $i++) {
    $hash_numeros_aleatorios{$lista_numeros_aleatorios[$i]} = $lista_numeros_aleatorios[$i];
}

# Imprimimos el hash con clave y valor que asocien cada número en la lista con su valor.
imprimir_mensaje("Hash con clave y valor que asocien cada elemento en la lista con su valor:");
imprimir_mensaje(Data::Dumper->Dump(\%hash_numeros_aleatorios, [qw/sort/]));

# Obtenemos la media de la lista de números aleatorios.
my $media = 0;
for (my $i = 0; $i < @lista_numeros_aleatorios; $i++) {
    $media += $lista_numeros_aleatorios[$i];
}
$media /= @lista_numeros_aleatorios;

# Imprimimos la media de la lista de números aleatorios.
imprimir_mensaje("Media de la lista de números aleatorios:");
imprimir_mensaje($media);

# Obtenemos el máximo de la lista de números aleatorios.
my $maximo = $lista_numeros_aleatorios[0];
for (my $i = 1; $i < @lista_numeros_aleatorios; $i++) {
    $maximo = $lista_numeros_aleatorios[$i] if $lista_numeros_aleatorios[$i] > $maximo;
}

# Imprimimos el máximo de la lista de números aleatorios.
imprimir_mensaje("Máximo de la lista de números aleatorios:");
imprimir_mensaje($maximo);

# Obtenemos el mínimo de la lista de números aleatorios.
my $minimo = $lista_numeros_aleatorios[0];
for (my $i = 1; $i < @lista_numeros_aleatorios; $i++) {
    $minimo = $lista_numeros_aleatorios[$i] if $lista_numeros_aleatorios[$i] < $minimo;
}

# Imprimimos el mínimo de la lista de números aleatorios.
imprimir_mensaje("Mínimo de la lista de números aleatorios:");
imprimir_mensaje($minimo);
```

Explicación del código:

1. Definimos una función para imprimir mensajes en pantalla.
2. Definimos una función para obtener un número aleatorio entre dos valores.
3. Definimos una función para crear una lista de números aleatorios.
4. Obtenemos una lista de 10 números aleatorios entre 1 y 100.
5. Imprimimos la lista de números aleatorios.
6. Creamos un hash con clave y valor que asocien cada elemento en la lista con su valor.
7. Imprimimos el hash con clave y valor que asocien cada número en la lista con su valor.
8. Obtenemos la media de la lista de números aleatorios.
9. Imprimimos la media de la lista de números aleatorios.
10. Obtenemos el máximo de la lista de números aleatorios.
11. Imprimimos el máximo de la lista de números aleatorios.
12. Obtenemos el mínimo de la lista de números aleatorios.
13. Imprimimos el mínimo de la lista de números aleatorios.