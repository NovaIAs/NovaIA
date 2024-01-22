```perl
use utf8;
use strict;
use warnings;

# Constantes
my $num_lineas = 10;
my $num_columnas = 10;

# Crear una matriz
my @matriz = ();
for (my $i = 0; $i < $num_lineas; $i++) {
    for (my $j = 0; $j < $num_columnas; $j++) {
        $matriz[$i][$j] = int(rand(100));
    }
}

# Imprimir la matriz
print "Matriz:\n";
for (my $i = 0; $i < $num_lineas; $i++) {
    for (my $j = 0; $j < $num_columnas; $j++) {
        printf "%3d ", $matriz[$i][$j];
    }
    print "\n";
}

# Calcular la suma de cada fila
my @sumas_filas = ();
for (my $i = 0; $i < $num_lineas; $i++) {
    my $suma = 0;
    for (my $j = 0; $j < $num_columnas; $j++) {
        $suma += $matriz[$i][$j];
    }
    $sumas_filas[$i] = $suma;
}

# Imprimir la suma de cada fila
print "Suma de cada fila:\n";
for (my $i = 0; $i < $num_lineas; $i++) {
    printf "%3d ", $sumas_filas[$i];
}
print "\n";

# Calcular la suma de cada columna
my @sumas_columnas = ();
for (my $j = 0; $j < $num_columnas; $j++) {
    my $suma = 0;
    for (my $i = 0; $i < $num_lineas; $i++) {
        $suma += $matriz[$i][$j];
    }
    $sumas_columnas[$j] = $suma;
}

# Imprimir la suma de cada columna
print "Suma de cada columna:\n";
for (my $j = 0; $j < $num_columnas; $j++) {
    printf "%3d ", $sumas_columnas[$j];
}
print "\n";

# Calcular la suma de la diagonal principal
my $suma_diagonal_principal = 0;
for (my $i = 0; $i < $num_lineas; $i++) {
    $suma_diagonal_principal += $matriz[$i][$i];
}

# Imprimir la suma de la diagonal principal
print "Suma de la diagonal principal: $suma_diagonal_principal\n";

# Calcular la suma de la diagonal secundaria
my $suma_diagonal_secundaria = 0;
for (my $i = 0; $i < $num_lineas; $i++) {
    $suma_diagonal_secundaria += $matriz[$i][$num_columnas - $i - 1];
}

# Imprimir la suma de la diagonal secundaria
print "Suma de la diagonal secundaria: $suma_diagonal_secundaria\n";
```

Explicación del código:

* Se definen las constantes `$num_lineas` y `$num_columnas` para especificar el tamaño de la matriz.
* Se crea una matriz `@matriz` con las dimensiones especificadas.
* Se inicializa la matriz con valores aleatorios utilizando la función `int(rand(100))`.
* Se imprime la matriz.
* Se calcula la suma de cada fila y se almacena en el array `@sumas_filas`.
* Se imprime la suma de cada fila.
* Se calcula la suma de cada columna y se almacena en el array `@sumas_columnas`.
* Se imprime la suma de cada columna.
* Se calcula la suma de la diagonal principal y se almacena en la variable `$suma_diagonal_principal`.
* Se imprime la suma de la diagonal principal.
* Se calcula la suma de la diagonal secundaria y se almacena en la variable `$suma_diagonal_secundaria`.
* Se imprime la suma de la diagonal secundaria.