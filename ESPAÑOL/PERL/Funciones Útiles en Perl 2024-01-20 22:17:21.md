```perl
use strict;
use warnings;

# Función para calcular el factorial de un número
sub factorial {
    my $n = shift;
    return 1 if $n == 0;
    return $n * factorial($n - 1);
}

# Función para generar una lista de números primos hasta un límite dado
sub generar_primos {
    my $limite = shift;
    my @primos;
    for (my $i = 2; $i <= $limite; $i++) {
        my $es_primo = 1;
        for (my $j = 2; $j <= $i / 2; $j++) {
            if ($i % $j == 0) {
                $es_primo = 0;
                last;
            }
        }
        push @primos, $i if $es_primo;
    }
    return @primos;
}

# Función para calcular la suma de los dígitos de un número
sub sumar_digitos {
    my $n = shift;
    my $suma = 0;
    while ($n > 0) {
        $suma += $n % 10;
        $n = int($n / 10);
    }
    return $suma;
}

# Función para encontrar el número más grande en una lista
sub encontrar_maximo {
    my @lista = @_;
    my $maximo = $lista[0];
    foreach my $elemento (@lista) {
        if ($elemento > $maximo) {
            $maximo = $elemento;
        }
    }
    return $maximo;
}

# Función para encontrar el número más pequeño en una lista
sub encontrar_minimo {
    my @lista = @_;
    my $minimo = $lista[0];
    foreach my $elemento (@lista) {
        if ($elemento < $minimo) {
            $minimo = $elemento;
        }
    }
    return $minimo;
}

# Función para ordenar una lista de números en orden ascendente
sub ordenar_ascendente {
    my @lista = @_;
    @lista = sort { $a <=> $b } @lista;
    return @lista;
}

# Función para ordenar una lista de números en orden descendente
sub ordenar_descendente {
    my @lista = @_;
    @lista = sort { $b <=> $a } @lista;
    return @lista;
}

# Función para buscar un elemento en una lista
sub buscar_elemento {
    my $elemento = shift;
    my @lista = @_;
    my $indice = -1;
    for (my $i = 0; $i < @lista; $i++) {
        if ($lista[$i] == $elemento) {
            $indice = $i;
            last;
        }
    }
    return $indice;
}

# Función para eliminar un elemento de una lista
sub eliminar_elemento {
    my $elemento = shift;
    my @lista = @_;
    my @nueva_lista;
    foreach my $elemento_lista (@lista) {
        if ($elemento_lista != $elemento) {
            push @nueva_lista, $elemento_lista;
        }
    }
    return @nueva_lista;
}

# Función para insertar un elemento en una lista en una posición específica
sub insertar_elemento {
    my $elemento = shift;
    my $posicion = shift;
    my @lista = @_;
    splice(@lista, $posicion, 0, $elemento);
    return @lista;
}

# Función para invertir una lista
sub invertir_lista {
    my @lista = @_;
    @lista = reverse @lista;
    return @lista;
}

# Función para unir dos listas
sub unir_listas {
    my @lista1 = @_;
    my @lista2 = @_;
    my @lista_unida = (@lista1, @lista2);
    return @lista_unida;
}

# Función para encontrar el elemento más frecuente en una lista
sub encontrar_elemento_frecuente {
    my @lista = @_;
    my %frecuencias;
    foreach my $elemento (@lista) {
        $frecuencias{$elemento}++;
    }
    my $elemento_frecuente;
    my $frecuencia_maxima = 0;
    foreach my $elemento (keys %frecuencias) {
        if ($frecuencias{$elemento} > $frecuencia_maxima) {
            $elemento_frecuente = $elemento;
            $frecuencia_