```perl
# Importa el módulo Math::Trig para usar funciones trigonométricas.
use Math::Trig;

# Define una función para calcular el área de un triángulo.
sub area_triangulo {
    my ($base, $altura) = @_;
    return 0.5 * $base * $altura;
}

# Define una función para calcular el volumen de una esfera.
sub volumen_esfera {
    my ($radio) = @_;
    return (4/3) * pi * $radio**3;
}

# Define una función para calcular el factorial de un número.
sub factorial {
    my ($num) = @_;
    if ($num == 0) {
        return 1;
    } else {
        return $num * factorial($num-1);
    }
}

# Define una función para generar una lista de números primos hasta un límite dado.
sub primos {
    my ($limite) = @_;
    my @primos;
    for (my $i = 2; $i <= $limite; $i++) {
        my $es_primo = 1;
        for (my $j = 2; $j < $i; $j++) {
            if ($i % $j == 0) {
                $es_primo = 0;
                last;
            }
        }
        if ($es_primo) {
            push @primos, $i;
        }
    }
    return @primos;
}

# Define una función para ordenar una lista de números.
sub ordenar {
    my @lista = @_;
    @lista = sort @lista;
    return @lista;
}

# Define una función para buscar un elemento en una lista.
sub buscar {
    my ($elemento, @lista) = @_;
    for (my $i = 0; $i < @lista; $i++) {
        if ($lista[$i] == $elemento) {
            return $i;
        }
    }
    return -1;
}

# Define una función para imprimir un mensaje de bienvenida.
sub bienvenida {
    my ($nombre) = @_;
    print "Hola, $nombre. Bienvenido a mi programa.\n";
}

# Imprime un mensaje de bienvenida.
bienvenida("Juan");

# Calcula el área de un triángulo con base de 10 y altura de 5.
my $area_triangulo = area_triangulo(10, 5);
print "El área del triángulo es $area_triangulo.\n";

# Calcula el volumen de una esfera con radio de 5.
my $volumen_esfera = volumen_esfera(5);
print "El volumen de la esfera es $volumen_esfera.\n";

# Calcula el factorial de 5.
my $factorial_5 = factorial(5);
print "El factorial de 5 es $factorial_5.\n";

# Genera una lista de números primos hasta 100.
my @primos = primos(100);
print "Los números primos hasta 100 son: @primos.\n";

# Ordena una lista de números.
my @lista = (5, 2, 8, 3, 1, 10);
my @lista_ordenada = ordenar(@lista);
print "La lista ordenada es: @lista_ordenada.\n";

# Busca un elemento en una lista.
my $indice = buscar(5, @lista);
if ($indice == -1) {
    print "El elemento no se encuentra en la lista.\n";
} else {
    print "El elemento se encuentra en el índice $indice.\n";
}
```

Explicación del código:

* La primera línea importa el módulo Math::Trig, que proporciona funciones trigonométricas.
* Las siguientes seis funciones definen diferentes operaciones matemáticas:
    * `area_triangulo()` calcula el área de un triángulo.
    * `volumen_esfera()` calcula el volumen de una esfera.
    * `factorial()` calcula el factorial de un número.
    * `primos()` genera una lista de números primos hasta un límite dado.
    * `ordenar()` ordena una lista de números.
    * `buscar()` busca un elemento en una lista.
* La función `bienvenida()` imprime un mensaje de bienvenida.
* El resto del código utiliza las funciones definidas anteriormente para realizar diferentes cálculos y operaciones.