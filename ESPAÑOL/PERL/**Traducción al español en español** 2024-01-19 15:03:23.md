```perl
# Creación de un diccionario en español
mi_diccionario = {
    "perro" => "dog",
    "gato" => "cat",
    "caballo" => "horse",
    "vaca" => "cow",
    "cerdo" => "pig",
};

# Creación de una matriz de cadenas en español
mi_matriz = ["uno", "dos", "tres", "cuatro", "cinco"];

# Función para traducir una palabra o frase del español al inglés
sub traducir {
    my $texto = $_[0];
    my @palabras = split /\s+/, $texto;

    # Traducción de cada palabra utilizando el diccionario
    for (my $i = 0; $i < @palabras; $i++) {
        if (exists $mi_diccionario{$palabras[$i]}) {
            $palabras[$i] = $mi_diccionario{$palabras[$i]};
        }
    }

    # Unión de las palabras traducidas en una cadena
    return join " ", @palabras;
}

# Uso de la función traducir
my $texto_a_traducir = "El perro corre rápido";
my $texto_traducido = traducir($texto_a_traducir);

# Impresión del texto traducido
print "$texto_traducido\n";

# Creación de una función para generar una tabla de multiplicar
sub tabla_de_multiplicar {
    my $numero = $_[0];

    # Creación de un encabezado de tabla
    print "\nTabla de multiplicar para el número $numero:\n";
    print "  |";

    # Impresión de la primera fila de la tabla
    for (my $i = 1; $i <= $numero; $i++) {
        printf "%3d |", $i;
    }
    print "\n";

    # Impresión de las filas restantes de la tabla
    for (my $i = 1; $i <= $numero; $i++) {
        printf "%3d |", $i;
        for (my $j = 1; $j <= $numero; $j++) {
            printf "%3d |", $i * $j;
        }
        print "\n";
    }
}

# Uso de la función tabla_de_multiplicar
my $numero_a_multiplicar = 5;
tabla_de_multiplicar($numero_a_multiplicar);
```

Explicación del código:

* El código comienza creando un diccionario en español, donde cada palabra en español se asocia con su traducción en inglés.
* A continuación, se crea una matriz de cadenas en español.
* Se define una función llamada `traducir` que toma una palabra o frase en español y la traduce al inglés utilizando el diccionario.
* La función `traducir` se utiliza para traducir una frase en español a inglés y se imprime el resultado.
* El código también define una función llamada `tabla_de_multiplicar` que genera una tabla de multiplicar para un número dado.
* La función `tabla_de_multiplicar` se utiliza para generar una tabla de multiplicar para un número dado y se imprime el resultado.