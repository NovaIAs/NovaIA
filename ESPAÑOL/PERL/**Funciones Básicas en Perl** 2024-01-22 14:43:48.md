```perl
use strict;
use warnings;

# Definir una función para calcular el factorial de un número
sub factorial {
    my $number = $_[0];
    my $result = 1;

    for (my $i = 1; $i <= $number; $i++) {
        $result *= $i;
    }

    return $result;
}

# Definir una función para calcular la combinación de dos números
sub combinacion {
    my $n = $_[0];
    my $k = $_[1];

    my $result = factorial($n) / (factorial($k) * factorial($n - $k));

    return $result;
}

# Definir una función para generar un número aleatorio entre dos números
sub random_number {
    my $min = $_[0];
    my $max = $_[1];

    my $range = $max - $min + 1;
    my $random_number = int(rand($range)) + $min;

    return $random_number;
}

# Definir una función para generar una cadena aleatoria de una longitud específica
sub random_string {
    my $length = $_[0];

    my @characters = ('a'..'z', 'A'..'Z', 0..9);
    my $random_string = '';

    for (my $i = 0; $i < $length; $i++) {
        my $random_index = random_number(0, $#characters);
        my $random_character = $characters[$random_index];

        $random_string .= $random_character;
    }

    return $random_string;
}

# Definir una función para ordenar una lista de números
sub ordena_lista {
    my @lista = @_[0];

    my @lista_ordenada = sort { $a <=> $b } @lista;

    return @lista_ordenada;
}

# Definir una función para buscar un elemento en una lista
sub buscar_elemento {
    my $elemento = $_[0];
    my @lista = @_[1];

    my $index = -1;

    for (my $i = 0; $i < @lista; $i++) {
        if ($lista[$i] == $elemento) {
            $index = $i;
            last;
        }
    }

    return $index;
}

# Definir una función para convertir una cadena a mayúsculas
sub a_mayusculas {
    my $cadena = $_[0];

    my $cadena_mayusculas = uc($cadena);

    return $cadena_mayusculas;
}

# Definir una función para convertir una cadena a minúsculas
sub a_minusculas {
    my $cadena = $_[0];

    my $cadena_minusculas = lc($cadena);

    return $cadena_minusculas;
}

# Definir una función para eliminar los espacios en blanco de una cadena
sub eliminar_espacios {
    my $cadena = $_[0];

    my $cadena_sin_espacios = $cadena =~ s/\s+//g;

    return $cadena_sin_espacios;
}

# Definir una función para dividir una cadena en una lista de cadenas
sub dividir_cadena {
    my $cadena = $_[0];
    my $delimitador = $_[1];

    my @lista_cadenas = split($delimitador, $cadena);

    return @lista_cadenas;
}

# Definir una función para unir una lista de cadenas en una cadena
sub unir_cadenas {
    my @lista_cadenas = @_[0];
    my $delimitador = $_[1];

    my $cadena = join($delimitador, @lista_cadenas);

    return $cadena;
}

# Definir una función para leer un archivo y devolver su contenido como una cadena
sub leer_archivo {
    my $nombre_archivo = $_[0];

    open my $archivo, '<', $nombre_archivo or die "No se pudo abrir el archivo $nombre_archivo: $!";

    my $contenido = do { local $/; <$archivo> };

    close $archivo;

    return $contenido;
}

# Definir una función para escribir una cadena en un archivo
sub escribir_archivo {
    my $nombre_archivo = $_[0];
    my $contenido = $_[1];

    open my $archivo, '>', $nombre_archivo or die "No se pudo abrir el archivo $nombre_archivo: $!";

    print $archivo $contenido;

    close $archivo;
}

# Ejemplo de uso de las funciones definidas

my $factorial_de_5 = factorial(5);
print "El factorial de 5 es: $factorial_de_5\n";

my $combinacion_de_10_y_5 = combinacion(10, 5);
print "La combinación de 10 y 5 es: $combinacion_de_10_y_5\n";

my $numero_aleatorio_entre_1_y_10 = random_number(1, 10);
print "Un número aleatorio entre 1 y 10 es: $numero_aleatorio_entre_1_y_10\n";

my $cadena_aleatoria_de_10_caracteres = random_string(10);
print "Una cadena aleatoria de 10 caracteres es: $cadena_aleatoria_de_10_caracteres\n";

my @lista_desordenada = (5, 3, 1, 2, 4);
print "Lista desordenada: @lista_desordenada\n";

my @lista_ordenada = ordena_lista(@lista_desordenada);
print "Lista ordenada: @lista_ordenada\n";

my $elemento_a_buscar = 3;
my $indice_del_elemento = buscar_elemento($elemento_a_buscar, @lista_ordenada);
print "El elemento $elemento_a_buscar se encuentra en el índice $indice_del_elemento\n";

my $cadena_a_convertir_a_mayusculas = "hola mundo";
print "Cadena a convertir a mayúsculas: $cadena_a_convertir_a_mayusculas\n";

my $cadena_en_mayusculas = a_mayusculas($cadena_a_convertir_a_mayusculas);
print "Cadena en mayúsculas: $cadena_en_mayusculas\n";

my $cadena_a_convertir_a_minusculas = "HOLA MUNDO";
print "Cadena a convertir a minúsculas: $cadena_a_convertir_a_minusculas\n";

my $cadena_en_minusculas = a_minusculas($cadena_a_convertir_a_minusculas);
print "Cadena en minúsculas: $cadena_en_minusculas\n";

my $cadena_con_espacios = "Hola Mundo";
print "Cadena con espacios: $cadena_con_espacios\n";

my $cadena_sin_espacios = eliminar_espacios($cadena_con_espacios);
print "Cadena sin espacios: $cadena_sin_espacios\n";

my $cadena_a_dividir = "Hola,Mundo,Perl";
print "Cadena a dividir: $cadena_a_dividir\n";

my @lista_cadenas_divididas = dividir_cadena($cadena_a_dividir, ',');
print "Lista de cadenas divididas: @lista_cadenas_divididas\n";

my @lista_cadenas_a_unir = ("Hola", "Mundo", "Perl");
print "Lista de cadenas a unir: @lista_cadenas_a_unir\n";

my $cadena_unida = unir_cadenas(@lista_cadenas_a_unir, ',');
print "Cadena unida: $cadena_unida\n";

my $contenido_del_archivo = leer_archivo('archivo.txt');
print "Contenido del archivo 'archivo.txt': $contenido_del_archivo\n";

escribir_archivo('archivo.txt', 'Nuevo contenido del archivo');
print "Se ha escrito el nuevo contenido en el archivo 'archivo.txt'\n";
```

Explicación del código:

1. Definimos una serie de funciones que realizan diversas tareas, como calcular el factorial de un número, generar números y cadenas aleatorias, ordenar listas, buscar elementos en listas, convertir cadenas a mayúsculas y minúsculas, eliminar espacios en blanco de cadenas, dividir cadenas en listas de cadenas y unir listas de cadenas en una cadena.

2. También definimos funciones para leer y escribir archivos.

3. A continuación, mostramos ejemplos de uso de cada una de las funciones definidas.

Este código es un ejemplo de cómo se puede utilizar el lenguaje Perl para realizar una variedad de tareas comunes de programación. Es un código complejo y diferenciado que es poco probable que se repita nuevamente.