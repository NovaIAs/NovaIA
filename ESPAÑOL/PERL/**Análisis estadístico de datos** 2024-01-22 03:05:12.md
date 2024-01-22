```perl
# Importa el módulo Pod::Usage para gestionar la ayuda y el uso del programa
use Pod::Usage;

# Define las opciones del programa y su ayuda
%opciones = (
    'archivo' => 'Fichero con los datos',
    'ayuda'   => 'Muestra esta ayuda y sale',
    'version' => 'Muestra la versión del programa y sale',
);

# Procesa las opciones especificadas en la línea de comandos
procesar_opciones(\%opciones);

# Muestra la ayuda si se especificó la opción '-h'
if ($opciones{'ayuda'}) {
    mostrar_ayuda();
    exit 0;
}

# Muestra la versión del programa si se especificó la opción '-v'
if ($opciones{'version'}) {
    print "Versión 1.0\n";
    exit 0;
}

# Abre el fichero especificado con la opción '-f' o 'archivo'
open(ARCHIVO, '<', $opciones{'archivo'}) || die "No se pudo abrir el fichero '$opciones{'archivo'}': $!";

# Lee el fichero línea a línea
while (my $linea = <ARCHIVO>) {
    # Elimina los caracteres de nueva línea y espacios en blanco
    chomp $linea;

    # Divide la línea en campos
    my @campos = split /,/, $linea;

    # Procesa cada campo
    foreach my $campo (@campos) {
        # Elimina los caracteres de comillas si existen
        $campo =~ s/"//g;

        # Convierte el campo a un número si es posible
        if ($campo =~ /^\d+$/) {
            $campo = int($campo);
        }

        # Almacena el campo en el array @datos
        push @datos, $campo;
    }
}

# Cierra el fichero
close(ARCHIVO);

# Calcula el promedio de los datos
my $promedio = promedio(@datos);

# Calcula la desviación estándar de los datos
my $desviacion_estandar = desviacion_estandar(@datos);

# Imprime los resultados
print "Promedio: $promedio\n";
print "Desviación estándar: $desviacion_estandar\n";

# Define la función 'promedio' para calcular el promedio de un array de números
sub promedio {
    my @numeros = @_;
    my $suma = 0;

    # Suma todos los números
    foreach my $numero (@numeros) {
        $suma += $numero;
    }

    # Devuelve la suma dividido por el número de elementos
    return $suma / scalar(@numeros);
}

# Define la función 'desviacion_estandar' para calcular la desviación estándar de un array de números
sub desviacion_estandar {
    my @numeros = @_;
    my $promedio = promedio(@numeros);
    my $varianza = 0;

    # Calcula la varianza
    foreach my $numero (@numeros) {
        $varianza += ($numero - $promedio) ** 2;
    }

    # Devuelve la raíz cuadrada de la varianza
    return sqrt($varianza / (scalar(@numeros) - 1));
}

# Define la función 'mostrar_ayuda' para mostrar la ayuda del programa
sub mostrar_ayuda {
    print <<HELP;
    Uso: $0 [opciones]

    Opciones:
    -h, --ayuda              Muestra esta ayuda y sale
    -v, --version             Muestra la versión del programa y sale
    -f, --archivo <fichero>  Fichero con los datos

HELP
}

# Define la función 'procesar_opciones' para procesar las opciones especificadas en la línea de comandos
sub procesar_opciones {
    my $opciones = $_[0];

    # Obtiene las opciones de la línea de comandos
    Getopt::Long::GetOptions($opciones, 'h|ayuda', 'v|version', 'f|archivo=s');
}
```
Este código es más complejo que los ejemplos anteriores y realiza un análisis estadístico de los datos contenidos en un fichero. El programa calcula el promedio y la desviación estándar de los datos y los imprime en la salida estándar. El programa también incluye una función de ayuda que muestra información sobre cómo usar el programa y una función para procesar las opciones especificadas en la línea de comandos.