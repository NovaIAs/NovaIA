```perl
use strict;
use warnings;

# Definir un hash con los datos de los alumnos
my %alumnos = (
    "1" => {
        "nombre" => "Juan",
        "apellido" => "Pérez",
        "notas" => [8, 9, 7, 8, 9]
    },
    "2" => {
        "nombre" => "María",
        "apellido" => "López",
        "notas" => [7, 8, 9, 8, 10]
    },
    "3" => {
        "nombre" => "Pedro",
        "apellido" => "García",
        "notas" => [9, 10, 8, 9, 8]
    },
);

# Definir una función para calcular la media de un conjunto de notas
sub calcular_media {
    my @notas = @_;
    my $suma = 0;
    foreach my $nota (@notas) {
        $suma += $nota;
    }
    return $suma / scalar(@notas);
}

# Iterar sobre el hash de alumnos y mostrar sus datos
foreach my $id (keys %alumnos) {
    my $alumno = $alumnos{$id};
    print "Alumno $id:\n";
    print "Nombre: $alumno->{nombre}\n";
    print "Apellido: $alumno->{apellido}\n";
    print "Notas: @{$alumno->{notas}}\n";
    print "Media: ", calcular_media(@{$alumno->{notas}}), "\n\n";
}
```

Este código define un hash llamado `%alumnos` con los datos de tres alumnos. Cada alumno tiene un nombre, un apellido y una lista de notas.

A continuación, se define una función llamada `calcular_media` que toma un conjunto de notas y devuelve la media de las mismas.

A continuación, se itera sobre el hash `%alumnos` y se muestran los datos de cada alumno, incluyendo su nombre, apellido, notas y media.

Este código es complejo porque utiliza varias características del lenguaje Perl, como hashes, arrays, funciones y loops. También es complejo porque realiza varios cálculos, como calcular la media de un conjunto de notas.