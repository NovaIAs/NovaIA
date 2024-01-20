```perl
use strict;
use warnings;

# Definimos un hash para almacenar los datos de los alumnos
my %alumnos = (
    "Juan" => {
        "nombre" => "Juan",
        "apellido" => "García",
        "edad" => 20,
        "notas" => [
            {
                "asignatura" => "Matemáticas",
                "nota" => 8
            },
            {
                "asignatura" => "Física",
                "nota" => 9
            },
            {
                "asignatura" => "Química",
                "nota" => 7
            }
        ]
    },
    "María" => {
        "nombre" => "María",
        "apellido" => "Fernández",
        "edad" => 22,
        "notas" => [
            {
                "asignatura" => "Matemáticas",
                "nota" => 9
            },
            {
                "asignatura" => "Física",
                "nota" => 8
            },
            {
                "asignatura" => "Química",
                "nota" => 8
            }
        ]
    },
    "Pedro" => {
        "nombre" => "Pedro",
        "apellido" => "López",
        "edad" => 24,
        "notas" => [
            {
                "asignatura" => "Matemáticas",
                "nota" => 7
            },
            {
                "asignatura" => "Física",
                "nota" => 7
            },
            {
                "asignatura" => "Química",
                "nota" => 6
            }
        ]
    }
);

# Definimos una subrutina para calcular la nota media de un alumno
sub nota_media {
    my $alumno = shift;
    my $total = 0;
    my $num_notas = scalar @{$alumno->{'notas'}};
    foreach my $nota (@{$alumno->{'notas'}}) {
        $total += $nota->{'nota'};
    }
    return $total / $num_notas;
}

# Imprimimos los datos de los alumnos
foreach my $alumno (values %alumnos) {
    print "Nombre: $alumno->{'nombre'} $alumno->{'apellido'}\n";
    print "Edad: $alumno->{'edad'}\n";
    print "Nota media: ", nota_media($alumno), "\n\n";
}
```

Este código crea un hash para almacenar los datos de los alumnos, con su nombre, apellido, edad y notas. Luego define una subrutina para calcular la nota media de un alumno y finalmente imprime los datos de los alumnos con su nota media.

El código es complejo porque:

* Utiliza un hash para almacenar los datos de los alumnos, lo que permite acceder a ellos de forma fácil y eficiente.
* Define una subrutina para calcular la nota media de un alumno, lo que permite reutilizar el código y hacerlo más modular.
* Imprime los datos de los alumnos utilizando un bucle `foreach`, lo que permite iterar sobre los datos y mostrarlos de forma clara y concisa.