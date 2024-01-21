```perl
use strict;
use warnings;

# Crear una matriz de datos
my @matriz = (
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
);

# Iterar sobre la matriz
foreach my $fila (@matriz) {
    foreach my $columna (@$fila) {
        print "$columna ";
    }
    print "\n";
}

# Crear un hash de datos
my %hash = (
    'nombre' => 'Juan',
    'edad' => 25,
    'ciudad' => 'Madrid'
);

# Iterar sobre el hash
foreach my $clave (keys %hash) {
    my $valor = $hash{$clave};
    print "$clave: $valor\n";
}

# Crear una función
sub suma {
    my ($a, $b) = @_;
    return $a + $b;
}

# Llamar a la función
my $resultado = suma(1, 2);
print "$resultado\n";

# Crear un objeto
package MiObjeto;

sub new {
    my ($class, %args) = @_;
    bless {
        nombre => $args{nombre},
        edad => $args{edad},
        ciudad => $args{ciudad}
    }, $class;
}

sub nombre {
    my ($self) = @_;
    return $self->{nombre};
}

sub edad {
    my ($self) = @_;
    return $self->{edad};
}

sub ciudad {
    my ($self) = @_;
    return $self->{ciudad};
}

package main;

# Crear una instancia del objeto
my $objeto = MiObjeto->new(
    nombre => 'Juan',
    edad => 25,
    ciudad => 'Madrid'
);

# Acceder a los atributos del objeto
print "$objeto->nombre\n";
print "$objeto->edad\n";
print "$objeto->ciudad\n";
```

Explicación del código:

* Se crea una matriz de datos y se itera sobre ella para imprimir el contenido de cada celda.
* Se crea un hash de datos y se itera sobre él para imprimir el contenido de cada clave y valor.
* Se crea una función que suma dos números y se llama a la función para obtener el resultado.
* Se crea una clase de objeto y se crea una instancia de la clase.
* Se accede a los atributos del objeto para imprimir el contenido de cada atributo.