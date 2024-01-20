```perl
use strict;
use warnings;

# Definición de subrutinas
sub saludar {
    my $nombre = shift;
    print "Hola, $nombre!\n";
}

sub despedirse {
    my $nombre = shift;
    print "Adiós, $nombre!\n";
}

# Definición de una lista de nombres
my @nombres = ("Juan", "María", "Pedro", "Ana");

# Iterar sobre la lista de nombres y llamar a las subrutinas para saludar y despedirse
foreach my $nombre (@nombres) {
    saludar($nombre);
    despedirse($nombre);
}

# Definición de una función que calcula el factorial de un número
sub factorial {
    my $n = shift;
    my $resultado = 1;
    for (my $i = 2; $i <= $n; $i++) {
        $resultado *= $i;
    }
    return $resultado;
}

# Imprimir el factorial de los números de 1 a 10
for (my $i = 1; $i <= 10; $i++) {
    print "$i! = ", factorial($i), "\n";
}

# Definición de una clase Persona
package Persona;

sub new {
    my $class = shift;
    my $self = {
        nombre => shift,
        edad => shift,
    };
    bless $self, $class;
    return $self;
}

sub nombre {
    my $self = shift;
    return $self->{nombre};
}

sub edad {
    my $self = shift;
    return $self->{edad};
}

sub hablar {
    my $self = shift;
    print "Hola, mi nombre es ", $self->nombre, " y tengo ", $self->edad, " años.\n";
}

# Crear una instancia de la clase Persona
my $persona = Persona->new("Juan", 25);

# Llamar a los métodos de la instancia
print "Nombre: ", $persona->nombre, "\n";
print "Edad: ", $persona->edad, "\n";
$persona->hablar;

# Definición de un módulo Matemáticas
package Matemáticas;

sub suma {
    my $a = shift;
    my $b = shift;
    return $a + $b;
}

sub resta {
    my $a = shift;
    my $b = shift;
    return $a - $b;
}

sub multiplicación {
    my $a = shift;
    my $b = shift;
    return $a * $b;
}

sub división {
    my $a = shift;
    my $b = shift;
    return $a / $b;
}

# Utilizar el módulo Matemáticas
print "Suma de 10 y 20: ", Matemáticas::suma(10, 20), "\n";
print "Resta de 25 y 15: ", Matemáticas::resta(25, 15), "\n";
print "Multiplicación de 3 y 4: ", Matemáticas::multiplicación(3, 4), "\n";
print "División de 100 por 2: ", Matemáticas::división(100, 2), "\n";
```

Explicación del código:

1. **Subrutinas saludar y despedirse:**
   - Definiendo subrutinas saludar y despedirse en PERL con un parámetro cada una.
   - Usando las subrutinas para saludar y despedirse de una lista de nombres dada.

2. **Función factorial:**
   - Definiendo una función factorial en PERL que calcula el factorial de un número dado.
   - Imprimir los factoriales de los números de 1 a 10.

3. **Clase Persona:**
   - Definiendo una clase Persona en PERL con los métodos nombre, edad y hablar.
   - Crear una instancia de la clase Persona y llamar a sus métodos.

4. **Módulo Matemáticas:**
   - Definiendo un módulo Matemáticas en PERL con las funciones suma, resta, multiplicación y división.
   - Utilizar el módulo Matemáticas para realizar operaciones aritméticas.

Este código es complejo y abarca diversas características de PERL, incluyendo subrutinas, funciones, clases, módulos y manejo de listas.