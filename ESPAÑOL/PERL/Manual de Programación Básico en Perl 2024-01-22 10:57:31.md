```perl
use strict;
use warnings;
use utf8;

# Definición de subrutinas
sub saludar {
    my $nombre = shift;
    print "Hola, $nombre!\n";
}

sub despedir {
    my $nombre = shift;
    print "Adiós, $nombre!\n";
}

sub sumar {
    my $a = shift;
    my $b = shift;
    return $a + $b;
}

sub restar {
    my $a = shift;
    my $b = shift;
    return $a - $b;
}

sub multiplicar {
    my $a = shift;
    my $b = shift;
    return $a * $b;
}

sub dividir {
    my $a = shift;
    my $b = shift;
    return $a / $b;
}

# Uso de subrutinas
saludar("Juan");
despedir("María");

my $resultado = sumar(1, 2);
print "La suma de 1 y 2 es $resultado\n";

$resultado = restar(3, 4);
print "La resta de 3 y 4 es $resultado\n";

$resultado = multiplicar(5, 6);
print "La multiplicación de 5 y 6 es $resultado\n";

$resultado = dividir(7, 8);
print "La división de 7 y 8 es $resultado\n";

# Uso de estructuras de control
my $edad = 18;

if ($edad >= 18) {
    print "Eres mayor de edad\n";
} elsif ($edad >= 13) {
    print "Eres adolescente\n";
} else {
    print "Eres niño\n";
}

my @nombres = ("Juan", "María", "Pedro");

foreach my $nombre (@nombres) {
    print "Hola, $nombre!\n";
}

my %edades = ("Juan" => 18, "María" => 20, "Pedro" => 22);

foreach my $nombre (keys %edades) {
    print "$nombre tiene $edades{$nombre} años\n";
}

# Uso de expresiones regulares
my $cadena = "Hola mundo, cómo estás?";

if ($cadena =~ /mundo/) {
    print "La cadena contiene la palabra 'mundo'\n";
}

my @palabras = $cadena =~ / /g;

foreach my $palabra (@palabras) {
    print "$palabra\n";
}

# Uso de módulos
use Math::Trig;

my $angulo = 45;

my $seno = sin($angulo);
print "El seno de $angulo es $seno\n";

my $coseno = cos($angulo);
print "El coseno de $angulo es $coseno\n";

my $tangente = tan($angulo);
print "La tangente de $angulo es $tangente\n";

# Uso de objetos y clases
package Punto;

sub new {
    my ($class, $x, $y) = @_;
    return bless {
        x => $x,
        y => $y,
    }, $class;
}

sub x {
    my $self = shift;
    return $self->{x};
}

sub y {
    my $self = shift;
    return $self->{y};
}

sub distancia_a {
    my $self = shift;
    my $otro_punto = shift;
    my $dx = $self->{x} - $otro_punto->{x};
    my $dy = $self->{y} - $otro_punto->{y};
    return sqrt($dx * $dx + $dy * $dy);
}

package main;

my $p1 = Punto->new(1, 2);
my $p2 = Punto->new(3, 4);

my $distancia = $p1->distancia_a($p2);
print "La distancia entre $p1 y $p2 es $distancia\n";
```

Explicación del código:

* El código está escrito en el lenguaje de programación Perl y utiliza una combinación de subrutinas, estructuras de control, expresiones regulares, módulos y objetos y clases para realizar diversas tareas.
* Las subrutinas son funciones que pueden ser llamadas desde cualquier parte del programa. Las subrutinas definidas en este código incluyen `saludar`, `despedir`, `sumar`, `restar`, `multiplicar` y `dividir`.
* Las estructuras de control permiten controlar el flujo de ejecución del programa. Las estructuras de control utilizadas en este código incluyen `if`, `elsif` y `else`, así como `foreach` y `for`.
* Las expresiones regulares son patrones que se utilizan para buscar y reemplazar texto. La expresión regular utilizada en este código es `~ /mundo/`, que busca la palabra "mundo" en una cadena de caracteres.
* Los módulos son bibliotecas que proporcionan funcionalidad adicional al programa. El módulo utilizado en este código es `Math::Trig`, que proporciona funciones para realizar cálculos trigonométricos.
* Los objetos y clases son una forma de organizar los datos y la funcionalidad en un programa. El código define una clase llamada `Punto`, que representa un punto en el espacio 2D. La clase `Punto` tiene dos atributos, `x` e `y`, y un método llamado `distancia_a`, que calcula la distancia entre dos puntos.

El código se puede utilizar para realizar diversas tareas, como calcular la suma, resta, multiplicación y división de dos números, saludar y despedir a una persona, imprimir los nombres de una lista de personas, buscar una palabra en una cadena de caracteres, calcular el seno, coseno y tangente de un ángulo, y calcular la distancia entre dos puntos en el espacio 2D.