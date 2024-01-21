```perl
use strict;
use warnings;
use utf8;

# Definición de constantes
use constant PI => 3.141592653589793;
use constant E  => 2.718281828459045;

# Definición de variables
my $nombre = "Juan Pérez";
my $edad   = 30;
my @colores = ("rojo", "verde", "azul");
my %países = (
    "España" => "Madrid",
    "Francia" => "París",
    "Alemania" => "Berlín",
);

# Operadores aritméticos
my $suma = 1 + 2;
my $resta = 3 - 4;
my $multiplicación = 5 * 6;
my $división = 7 / 8;
my $módulo = 9 % 10;

# Operadores de comparación
my $igualdad = 1 == 1;
my $desigualdad = 2 != 3;
my $mayor_que = 4 > 5;
my $menor_que = 6 < 7;
my $mayor_o_igual_que = 8 >= 8;
my $menor_o_igual_que = 9 <= 10;

# Operadores lógicos
my $y = 1 && 0;
my $o = 1 || 0;
my $no = !1;

# Control de flujo
if ($edad >= 18) {
    print "Eres mayor de edad.\n";
} elsif ($edad >= 16) {
    print "Eres menor de edad, pero puedes conducir.\n";
} else {
    print "Eres menor de edad.\n";
}

unless ($edad >= 18) {
    print "No eres mayor de edad.\n";
}

for my $color (@colores) {
    print "$color\n";
}

while (my $país = each %países) {
    print "$país => $países{$país}\n";
}

do {
    print "Introduzca un número: ";
    my $número = <STDIN>;
    chomp $número;
} until ($número =~ /^\d+$/);

# Funciones
sub saludar {
    my ($nombre) = @_;
    print "Hola, $nombre!\n";
}

sub calcular_área_de_círculo {
    my ($radio) = @_;
    return PI * $radio ** 2;
}

# Llamadas a funciones
saludar($nombre);

my $área = calcular_área_de_círculo(5);
print "El área del círculo es $área.\n";

# Expresiones regulares
my $texto = "Este es un texto de ejemplo.";

if ($texto =~ /ejemplo/) {
    print "El texto contiene la palabra 'ejemplo'.\n";
}

my @palabras = $texto =~ /\w+/g;
print join(", ", @palabras), "\n";

# Manejo de errores
eval {
    open my $archivo, "<", "archivo.txt";
    print <$archivo>;
    close $archivo;
} or do {
    print "No se pudo abrir el archivo.\n";
};

# Referencias
my $referencia_a_nombre = \$nombre;
print "El nombre es $nombre.\n";
$$referencia_a_nombre = "María González";
print "El nombre ahora es $nombre.\n";

# Paquetes
package MiPaquete;

sub saludar {
    my ($nombre) = @_;
    print "Hola, $nombre, desde MiPaquete!\n";
}

package main;

MiPaquete::saludar("Juan Pérez");

# Módulos
use MiMódulo;

MiMódulo::saludar("María González");

1;
```

Este código es un ejemplo de código complejo en Perl. Contiene una variedad de características y funcionalidades, incluyendo:

* Definición de constantes y variables
* Operadores aritméticos, de comparación y lógicos
* Control de flujo (if, elsif, else, unless, for, while, do-while)
* Funciones definidas por el usuario
* Expresiones regulares
* Manejo de errores (eval)
* Referencias
* Paquetes
* Módulos

Este código puede ser utilizado para realizar una variedad de tareas, incluyendo:

* Procesamiento de texto
* Cálculos matemáticos
* Programación de redes
* Desarrollo de aplicaciones web

Es importante tener en cuenta que este código es sólo un ejemplo, y que hay muchas otras formas de escribir código complejo en Perl. La mejor manera de aprender a escribir código complejo en Perl es comenzar con proyectos pequeños y simples, y luego ir aumentando gradualmente la complejidad de los mismos a medida que se adquiera más experiencia.