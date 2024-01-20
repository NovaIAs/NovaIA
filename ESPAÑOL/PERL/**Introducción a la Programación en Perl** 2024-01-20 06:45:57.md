```
use strict;
use warnings;

# Declaración de variables
my $nombre = "Juan";
my $edad = 30;
my @ciudades = ("Madrid", "Barcelona", "Valencia");
my %paises = (
    "España" => "Madrid",
    "Francia" => "París",
    "Italia" => "Roma"
);

# Operadores
print "Hola, mi nombre es $nombre y tengo $edad años.\n";
print "Las ciudades más importantes de España son: @ciudades.\n";
print "La capital de España es $paises{'España'}.\n";

# Control de flujo
if ($edad >= 18) {
    print "Eres mayor de edad.\n";
} else {
    print "Eres menor de edad.\n";
}

# Bucles
for my $ciudad (@ciudades) {
    print "Estoy visitando $ciudad.\n";
}

foreach my $pais (keys %paises) {
    print "La capital de $pais es $paises{$pais}.\n";
}

# Funciones
sub saludar {
    my $nombre = shift;
    print "Hola, $nombre!\n";
}

saludar("María");

# Uso de módulos
use Math::Trig;
my $angulo = 45;
my $seno = sin($angulo);
print "El seno de $angulo es $seno.\n";

# Excepciones
eval {
    open(MI_FICHERO, "<", "no_existe.txt");
};
if ($@) {
    print "No se ha podido abrir el fichero.\n";
} else {
    print "El fichero se ha abierto correctamente.\n";
}

# Referencias
my $referencia_a_nombre = \$nombre;
$$referencia_a_nombre = "María";
print "El nombre es ahora $nombre.\n";

# Expresiones regulares
my $texto = "Hola, soy Juan y tengo 30 años.";
if ($texto =~ /Juan/) {
    print "El texto contiene el nombre Juan.\n";
}

# Programación orientada a objetos
package Persona;

sub new {
    my ($class, %args) = @_;
    my $self = bless {
        nombre => $args{nombre},
        edad => $args{edad}
    }, $class;
    return $self;
}

sub nombre {
    my ($self) = @_;
    return $self->{nombre};
}

sub edad {
    my ($self) = @_;
    return $self->{edad};
}

package main;

my $persona = Persona->new(nombre => "Juan", edad => 30);
print "El nombre de la persona es $persona->nombre.\n";
print "La edad de la persona es $persona->edad.\n";

# Cierre del programa
exit;
```

Explicación del código:

* El código está escrito en Perl, un lenguaje de programación interpretado de alto nivel.
* El código declara una serie de variables, incluyendo una variable de tipo escalar ($nombre), una variable de tipo array (@ciudades), una variable de tipo hash (%paises) y una variable de tipo referencia ($referencia_a_nombre).
* El código utiliza varios operadores, incluyendo el operador de concatenación (.), el operador de acceso a arrays (@), el operador de acceso a hashes (%), el operador de comparación (==) y el operador de asignación (=).
* El código utiliza varias estructuras de control de flujo, incluyendo el condicional if-else, el bucle for y el bucle foreach.
* El código define una función llamada saludar() que toma un argumento y lo saluda.
* El código utiliza el módulo Math::Trig para calcular el seno de un ángulo.
* El código utiliza el bloque eval para capturar excepciones.
* El código utiliza expresiones regulares para buscar una cadena dentro de un texto.
* El código define una clase llamada Persona que tiene dos métodos: nombre() y edad().
* El código crea una instancia de la clase Persona y llama a sus métodos para obtener el nombre y la edad de la persona.
* El código sale del programa con el comando exit.