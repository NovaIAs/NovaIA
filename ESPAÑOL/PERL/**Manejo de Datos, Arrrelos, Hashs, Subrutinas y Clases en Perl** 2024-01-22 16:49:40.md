```perl
# Establecer el valor de una variable usando la función "my"
my $nombre = "Alejandro";

# Crear un array con números
my @numeros = (1, 2, 3, 4, 5);

# Imprimir el contenido de una matriz usando un ciclo "for"
print "Los números del array son: ";
for my $numero (@numeros) {
  print "$numero, ";
}
print "\n";

# Crear un hash con claves y valores
my %paises = (
  "España" => "Madrid",
  "Francia" => "París",
  "Alemania" => "Berlín"
);

# Recuperar el valor de una clave de un hash usando la función "retrieve"
my $capital = $paises{"España"};
print "La capital de España es $capital\n";

# Crear una subrutina para imprimir un saludo
sub saludar {
  my $persona = shift;
  print "Hola, $persona!\n";
}

# Invocar la subrutina
saludar("Alejandro");

# Crear un objeto del tipo "Persona"
package Persona;

sub new {
  my $class = shift;
  my $nombre = shift;
  my $edad = shift;

  my $self = {
    _nombre => $nombre,
    _edad => $edad
  };

  return bless $self, $class;
}

sub get_nombre {
  my $self = shift;
  return $self->{_nombre};
}

sub get_edad {
  my $self = shift;
  return $self->{_edad};
}

sub saludar {
  my $self = shift;
  print "Hola, $self->{_nombre} ($self->{_edad} años)\n";
}

package main;

# Crear una instancia del objeto "Persona"
my $persona = new Persona("Juan", 25);

# Invocar los métodos del objeto
print "El nombre de la persona es: ", $persona->get_nombre(), "\n";
print "La edad de la persona es: ", $persona->get_edad(), "\n";
$persona->saludar();
```

Explicación del código:

1. Se declara una variable `$nombre` y se le asigna el valor "Alejandro".

2. Se crea un array `@numeros` con valores del 1 al 5.

3. Se utiliza un ciclo `for` para imprimir los elementos del array.

4. Se crea un hash `%paises` con las capitales de algunos países.

5. Se utiliza la función `retrieve` para obtener el valor de la clave "España" del hash.

6. Se define una subrutina `saludar` que recibe un parámetro e imprime un saludo.

7. Se invoca la subrutina `saludar` con el argumento "Alejandro".

8. Se define el paquete `Persona` que contiene una clase que representa a una persona con un nombre y una edad.

9. Se definen métodos para obtener el nombre, la edad y saludar a la persona.

10. Se crea una instancia del objeto `Persona` y se invocan sus métodos para imprimir su nombre, edad y saludarla.