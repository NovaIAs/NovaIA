```perl
use strict;
use warnings;

# Definir funciones

sub saludar {
  my ($nombre) = @_;
  print "Hola, $nombre!\n";
}

sub sumar {
  my ($num1, $num2) = @_;
  return $num1 + $num2;
}

sub restar {
  my ($num1, $num2) = @_;
  return $num1 - $num2;
}

sub multiplicar {
  my ($num1, $num2) = @_;
  return $num1 * $num2;
}

sub dividir {
  my ($num1, $num2) = @_;
  return $num1 / $num2;
}

sub potencia {
  my ($num1, $num2) = @_;
  return $num1 ** $num2;
}

sub factorial {
  my ($num) = @_;
  if ($num == 0) {
    return 1;
  } else {
    return $num * factorial($num - 1);
  }
}

sub fibonacci {
  my ($num) = @_;
  if ($num <= 1) {
    return $num;
  } else {
    return fibonacci($num - 1) + fibonacci($num - 2);
  }
}

sub es_primo {
  my ($num) = @_;
  if ($num <= 1) {
    return 0;
  }
  for (my $i = 2; $i < $num; $i++) {
    if ($num % $i == 0) {
      return 0;
    }
  }
  return 1;
}

sub encontrar_divisores {
  my ($num) = @_;
  my @divisores;
  for (my $i = 1; $i <= $num; $i++) {
    if ($num % $i == 0) {
      push @divisores, $i;
    }
  }
  return @divisores;
}

sub máximo_común_divisor {
  my ($num1, $num2) = @_;
  while ($num2) {
    my $temp = $num2;
    $num2 = $num1 % $num2;
    $num1 = $temp;
  }
  return $num1;
}

sub mínimo_común_múltiplo {
  my ($num1, $num2) = @_;
  my $mcd = máximo_común_divisor($num1, $num2);
  return ($num1 * $num2) / $mcd;
}

# Definir datos

my @numeros = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
my %nombres = ("Juan" => 20, "María" => 30, "Pedro" => 40);

# Utilizar funciones

print "Hola mundo!\n";

saludar("Juan");

my $suma = sumar(10, 20);
print "La suma de 10 y 20 es $suma\n";

my $resta = restar(20, 10);
print "La resta de 20 y 10 es $resta\n";

my $multiplicación = multiplicar(10, 20);
print "La multiplicación de 10 y 20 es $multiplicación\n";

my $división = dividir(20, 10);
print "La división de 20 y 10 es $división\n";

my $potencia = potencia(2, 10);
print "La potencia de 2 elevado a 10 es $potencia\n";

my $factorial = factorial(5);
print "El factorial de 5 es $factorial\n";

my $fibonacci = fibonacci(10);
print "El número de Fibonacci de 10 es $fibonacci\n";

print "El número 7 es primo: ", es_primo(7), "\n";
print "El número 10 es primo: ", es_primo(10), "\n";

my @divisores = encontrar_divisores(12);
print "Los divisores de 12 son: @divisores\n";

my $mcd = máximo_común_divisor(12, 18);
print "El máximo común divisor de 12 y 18 es $mcd\n";

my $mcm = mínimo_común_múltiplo(12, 18);
print "El mínimo común múltiplo de 12 y 18 es $mcm\n";

# Utilizar estructuras de control

if (10 > 20) {
  print "10 es mayor que 20\n";
} else {
  print "10 es menor que 20\n";
}

for (my $i = 0; $i < 10; $i++) {
  print "$i\n";
}

foreach my $nombre (keys %nombres) {
  print "$nombre tiene $nombres{$nombre} años\n";
}

while (my $numero = shift @numeros) {
  print "$numero\n";
}

# Utilizar referencias

my $referencia_a_lista = \@numeros;
print "El primer elemento de la lista es $referencia_a_lista->[0]\n";

my $referencia_a_hash = \%nombres;
print "La edad de Juan es $referencia_a_hash->{Juan}\n";

# Utilizar módulos

use Math::Trig;

my $ángulo = 30;
my $seno = sin($ángulo);
print "El seno de 30 grados es $seno\n";
```

Explicación del código:

* El código define varias funciones, incluyendo funciones para saludar, sumar, restar, multiplicar, dividir, calcular potencias, calcular factoriales, calcular números de Fibonacci, comprobar si un número es primo, encontrar los divisores de un número, calcular el máximo común divisor y el mínimo común múltiplo.
* El código también define algunos datos, incluyendo una lista de números y un hash de nombres y edades.
* El código utiliza las funciones y los datos para realizar varios cálculos y operaciones.
* El código utiliza estructuras de control como `if`, `for`, `foreach` y `while` para controlar el flujo del programa.
* El código utiliza referencias para acceder a listas y hashes por referencia.
* El código utiliza el módulo `Math::Trig` para calcular funciones trigonométricas.