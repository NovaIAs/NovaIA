```perl
use strict;
use warnings;

# Definir variables
my $num1 = 10;
my $num2 = 20;
my $resultado = 0;

# Realizar operaciones matemáticas
$resultado = $num1 + $num2;
print "La suma de $num1 y $num2 es $resultado\n";

$resultado = $num1 - $num2;
print "La resta de $num1 y $num2 es $resultado\n";

$resultado = $num1 * $num2;
print "La multiplicación de $num1 y $num2 es $resultado\n";

$resultado = $num1 / $num2;
print "La división de $num1 y $num2 es $resultado\n";

# Realizar operaciones de comparación
if ($num1 == $num2) {
    print "$num1 y $num2 son iguales\n";
} else {
    print "$num1 y $num2 no son iguales\n";
}

if ($num1 > $num2) {
    print "$num1 es mayor que $num2\n";
} else {
    print "$num1 es menor que $num2\n";
}

if ($num1 >= $num2) {
    print "$num1 es mayor o igual que $num2\n";
} else {
    print "$num1 es menor o igual que $num2\n";
}

# Realizar operaciones lógicas
if ($num1 && $num2) {
    print "$num1 y $num2 son verdaderos\n";
} else {
    print "$num1 o $num2 son falsos\n";
}

if ($num1 || $num2) {
    print "$num1 o $num2 son verdaderos\n";
} else {
    print "$num1 y $num2 son falsos\n";
}

# Realizar operaciones de repetición
for (my $i = 1; $i <= 10; $i++) {
    print "$i\n";
}

while (my $i = 1; $i <= 10; $i++) {
    print "$i\n";
}

do {
    print "$i\n";
    $i++;
} while ($i <= 10);

# Realizar operaciones de selección
if ($num1 > $num2) {
    print "$num1 es mayor que $num2\n";
} elsif ($num1 == $num2) {
    print "$num1 y $num2 son iguales\n";
} else {
    print "$num1 es menor que $num2\n";
}

# Realizar operaciones de manipulación de cadenas
my $cadena = "Hola mundo";

print "La longitud de la cadena es " . length($cadena) . "\n";
print "El primer carácter de la cadena es " . substr($cadena, 0, 1) . "\n";
print "El último carácter de la cadena es " . substr($cadena, -1, 1) . "\n";
print "La cadena en mayúsculas es " . uc($cadena) . "\n";
print "La cadena en minúsculas es " . lc($cadena) . "\n";

# Realizar operaciones de manipulación de arrays
my @array = (1, 2, 3, 4, 5);

print "El tamaño del array es " . scalar(@array) . "\n";
print "El primer elemento del array es " . $array[0] . "\n";
print "El último elemento del array es " . $array[-1] . "\n";
push(@array, 6);
print "Se ha añadido el elemento 6 al array\n";
pop(@array);
print "Se ha eliminado el último elemento del array\n";

# Realizar operaciones de manipulación de hashes
my %hash = (
    nombre => "Juan",
    apellido => "Pérez",
    edad => 20,
    ciudad => "Madrid",
);

print "El nombre del usuario es " . $hash{'nombre'} . "\n";
print "El apellido del usuario es " . $hash{'apellido'} . "\n";
print "La edad