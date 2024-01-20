```perl
use utf8;
use strict;
use warnings;

# Definir las funciones

sub saludar {
    my ($nombre) = @_;
    print "Hola, $nombre!\n";
}

sub sumar {
    my ($a, $b) = @_;
    return $a + $b;
}

sub restar {
    my ($a, $b) = @_;
    return $a - $b;
}

sub multiplicar {
    my ($a, $b) = @_;
    return $a * $b;
}

sub dividir {
    my ($a, $b) = @_;
    return $a / $b;
}

# Crear un menú de opciones

print "Elige una opción:\n";
print "1. Saludar\n";
print "2. Sumar\n";
print "3. Restar\n";
print "4. Multiplicar\n";
print "5. Dividir\n";

# Leer la opción del usuario

my $opcion = <STDIN>;
chomp($opcion);

# Ejecutar la opción elegida

if ($opcion == 1) {
    print "Introduce tu nombre: ";
    my $nombre = <STDIN>;
    chomp($nombre);
    saludar($nombre);
} elsif ($opcion == 2) {
    print "Introduce dos números: ";
    my $a = <STDIN>;
    chomp($a);
    my $b = <STDIN>;
    chomp($b);
    print "La suma de los números es: ", sumar($a, $b), "\n";
} elsif ($opcion == 3) {
    print "Introduce dos números: ";
    my $a = <STDIN>;
    chomp($a);
    my $b = <STDIN>;
    chomp($b);
    print "La resta de los números es: ", restar($a, $b), "\n";
} elsif ($opcion == 4) {
    print "Introduce dos números: ";
    my $a = <STDIN>;
    chomp($a);
    my $b = <STDIN>;
    chomp($b);
    print "La multiplicación de los números es: ", multiplicar($a, $b), "\n";
} elsif ($opcion == 5) {
    print "Introduce dos números: ";
    my $a = <STDIN>;
    chomp($a);
    my $b = <STDIN>;
    chomp($b);
    print "La división de los