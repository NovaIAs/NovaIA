```perl
# Préambule
use strict;
use warnings;
use Math::Trig;
use Math::Complex;
use Data::Dumper;

# Constantes et variables globales
my $pi = 3.14159265358979323846;
my $e  = 2.71828182845904523536;
my @primes = (2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31);

# Fonctions
sub factorielle {
    my $n = shift;
    return 1 if $n == 0;
    return $n * factorielle($n - 1);
}

sub est_nombre_premier {
    my $n = shift;
    return 0 if $n <= 1;
    for my $i (2 .. int(sqrt($n))) {
        return 0 if $n % $i == 0;
    }
    return 1;
}

sub tri_rapide {
    my ($gauche, $droite) = @_;
    my $pivot = $gauche;

    while ($gauche < $droite) {
        while ($gauche < $droite && $gauche->[0] <= $pivot->[0]) {
            $gauche++;
        }

        while ($gauche < $droite && $droite->[0] >= $pivot->[0]) {
            $droite--;
        }

        if ($gauche < $droite) {
            my $temp = $gauche->[0];
            $gauche->[0] = $droite->[0];
            $droite->[0] = $temp;
        }
    }

    $pivot->[0] = $gauche->[0];
    $gauche->[0] = $droite->[0];
    $droite->[0] = $pivot->[0];

    tri_rapide([@gauche[0 .. $gauche - 1]], [@droite[$gauche + 1 .. $#droite]]);

    return [@gauche, $gauche->[0], @droite];
}

# Programme principal
my $tab = [2, 5, 1, 3, 9, 7, 6, 4, 8];
print "Tableau d'origine :\n";
print Data::Dumper->Dump($tab, [qw(depth 1 maxwidth 200)]);

print "\nFactorielle de 5 :\n";
print factorielle(5);

print "\nEst-ce que 17 est un nombre premier ?\n";
print est_nombre_premier(17);

print "\nTableau trié avec le tri rapide :\n";
print Data::Dumper->Dump(tri_rapide($tab, $#[$tab]), [qw(depth 1 maxwidth 200)]);

print "\nCalcul de sinus et cosinus :\n";
print "sin(pi/2) = ", sin($pi/2), "\n";
print "cos(pi/4) = ", cos($pi/4), "\n";

print "\nNombre complexe :\n";
my $z = Math::Complex->new(3, 4);
print "Partie réelle : ", $z->real(), "\n";
print "Partie imaginaire : ", $z->imag(), "\n";
```

**Explication du code :**

Ce code complexe en Perl :

* Déclare et utilise des constantes et des variables.
* Définit des fonctions utiles, notamment pour calculer la factorielle, vérifier si un nombre est premier et effectuer un tri rapide.
* Montre l'utilisation de modules externes pour les opérations trigonométriques et les nombres complexes.
* Effectue un tri rapide sur un tableau, calcule la factorielle, vérifie la primalité et affiche les résultats.
* Travaille avec des nombres complexes en utilisant le module Math::Complex.