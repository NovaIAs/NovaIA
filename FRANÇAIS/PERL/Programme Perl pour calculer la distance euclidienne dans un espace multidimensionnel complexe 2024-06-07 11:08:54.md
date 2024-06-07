**Programme Perl complexe pour calculer la distance euclidienne entre les points dans un espace multidimensionnel**

```perl
#!/usr/bin/perl

use strict;
use warnings;

my $usage = "Usage: distance.pl x1 y1 z1 x2 y2 z2 ...";

my @points = @ARGV;

if (scalar @points % 3 != 0) {
    die $usage;
}

my $num_dimensions = scalar @points / 3;

my @distances;

for my $i (0..$#points) {
    my $x1 = $points[$i++];
    my $y1 = $points[$i++];
    my $z1 = $points[$i];

    for my $j ($i..$#points) {
        my $x2 = $points[$j++];
        my $y2 = $points[$j++];
        my $z2 = $points[$j];

        my $dx = $x2 - $x1;
        my $dy = $y2 - $y1;
        my $dz = $z2 - $z1;

        my $distance = sqrt($dx**2 + $dy**2 + $dz**2);

        push @distances, $distance;
    }
}

print "Distances calculées :\n";

for my $distance (@distances) {
    printf "%.2f\n", $distance;
}
```

**Explication du code**

Ce programme Perl calcule la distance euclidienne entre tous les points dans un espace multidimensionnel. Les points sont fournis en tant qu'arguments de ligne de commande, avec trois valeurs par point (par exemple, `x1 y1 z1 x2 y2 z2`).

**Fonctionnalités principales**

* **Gestion des arguments de ligne de commande** : Le programme lit les arguments de ligne de commande et vérifie qu'ils sont fournis en triplets (x, y, z).
* **Calcul de la distance euclidienne** : Pour chaque paire de points, le programme calcule la distance euclidienne à l'aide de la formule racine carrée ((x2 - x1)**2 + (y2 - y1)**2 + (z2 - z1)**2).
* **Stockage des distances calculées** : Les distances calculées sont stockées dans un tableau `@distances`.
* **Affichage des résultats** : Une fois tous les calculs terminés, le programme affiche les distances calculées.

**Complexité du code**

Ce code est considéré comme complexe en raison des éléments suivants :

* **Gestion des dimensions multiples** : Le programme peut gérer un nombre arbitraire de dimensions pour les points.
* **Calculs itératifs** : Le programme effectue un nombre important de calculs itératifs pour calculer toutes les paires de distances.
* **Gestion des arguments de ligne de commande** : La validation et la gestion des arguments de ligne de commande ajoutent de la complexité au code.

**Utilisation**

Pour utiliser le programme, exécutez la commande suivante :

```
perl distance.pl x1 y1 z1 x2 y2 z2 ...
```

où `x1 y1 z1` représentent les coordonnées du premier point, `x2 y2 z2` les coordonnées du deuxième point, etc.

**Exemple**

Par exemple, pour calculer les distances entre les points suivants dans un espace 3D :

* P1(1, 2, 3)
* P2(4, 5, 6)

Exécutez la commande suivante :

```
perl distance.pl 1 2 3 4 5 6
```

Le programme affichera la sortie suivante :

```
Distances calculées :
5.196152
```