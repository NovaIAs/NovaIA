**Module de Tri Avancé et Personnalisable en Perl**

```perl
use strict;
use warnings;

package Sort::Advanced;

sub new {
    my ($class, %args) = @_;
    my $obj = bless {
        %args,
    }, $class;
    return $obj;
}

sub sort {
    my ($self, @values) = @_;
    my $compare = $self->{cmpr} // sub { $_[0] <=> $_[1] };
    my $order = $self->{order} // 'asc';
    my @sorted = my @values = sort {
        my $a = $values[$_a];
        my $b = $values[$_b];
        if ($order eq 'asc') {
            $compare->($a, $b)
        } else {
            $compare->($b, $a)
        }
    } 0 .. $#values;
    return @sorted;
}

sub cmpr {
    my ($self, @values) = @_;
    if (my $cmpr = $self->{cmpr}) {
        return $cmpr->(@values);
    } else {
        return $_[0] <=> $_[1];
    }
}

sub order {
    my ($self, $new_order) = @_;
    if (defined $new_order) {
        if ($new_order eq 'asc' || $new_order eq 'desc') {
            $self->{order} = $new_order;
        } else {
            die "Order doit être 'asc' ou 'desc'";
        }
    }
    return $self->{order};
}

package main;

my $sorter = Sort::Advanced->new(
    cmpr => sub { $_[0]->[0] <=> $_[1]->[0] },
    order => 'asc',
);

my @array = (
    [1, "Alice"],
    [3, "Bob"],
    [2, "Charlie"],
);

print "Tableau d'origine : ", "@array\n";

my @sorted_array = $sorter->sort(@array);

print "Tableau trié (par le premier élément) : ", "@sorted_array\n";

$sorter->order('desc');

my @desc_sorted_array = $sorter->sort(@array);

print "Tableau trié (par le premier élément, ordre décroissant) : ", "@desc_sorted_array\n";
```

**Explication du Code**

Ce code crée un module Perl pour le tri avancé qui permet de trier des tableaux de manière personnalisable.

1. **Package Sort::Advanced** : Définit un nouveau package pour encapsuler la fonctionnalité de tri.

2. **Méthode new** : Crée un nouvel objet de tri avec les arguments spécifiés, y compris une fonction de comparaison et un ordre de tri.

3. **Méthode sort** : Trie un tableau de valeurs.

   - Il utilise la fonction de comparaison `$compare` spécifiée dans l'objet de tri ou une valeur par défaut si aucune n'est fournie.
   - Il trie le tableau dans l'ordre ascendant par défaut, mais prend également en charge l'ordre descendant.

4. **Méthodes cmpr et order** : Gestion de la fonction de comparaison et de l'ordre de tri.

   - `cmpr` permet de définir ou de récupérer la fonction de comparaison personnalisée.
   - `order` permet de définir ou de récupérer l'ordre de tri (ascendant ou descendant).

**Utilisation**

Dans le package `main`, un objet `$sorter` est créé avec une comparaison par défaut (premier élément du tableau) et un ordre ascendant.

- `@sorted_array` trie le tableau d'entrée dans l'ordre ascendant par défaut.
- `@desc_sorted_array` trie le tableau dans l'ordre descendant après avoir défini `$sorter` sur un ordre descendant.