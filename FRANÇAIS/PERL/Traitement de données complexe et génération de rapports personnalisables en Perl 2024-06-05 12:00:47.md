```perl
# Script en Perl pour traiter une liste complexe de données et générer des rapports personnalisables

use strict;
use warnings;
use Data::Dumper;

# Définir les données d'entrée
my @data = (
    {
        nom      => 'Jean',
        prenom   => 'Dupont',
        age      => 30,
        region   => 'Île-de-France',
    },
    {
        nom      => 'Marie',
        prenom   => 'Martin',
        age      => 25,
        region   => 'Provence-Alpes-Côte d'Azur',
    },
    {
        nom      => 'Pierre',
        prenom   => 'Leroux',
        age      => 40,
        region   => 'Auvergne-Rhône-Alpes',
    },
);

# Définir les paramètres de rapport
my %params = (
    format    => 'csv',   # Format de rapport (csv, json, xml)
    champs    => ['nom', 'prenom', 'age'],  # Champs à inclure dans le rapport
    tri       => {
        colonne => 'nom',  # Colonne de tri
        ordre   => 'asc',  # Ordre de tri (asc, desc)
    },
    filtres   => {
        age     => {
            op => '>',  # Opérateur de filtrage
            valeur => 35,  # Valeur à filtrer
        },
        region  => {
            op => 'in',  # Opérateur de filtrage
            valeurs => ['Île-de-France', 'Provence-Alpes-Côte d'Azur'],  # Valeurs à filtrer
        },
    },
);

# Traiter les données selon les paramètres
my @data_filtered = grep {
    my $ok = 1;
    foreach my $filtre (values %{$params{filtres}}) {
        if ($filtre->{op} eq '>') {
            $ok &&= $_->{$filtre->{colonne}} > $filtre->{valeur};
        } elsif ($filtre->{op} eq '<') {
            $ok &&= $_->{$filtre->{colonne}} < $filtre->{valeur};
        } elsif ($filtre->{op} eq 'in') {
            $ok &&= grep { $_ eq $_->{$filtre->{colonne}} } @{$filtre->{valeurs}};
        }
    }
    $ok;
} @data;

my @data_sorted = sort {
    my $a = $_->{$params{tri}{colonne}};
    my $b = $a->{$params{tri}{ordre}};
    $a cmp $b;
} @data_filtered;

# Générer le rapport en fonction du format
if ($params{format} eq 'csv') {
    my $csv = join(',', @{$params{champs}})."\n";
    foreach my $personne (@data_sorted) {
        $csv .= join(',', map { $personne->{$_} } @{$params{champs}})."\n";
    }
    print $csv;
} elsif ($params{format} eq 'json') {
    my $json = Data::Dumper->Dump(@data_sorted, ['sort']);
    print $json;
} elsif ($params{format} eq 'xml') {
    my $xml = "<data>\n";
    foreach my $personne (@data_sorted) {
        $xml .= "  <personne>\n";
        foreach my $champ (@{$params{champs}}) {
            $xml .= "    <$champ>$personne->{$champ}</$champ>\n";
        }
        $xml .= "  </personne>\n";
    }
    $xml .= "</data>\n";
    print $xml;
} else {
    print "Format de rapport inconnu : $params{format}\n";
}
```