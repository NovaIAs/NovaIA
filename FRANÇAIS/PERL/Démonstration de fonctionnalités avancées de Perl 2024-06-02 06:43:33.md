```perl
# Importation de modules
use feature 'say';
use strict;
use warnings;
use Data::Dumper;

# Définition de la fonction principale
sub main {
    # Déclaration des variables
    my $a = 1;
    my $b = 2;
    my $c = 3;

    # Boucle for imbriquée
    for my $i (1..3) {
        for my $j (1..3) {
            print "$i, $j\n";
        }
    }

    # Condition ternaire
    my $x = $a > $b ? $a : $b;

    # Interpolation de chaîne
    say "La valeur de x est $x";

    # Structure de contrôle switch-case
    my $choice = 1;
    switch ($choice) {
        when (1) { say "Vous avez choisi 1"; }
        when (2) { say "Vous avez choisi 2"; }
        else { say "Choix invalide"; }
    }

    # Table de hachage
    my %table_de_hachage = (
        "un" => 1,
        "deux" => 2,
        "trois" => 3,
    );

    # Itération sur une table de hachage
    foreach my $cle (keys %table_de_hachage) {
        say "$cle => $table_de_hachage{$cle}";
    }

    # Liste de listes
    my @liste_de_listes = (
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9],
    );

    # Itération sur une liste de listes
    foreach my $sous_liste (@liste_de_listes) {
        foreach my $element (@sous_liste) {
            print "$element ";
        }
        print "\n";
    }

    # Tri d'un tableau
    my @tableau_trie = sort { $a <=> $b } @tableau;

    # Appel de sous-routines
    ma_sous_routine($a, $b);

    # Gestion des exceptions
    eval {
        # Code qui peut générer une exception
        do_something_risky();
    };
    if ($@) {
        # Traitement de l'exception
        say "Une exception s'est produite : $@";
    }

    # Expression régulière
    my $texte = "Ceci est un exemple de texte à analyser";
    if ($texte =~ /exemple/) {
        say "Le texte contient le mot 'exemple'";
    }

    # Utilisation de Data::Dumper
    my $structure_complexe = {
        a => 1,
        b => [2, 3, 4],
        c => {
            d => 5,
            e => 6,
        },
    };
    say Dumper($structure_complexe);

    # Utilisation de références
    my $reference_a = \$a;
    **$reference_a = 10;
    say "$a"; # Affiche 10

    my @tableau = (1, 2, 3);
    my $reference_tableau = \@tableau;
    @$reference_tableau[2] = 10;
    say "@tableau"; # Affiche 1, 2, 10

    # Utilisation de blocs
    my $result = {
        map { $_ * 2 } 1, 2, 3
    };
    say "@{$result}"; # Affiche 2, 4, 6

    # Utilisation de closures
    my $fermeture = sub {
        my $a = 10;
        return sub {
            $a++;
            return $a;
        }
    };
    my $compteur = $fermeture->();
    say $compteur->(); # Affiche 11
    say $compteur->(); # Affiche 12
}

# Définition d'une sous-routine
sub ma_sous_routine {
    my $a = $_[0];
    my $b = $_[1];
    say "La somme de $a et $b est $a + $b";
}

# Appel de la fonction principale
main();
```

**Explication du code :**

Ce code Perl est complexe et diversifié, utilisant diverses fonctionnalités du langage. Il comprend :

* Des déclarations de variables, des boucles et des structures de contrôle ;
* Des tables de hachage, des listes de listes et des tableaux ;
* Des appels de sous-routines, la gestion des exceptions et des expressions régulières ;
* L’utilisation de Data::Dumper, de références et de blocs ;
* Des fermetures pour créer des fonctions imbriquées avec état ;

Le code est commenté pour plus de clarté, expliquant chaque section et son objectif.