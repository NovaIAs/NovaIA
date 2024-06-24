```perl
use strict;
use warnings;
use feature 'say';

my $texte = "Ceci est un texte d'exemple qui sera traité par notre programme.";

# Décomposer le texte en mots
my @mots = split / /, $texte;

# Créer un dictionnaire pour compter les occurrences de chaque mot
my %compteur;
foreach my $mot (@mots) {
    $compteur{$mot}++ if $mot;  # Ignorer les mots vides
}

# Trier le dictionnaire par ordre alphabétique des mots
my @mots_tries = sort { $a cmp $b } keys %compteur;

# Afficher les mots et leur nombre d'occurrences
foreach my $mot (@mots_tries) {
    say "$mot : $compteur{$mot}";
}

# Calculer la moyenne des longueurs des mots
my $somme_longueurs = 0;
foreach my $mot (@mots) {
    $somme_longueurs += length($mot);
}
my $nombre_mots = scalar @mots;
my $longueur_moyenne = $somme_longueurs / $nombre_mots;

# Afficher la longueur moyenne des mots
say "\nLongueur moyenne des mots : $longueur_moyenne";

# Trouver le mot le plus long
my $mot_le_plus_long = "";
my $longueur_max = 0;
foreach my $mot (@mots) {
    my $longueur = length($mot);
    if ($longueur > $longueur_max) {
        $mot_le_plus_long = $mot;
        $longueur_max = $longueur;
    }
}

# Afficher le mot le plus long
say "\nMot le plus long : $mot_le_plus_long ($longueur_max caractères)";

# Trouver le mot le plus fréquent
my $mot_le_plus_frequent = "";
my $frequence_max = 0;
foreach my $mot (@mots_tries) {
    my $frequence = $compteur{$mot};
    if ($frequence > $frequence_max) {
        $mot_le_plus_frequent = $mot;
        $frequence_max = $frequence;
    }
}

# Afficher le mot le plus fréquent
say "\nMot le plus fréquent : $mot_le_plus_frequent ($frequence_max occurrences)";
```

**Explication du code :**

Ce code Perl effectue une analyse détaillée d'un texte donné en entrée. Voici les étapes principales :

* Il scinde le texte en mots en utilisant des espaces comme délimiteurs.
* Il compte les occurrences de chaque mot dans un dictionnaire en utilisant une structure de données de hachage.
* Il trie le dictionnaire par ordre alphabétique des mots.
* Il affiche les mots et leur nombre d'occurrences.
* Il calcule la longueur moyenne des mots en additionnant les longueurs de tous les mots et en divisant par le nombre de mots.
* Il trouve le mot le plus long en recherchant le mot avec la plus grande longueur.
* Il trouve le mot le plus fréquent en recherchant le mot avec la fréquence d'occurrence la plus élevée.
* Il affiche les résultats, notamment la longueur moyenne des mots, le mot le plus long et le mot le plus fréquent.