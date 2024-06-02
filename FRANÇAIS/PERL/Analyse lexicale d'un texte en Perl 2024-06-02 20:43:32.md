```perl
use strict;
use warnings;
use Data::Dumper;
use List::Util qw(min max);

# Définition des constantes
my $TAILLE_MAX_MOT = 20;
my $TAILLE_MIN_MOT = 3;

# Création d'un dictionnaire de mots valides
my %dictionnaire;
open my $fic_dico, '<', 'dico.txt';
while (my $mot = <$fic_dico>) {
    chomp $mot;
    $dictionnaire{$mot} = 1;
}
close $fic_dico;

# Lecture du texte à analyser
my $texte = join "", <STDIN>;

# Tokenisation du texte
my @mots = $texte =~ /\b[a-zA-Z]{$TAILLE_MIN_MOT,$TAILLE_MAX_MOT}\b/g;

# Suppression des mots non valides
@mots = grep { exists $dictionnaire{$_} } @mots;

# Tri des mots par ordre alphabétique
@mots = sort @mots;

# Calcul des fréquences d'apparition des mots
my %frequences;
foreach my $mot (@mots) {
    $frequences{$mot}++;
}

# Affichage des résultats
print "Voici la liste des mots trouvés dans le texte, triés par ordre alphabétique :\n";
foreach my $mot (sort keys %frequences) {
    print "$mot : $frequences{$mot}\n";
}

# Affichage des statistiques
my $nombre_total_mots = scalar @mots;
my $nombre_mots_uniques = scalar keys %frequences;
my $mot_le_plus_frequent = (sort { $frequences{$b} <=> $frequences{$a} } keys %frequences)[0];
my $nombre_occurrences_mot_le_plus_frequent = $frequences{$mot_le_plus_frequent};
my $mot_le_moins_frequent = (sort { $frequences{$a} <=> $frequences{$b} } keys %frequences)[0];
my $nombre_occurrences_mot_le_moins_frequent = $frequences{$mot_le_moins_frequent};

print "Nombre total de mots : $nombre_total_mots\n";
print "Nombre de mots uniques : $nombre_mots_uniques\n";
print "Mot le plus fréquent : $mot_le_plus_frequent ($nombre_occurrences_mot_le_plus_frequent occurrences)\n";
print "Mot le moins fréquent : $mot_le_moins_frequent ($nombre_occurrences_mot_le_moins_frequent occurrences)\n";
```

**Explications du code :**

* Le code utilise le module `Data::Dumper` pour afficher les données de débogage.
* Le code lit un dictionnaire de mots valides à partir d'un fichier.
* Le code tokenise le texte d'entrée et supprime les mots non valides.
* Le code trie les mots par ordre alphabétique.
* Le code calcule la fréquence d'apparition de chaque mot.
* Le code affiche une liste des mots trouvés dans le texte, triés par ordre alphabétique, ainsi que leur fréquence d'apparition.
* Le code affiche également des statistiques sur le nombre total de mots, le nombre de mots uniques, le mot le plus fréquent et le mot le moins fréquent.