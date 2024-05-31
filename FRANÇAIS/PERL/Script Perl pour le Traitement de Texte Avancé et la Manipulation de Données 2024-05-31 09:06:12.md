```perl
use strict;
use warnings;

# Un script Perl complexe qui effectue des opérations de traitement de texte avancées
# et de manipulation de données.

# Déclaration des variables et des structures de données
my %hash_donnees;    # Un hachage pour stocker les données
my @tableau_lignes;  # Un tableau pour stocker les lignes de texte
my $ligne_courante;  # La ligne de texte en cours de traitement

# Lecture du fichier texte
open(mon_fichier, '<', 'fichier.txt') or die "Impossible d'ouvrir le fichier";
while (my $ligne = <mon_fichier>) {
    chomp $ligne;  # Supprime les caractères de fin de ligne
    push @tableau_lignes, $ligne;  # Ajoute la ligne au tableau
}
close(mon_fichier);

# Parcours du tableau des lignes de texte
foreach my $ligne (@tableau_lignes) {
    # Extraction des champs de la ligne
    ($champ1, $champ2, $champ3) = split /\t/, $ligne;  # Séparateur par tabulation

    # Stockage des données dans le hachage
    $hash_donnees{$champ1} = {
        champ2 => $champ2,
        champ3 => $champ3,
    };
}

# Tri des données dans le hachage par champ3
my @cles_triees = sort { $hash_donnees{$a}{'champ3'} <=> $hash_donnees{$b}{'champ3'} } keys %hash_donnees;

# Affichage des données triées
print "Données triées par champ3 :\n";
foreach my $cle (@cles_triees) {
    print "$cle: ", $hash_donnees{$cle}{'champ2'}, " ", $hash_donnees{$cle}{'champ3'}, "\n";
}

# Calcul des statistiques sur les données
my $total_champ3 = 0;
foreach my $valeur_champ3 (values %{$hash_donnees{'champ3'}}) {
    $total_champ3 += $valeur_champ3;
}
my $moyenne_champ3 = $total_champ3 / scalar(keys %hash_donnees);

print "\nStatistiques :\n";
print "Total champ3 : $total_champ3\n";
print "Moyenne champ3 : ", sprintf("%.2f", $moyenne_champ3), "\n";
```

**Explication du code :**

* Ce code lit un fichier texte, extrait les champs des lignes de texte et les stocke dans un hachage.
* Il trie ensuite les données dans le hachage par un champ spécifique.
* Enfin, il affiche les données triées et calcule des statistiques sur les données.
* Le code utilise des techniques avancées telles que le hachage, le tri et les calculs statistiques.