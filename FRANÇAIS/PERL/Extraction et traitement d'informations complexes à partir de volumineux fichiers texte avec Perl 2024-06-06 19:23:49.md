**Programme Perl complexe pour extraire et traiter des informations à partir de fichiers texte volumineux (en français)**

```perl
use strict;
use warnings;
use feature 'say';
use Data::Dumper;

# Fonction pour lire un fichier texte et renvoyer son contenu en lignes
sub lire_fichier {
    my ($fichier) = @_;
    open my $fh, '<', $fichier or die "Impossible d'ouvrir le fichier $fichier : $!";
    my @lignes = <$fh>;
    close $fh;
    return @lignes;
}

# Fonction pour extraire les informations pertinentes d'une ligne de texte
sub extraire_informations {
    my ($ligne) = @_;
    my ($date, $heure, $type, $message) = $ligne =~ m/^(\d{4}-\d{2}-\d{2}) (\d{2}:\d{2}:\d{2}) ([A-Z]+) (.*)$/;
    return {
        date => $date,
        heure => $heure,
        type => $type,
        message => $message,
    };
}

# Fonction pour filtrer les informations extraites en fonction de critères
sub filtrer_informations {
    my ($critere, $valeur) = @_;
    return sub {
        my ($infos) = @_;
        return $infos->{$_} eq $valeur for $critere;
    };
}

# Fonction principale pour traiter les informations extraites
sub traiter_informations {
    my @lignes = lire_fichier('fichier.txt');
    my @informations = map { extraire_informations($_) } @lignes;
    my @erreurs = grep { filtrer_informations('type', 'ERREUR') } @informations;
    my @avertissements = grep { filtrer_informations('type', 'AVERTISSEMENT') } @informations;
    my @infos = grep { filtrer_informations('type', 'INFO') } @informations;
    say "\nErreurs :\n", Data::Dumper->Dump(\@erreurs, [qw(sort]));
    say "\nAvertissements :\n", Data::Dumper->Dump(\@avertissements, [qw(sort)]);
    say "\nInformations :\n", Data::Dumper->Dump(\@infos, [qw(sort)]);
}

# Appel de la fonction principale
traiter_informations();
```

**Explications du code :**

* **Fonction `lire_fichier` :** Lit un fichier texte et renvoie un tableau contenant ses lignes.

* **Fonction `extraire_informations` :** Extrait les informations pertinentes (date, heure, type, message) d'une ligne de texte et les renvoie dans un hachage.

* **Fonction `filtrer_informations` :** Retourne une expression de filtrage qui peut être utilisée pour filtrer les informations extraites en fonction d'un critère et d'une valeur spécifiés.

* **Fonction `traiter_informations` :** Traite les informations extraites à partir d'un fichier texte :
    * Lit le fichier et extrait les informations.
    * Filtre les informations par type (erreur, avertissement ou info).
    * Affiche les informations filtrées groupées par type.

* **Appel de la fonction principale :** Appelle la fonction principale pour traiter les informations du fichier spécifié ("fichier.txt" dans cet exemple).