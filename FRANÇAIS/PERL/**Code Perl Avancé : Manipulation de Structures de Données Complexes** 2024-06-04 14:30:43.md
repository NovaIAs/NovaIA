**Code Perl complexe**

```perl
use strict;
use warnings;

# Déclaration d'un tableau de hachages représentant des personnes
my @personnes = (
    {
        nom => "Dupont",
        prenom => "Jean",
        age => 30,
        adresse => {
            rue => "Rue de la Paix",
            numero => 12,
            code_postal => "75001",
            ville => "Paris"
        },
        hobbies => ["tennis", "lecture", "musique"]
    },
    {
        nom => "Martin",
        prenom => "Marie",
        age => 25,
        adresse => {
            rue => "Avenue des Champs-Élysées",
            numero => 25,
            code_postal => "75008",
            ville => "Paris"
        },
        hobbies => ["cinéma", "voyages", "danse"]
    }
);

# Fonction pour afficher les informations d'une personne
sub afficher_personne {
    my $personne = shift;

    print "Nom : $personne->{nom}\n";
    print "Prénom : $personne->{prenom}\n";
    print "Âge : $personne->{age}\n";

    print "Adresse :\n";
    print " - Rue : $personne->{adresse}{rue}\n";
    print " - Numéro : $personne->{adresse}{numero}\n";
    print " - Code postal : $personne->{adresse}{code_postal}\n";
    print " - Ville : $personne->{adresse}{ville}\n";

    print "Hobbies :\n";
    foreach my $hobby (@{$personne->{hobbies}}) {
        print " - $hobby\n";
    }

    print "\n";
}

# Afficher les informations de chaque personne
foreach my $personne (@personnes) {
    afficher_personne($personne);
}

# Trier les personnes par âge
my @personnes_triees_par_age = sort { $a->{age} <=> $b->{age} } @personnes;

# Afficher les informations des personnes triées par âge
print "Personnes triées par âge :\n";
foreach my $personne (@personnes_triees_par_age) {
    afficher_personne($personne);
}
```

**Explication du code**

Ce code Perl définit un tableau de hachages représentant des personnes, avec leur nom, prénom, âge, adresse et hobbies. Il contient une fonction pour afficher les informations d'une personne et l'utilise pour afficher les informations de chaque personne du tableau. Il trie ensuite les personnes par âge et affiche à nouveau les informations des personnes triées.

* **Déclaration des modules** :

```perl
use strict;
use warnings;
```

Ces modules activent le mode strict et les avertissements, ce qui aide à détecter les erreurs de codage et les mauvaises pratiques.

* **Déclaration du tableau de hachages** :

```perl
my @personnes = (
    # ...
);
```

Ce tableau contient deux hachages, chacun représentant une personne.

* **Fonction pour afficher les informations d'une personne** :

```perl
sub afficher_personne {
    # ...
}
```

Cette fonction prend un hachage représentant une personne en argument et affiche ses informations sur la console.

* **Affichage des informations de chaque personne** :

```perl
foreach my $personne (@personnes) {
    afficher_personne($personne);
}
```

Cette boucle itère sur chaque personne du tableau et appelle la fonction pour afficher ses informations.

* **Tri des personnes par âge** :

```perl
my @personnes_triees_par_age = sort { $a->{age} <=> $b->{age} } @personnes;
```

Cette ligne trie le tableau de personnes par âge en utilisant la fonction de comparaison lambda `$a->{age} <=> $b->{age}`.

* **Affichage des informations des personnes triées** :

```perl
print "Personnes triées par âge :\n";
foreach my $personne (@personnes_triees_par_age) {
    afficher_personne($personne);
}
```

Cette boucle itère sur le tableau trié et appelle la fonction pour afficher les informations de chaque personne triée.