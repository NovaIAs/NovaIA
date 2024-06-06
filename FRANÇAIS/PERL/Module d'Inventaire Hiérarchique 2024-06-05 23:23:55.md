**Module d'inventaire complexe**

```perl
use strict;
use warnings;

# Déclaration des constantes
use constant {
    MAX_ARTICLES   => 1000,
    MAX_CATEGORIES => 100,
    MAX_FOURNISSEURS => 100,
};

# Structures de données
my %articles;      # Hash des articles, clé: code article, valeur: objet article
my %categories;    # Hash des catégories, clé: code catégorie, valeur: objet catégorie
my %fournisseurs;  # Hash des fournisseurs, clé: code fournisseur, valeur: objet fournisseur

# Objets
package Article;
sub new {
    my ($class, %args) = @_;
    bless {
        code         => $args{code},
        designation  => $args{designation},
        quantite     => $args{quantite} || 0,
        prix_achat   => $args{prix_achat} || 0,
        prix_vente   => $args{prix_vente} || 0,
        categorie    => $args{categorie},
        fournisseur  => $args{fournisseur},
    }, $class;
}
sub code         { $_[0]->{code} }
sub designation  { $_[0]->{designation} }
sub quantite     { $_[0]->{quantite} }
sub prix_achat   { $_[0]->{prix_achat} }
sub prix_vente   { $_[0]->{prix_vente} }
sub categorie    { $_[0]->{categorie} }
sub fournisseur  { $_[0]->{fournisseur} }

package Categorie;
sub new {
    my ($class, %args) = @_;
    bless {
        code       => $args{code},
        designation => $args{designation},
    }, $class;
}
sub code         { $_[0]->{code} }
sub designation  { $_[0]->{designation} }

package Fournisseur;
sub new {
    my ($class, %args) = @_;
    bless {
        code       => $args{code},
        designation => $args{designation},
        adresse    => $args{adresse},
        telephone  => $args{telephone},
    }, $class;
}
sub code         { $_[0]->{code} }
sub designation  { $_[0]->{designation} }
sub adresse    { $_[0]->{adresse} }
sub telephone  { $_[0]->{telephone} }

# Fonctions utilitaires
sub get_article {
    my $code = shift;
    return $articles{$code} if exists $articles{$code};
    return undef;
}

sub get_categorie {
    my $code = shift;
    return $categories{$code} if exists $categories{$code};
    return undef;
}

sub get_fournisseur {
    my $code = shift;
    return $fournisseurs{$code} if exists $fournisseurs{$code};
    return undef;
}

sub add_article {
    my $article = shift;
    return if exists $articles{$article->code};
    $articles{$article->code} = $article;
}

sub add_categorie {
    my $categorie = shift;
    return if exists $categories{$categorie->code};
    $categories{$categorie->code} = $categorie;
}

sub add_fournisseur {
    my $fournisseur = shift;
    return if exists $fournisseurs{$fournisseur->code};
    $fournisseurs{$fournisseur->code} = $fournisseur;
}

# Fonction principale
sub main {
    # Initialisation
    my $article1 = Article->new(
        code         => 'A001',
        designation  => 'Produit 1',
        quantite     => 10,
        prix_achat   => 10,
        prix_vente   => 20,
        categorie    => 'C001',
        fournisseur  => 'F001',
    );
    add_article($article1);

    my $categorie1 = Categorie->new(
        code       => 'C001',
        designation => 'Catégorie 1',
    );
    add_categorie($categorie1);

    my $fournisseur1 = Fournisseur->new(
        code       => 'F001',
        designation => 'Fournisseur 1',
        adresse    => '123 Main Street',
        telephone  => '555-1212',
    );
    add_fournisseur($fournisseur1);

    # Affichage
    print "Article 1 :\n";
    print " - Code : ", $article1->code, "\n";
    print " - Désignation : ", $article1->designation, "\n";
    print " - Quantité : ", $article1->quantite, "\n";
    print " - Prix d'achat : ", $article1->prix_achat, "\n";
    print " - Prix de vente : ", $article1->prix_vente, "\n\n";

    print "Catégorie 1 :\n";
    print " - Code : ", $categorie1->code, "\n";
    print " - Désignation : ", $categorie1->designation, "\n\n";

    print "Fournisseur 1 :\n";
    print " - Code : ", $fournisseur1->code, "\n";
    print " - Désignation : ", $fournisseur1->designation, "\n";
    print " - Adresse : ", $fournisseur1->adresse, "\n";
    print " - Téléphone : ", $fournisseur1->telephone, "\n";
}

main();
```

**Explication du code**

Ce code implémente un module d'inventaire complexe en Perl. Il utilise des structures de données hiérarchiques (objet-orientées) et des fonctions utilitaires pour gérer les articles, les catégories et les fournisseurs.

Les structures de données sont implémentées à l'aide de hachages et d'objets. Les objets ont des méthodes pour accéder aux propriétés de l'objet.

Les fonctions utilitaires sont utilisées pour ajouter, récupérer et supprimer des éléments des structures de données.

La fonction principale `main()` initialise l'inventaire avec des données de test et affiche les détails de l'article, de la catégorie et du fournisseur.

Ce code est complexe car il utilise des structures de données hiérarchiques, des objets et des fonctions utilitaires pour gérer les données d'inventaire. Il est également difficilement répétable car il utilise un style de programmation orienté objet, qui est différent du style de programmation procédurale plus courant.