**Fichier : complex_code.pl**

```perl
# Encodage UTF-8
use strict;
use warnings;
use feature 'say';

# Déclaration de modules personnalisés
use Module::Complex1;
use Module::Complex2;
use Module::Complex3;

# Déclaration de constantes globales
my $CONSTANTE1 = 'Valeur constante 1';
my $CONSTANTE2 = 23.5;
my @ARRAY_CONST = (1, 2, 3, 4, 5);

# Fonction complexe qui effectue plusieurs opérations
sub fonction_complexe {
    my ($param1, $param2) = @_;

    # Utilisation de modules personnalisés
    my $resultat1 = Complex1::calculer($param1, $param2);
    my $resultat2 = Complex2::traiter($resultat1);
    my $resultat3 = Complex3::finaliser($resultat2);

    # Opérations supplémentaires
    my $resultat4 = $resultat3 * $CONSTANTE2;
    my $resultat5 = join(', ', @$ARRAY_CONST);

    # Retourner un tableau de résultats
    return ($resultat4, $resultat5);
}

# Boucle imbriquée avec des conditions complexes
my ($var1, $var2) = (1, 2);
for my $i (1..10) {
    for my $j (1..5) {
        if ($i % 2 == 0 && $j % 3 == 0) {
            ($var1, $var2) = fonction_complexe($var1, $var2);
        } else {
            ($var1, $var2) = ($var2, $var1);
        }
    }
}

# Affichage des résultats
say "Var1 : $var1";
say "Var2 : $var2";
```

**Explication du code :**

Ce code PERL est un exemple complexe qui utilise des modules personnalisés, des constantes globales, des fonctions imbriquées, des boucles conditionnelles et des opérations complexes.

**Modules personnalisés :** Les trois modules personnalisés (Complex1, Complex2 et Complex3) sont utilisés pour effectuer des opérations spécifiques sur les paramètres d'entrée.

**Constantes globales :** Deux constantes globales ($CONSTANTE1 et $CONSTANTE2) sont déclarées et utilisées dans le code pour fournir des valeurs fixes.

**Fonction complexe :** La fonction `fonction_complexe` prend deux paramètres et effectue une série d'opérations :

* Elle utilise les modules personnalisés pour effectuer des calculs
* Elle effectue des opérations supplémentaires sur les résultats
* Elle renvoie un tableau contenant les résultats

**Boucle imbriquée :** Les boucles imbriquées utilisent des conditions complexes pour modifier les valeurs des variables `$var1` et `$var2`.

**Affichage des résultats :** Une fois les boucles terminées, les valeurs finales de `$var1` et `$var2` sont affichées à la console.