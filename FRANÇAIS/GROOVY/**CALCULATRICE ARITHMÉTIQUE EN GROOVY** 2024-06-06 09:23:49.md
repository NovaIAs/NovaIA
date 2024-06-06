```groovy
// Définition d'une classe `Calculatrice` avec des méthodes pour effectuer des opérations arithmétiques
class Calculatrice {

    // Méthode pour additionner deux nombres
    double additionner(double a, double b) {
        return a + b
    }

    // Méthode pour soustraire deux nombres
    double soustraire(double a, double b) {
        return a - b
    }

    // Méthode pour multiplier deux nombres
    double multiplier(double a, double b) {
        return a * b
    }

    // Méthode pour diviser deux nombres
    double diviser(double a, double b) {
        if (b == 0) {
            throw new IllegalArgumentException("Division par zéro")
        }
        return a / b
    }

    // Méthode pour calculer la puissance d'un nombre
    double puissance(double a, double b) {
        return Math.pow(a, b)
    }

    // Méthode pour calculer la racine carrée d'un nombre
    double racineCarree(double a) {
        if (a < 0) {
            throw new IllegalArgumentException("Racine carrée d'un nombre négatif")
        }
        return Math.sqrt(a)
    }

    // Méthode pour calculer le logarithme d'un nombre
    double logarithme(double a) {
        if (a <= 0) {
            throw new IllegalArgumentException("Logarithme d'un nombre non positif")
        }
        return Math.log10(a)
    }
}

// Création d'une instance de la classe `Calculatrice`
Calculatrice calculatrice = new Calculatrice()

// Utilisation de la calculatrice pour effectuer des opérations arithmétiques
double resultatAddition = calculatrice.additionner(2.5, 3.7)
double resultatSoustraction = calculatrice.soustraire(5.4, 2.1)
double resultatMultiplication = calculatrice.multiplier(1.5, 3.2)
double resultatDivision = calculatrice.diviser(10.0, 2.5)
double resultatPuissance = calculatrice.puissance(2.0, 3.0)
double resultatRacineCarree = calculatrice.racineCarree(4.0)
double resultatLogarithme = calculatrice.logarithme(100.0)

// Affichage des résultats
println("Résultat de l'addition : " + resultatAddition)
println("Résultat de la soustraction : " + resultatSoustraction)
println("Résultat de la multiplication : " + resultatMultiplication)
println("Résultat de la division : " + resultatDivision)
println("Résultat de la puissance : " + resultatPuissance)
println("Résultat de la racine carrée : " + resultatRacineCarree)
println("Résultat du logarithme : " + resultatLogarithme)
```

**Ce code est un exemple complexe d'utilisation du langage GROOVY pour effectuer diverses opérations arithmétiques. Voici une explication détaillée du code :**

**Définition de la classe `Calculatrice` :**

* Cette classe contient des méthodes pour effectuer des opérations arithmétiques courantes telles que l'addition, la soustraction, la multiplication, la division, la puissance, la racine carrée et le logarithme.
* Chaque méthode prend des arguments de type double et renvoie un résultat de type double.
* Les méthodes vérifient également si les arguments sont valides (par exemple, éviter la division par zéro) et lèvent une exception en cas d'invalidité.

**Création d'une instance de la classe `Calculatrice` :**

* Une instance de la classe `Calculatrice` est créée à l'aide du mot-clé `new`.

**Utilisation de la calculatrice pour effectuer des opérations arithmétiques :**

* Les méthodes de la classe `Calculatrice` sont appelées pour effectuer les opérations arithmétiques souhaitées.
* Les résultats sont stockés dans des variables distinctes.

**Affichage des résultats :**

* Les résultats des opérations arithmétiques sont affichés à l'aide de l'instruction `println`.

**Ce code démontre la puissance et la polyvalence du langage GROOVY pour résoudre des problèmes complexes de traitement mathématique.**