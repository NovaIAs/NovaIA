**Code complexe en PHP**

```php
<?php

// Déclaration d'une classe abstraite
abstract class Vehicule {

    // Propriétés protégées (accessibles par les classes enfants)
    protected $marque;
    protected $modele;
    protected $annee;

    // Méthode abstraite (doit être implémentée par les classes enfants)
    abstract public function demarrer();

    // Méthode finale (ne peut pas être surchargée par les classes enfants)
    final public function arreter() {
        echo "Le véhicule s'arrête.";
    }

}

// Classe enfant qui étend la classe abstraite Vehicule
class Voiture extends Vehicule {

    // Propriétés spécifiques à la voiture
    private $nombreDePortes;
    private $typeDeCarburant;

    // Constructeur (méthode spéciale appelée lors de l'instanciation)
    public function __construct($marque, $modele, $annee, $nombreDePortes, $typeDeCarburant) {
        // Appel du constructeur de la classe parente (Vehicule)
        parent::__construct($marque, $modele, $annee);

        // Initialisation des propriétés spécifiques à la voiture
        $this->nombreDePortes = $nombreDePortes;
        $this->typeDeCarburant = $typeDeCarburant;
    }

    // Implémentation de la méthode abstraite demarrer()
    public function demarrer() {
        echo "La voiture démarre.";
    }

    // Méthode spécifique à la voiture
    public function afficherDetails() {
        echo "Marque : " . $this->marque . "<br>";
        echo "Modèle : " . $this->modele . "<br>";
        echo "Année : " . $this->annee . "<br>";
        echo "Nombre de portes : " . $this->nombreDePortes . "<br>";
        echo "Type de carburant : " . $this->typeDeCarburant . "<br>";
    }

}

// Classe enfant qui étend la classe abstraite Vehicule
class Moto extends Vehicule {

    // Propriétés spécifiques à la moto
    private $nombreDeRoues;
    private $cylindree;

    // Constructeur
    public function __construct($marque, $modele, $annee, $nombreDeRoues, $cylindree) {
        // Appel du constructeur de la classe parente (Vehicule)
        parent::__construct($marque, $modele, $annee);

        // Initialisation des propriétés spécifiques à la moto
        $this->nombreDeRoues = $nombreDeRoues;
        $this->cylindree = $cylindree;
    }

    // Implémentation de la méthode abstraite demarrer()
    public function demarrer() {
        echo "La moto démarre.";
    }

    // Méthode spécifique à la moto
    public function afficherDetails() {
        echo "Marque : " . $this->marque . "<br>";
        echo "Modèle : " . $this->modele . "<br>";
        echo "Année : " . $this->annee . "<br>";
        echo "Nombre de roues : " . $this->nombreDeRoues . "<br>";
        echo "Cylindrée : " . $this->cylindree . "<br>";
    }

}

// Utilisation des classes

// Création d'une voiture
$voiture = new Voiture("Peugeot", "208", 2021, 5, "Essence");

// Création d'une moto
$moto = new Moto("Harley-Davidson", "Sportster 883", 2018, 2, 883);

// Affichage des détails de la voiture
echo "<h2>Détails de la voiture</h2>";
$voiture->afficherDetails();

// Démarrage de la voiture
echo "<br><h2>Démarrage de la voiture</h2>";
$voiture->demarrer();

// Arrêt de la voiture
echo "<br><h2>Arrêt de la voiture</h2>";
$voiture->arreter();

// Affichage des détails de la moto
echo "<br><h2>Détails de la moto</h2>";
$moto->afficherDetails();

// Démarrage de la moto
echo "<br><h2>Démarrage de la moto</h2>";
$moto->demarrer();

```

**Explication du code**

Ce code PHP est un exemple de programmation orientée objet avec héritage et polymorphisme.

* **Classe abstraite Vehicule:**
    * Définit une classe abstraite qui fournit une interface commune pour les différentes implémentations de véhicules.
    * Contient une méthode abstraite `demarrer()` qui doit être implémentée par les classes enfants.
    * Contient une méthode finale `arreter()` qui ne peut pas être surchargée par les classes enfants.

* **Classe Voiture:**
    * Étend la classe Vehicule et implémente la méthode `demarrer()` pour les voitures.
    * Possède des propriétés spécifiques aux voitures, telles que le nombre de portes et le type de carburant.
    * Fournit une méthode `afficherDetails()` pour afficher les informations détaillées sur la voiture.

* **Classe Moto:**
    * Étend la classe Vehicule et implémente la méthode `demarrer()` pour les motos.
    * Possède des propriétés spécifiques aux motos, telles que le nombre de roues et la cylindrée.
    * Fournit une méthode `afficherDetails()` pour afficher les informations détaillées sur la moto.

* **Utilisation des classes:**
    * Crée des instances des classes Voiture et Moto pour représenter une voiture et une moto.
    * Affiche les détails des véhicules en appelant la méthode `afficherDetails()`.
    * Démarre et arrête les véhicules en appelant les méthodes `demarrer()` et `arreter()`.

Ce code illustre les concepts de base de la programmation orientée objet en PHP, notamment l'héritage, le polymorphisme et les classes abstraites. Il permet de créer des classes qui partagent des caractéristiques communes (comme la propriété `marque` et la méthode `demarrer()`) tout en ayant des implémentations spécifiques pour différents types de véhicules (voitures et motos).