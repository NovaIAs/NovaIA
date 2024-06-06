```d
// Définie une interface `IVéhicule` avec une propriété abstraite `démarrer`
interface IVéhicule {
    abstract void démarrer();
}

// Définie une classe concrète `Voiture` qui implémente `IVéhicule`
class Voiture : IVéhicule {
    // Implémentation de la méthode `démarrer` pour `Voiture`
    override void démarrer() {
        // Implémentation spécifique pour démarrer une voiture
    }
}

// Définie une classe abstraite `IMoteur` avec une propriété abstraite `puissance`
abstract class IMoteur {
    abstract int puissance;
}

// Définie une classe concrète `MoteurEssence` qui hérite de `IMoteur`
class MoteurEssence : IMoteur {
    override int puissance = 150;
}

// Définie une classe concrète `MoteurDiesel` qui hérite de `IMoteur`
class MoteurDiesel : IMoteur {
    override int puissance = 200;
}

// Définie une classe concrète `Usine` qui crée des véhicules
class Usine {
    // Méthode pour créer une voiture avec un moteur essence
    Voiture créerVoitureAvecMoteurEssence() {
        Voiture voiture = new Voiture();
        voiture.moteur = new MoteurEssence();
        return voiture;
    }

    // Méthode pour créer une voiture avec un moteur diesel
    Voiture créerVoitureAvecMoteurDiesel() {
        Voiture voiture = new Voiture();
        voiture.moteur = new MoteurDiesel();
        return voiture;
    }
}

// Crée une instance de l'usine
Usine usine = new Usine();

// Crée une voiture avec un moteur essence
Voiture voitureEssence = usine.créerVoitureAvecMoteurEssence();

// Crée une voiture avec un moteur diesel
Voiture voitureDiesel = usine.créerVoitureAvecMoteurDiesel();

// Démarre la voiture avec un moteur essence
voitureEssence.démarrer();

// Démarre la voiture avec un moteur diesel
voitureDiesel.démarrer();
```

**Explication du code :**

Ce code définit une interface `IVéhicule` avec une propriété abstraite `démarrer`. La classe concrète `Voiture` implémente cette interface et fournit une implémentation spécifique pour démarrer une voiture.

Le code définit également une interface abstraite `IMoteur` avec une propriété abstraite `puissance`. Les classes concrètes `MoteurEssence` et `MoteurDiesel` héritent de `IMoteur` et fournissent des implémentations spécifiques pour les moteurs à essence et diesel, respectivement.

La classe `Usine` crée des véhicules. Elle a des méthodes pour créer une voiture avec un moteur essence ou diesel.

Le code crée une instance de l'usine et utilise ses méthodes pour créer une voiture avec un moteur essence et une voiture avec un moteur diesel.

Enfin, le code démarre les deux voitures.