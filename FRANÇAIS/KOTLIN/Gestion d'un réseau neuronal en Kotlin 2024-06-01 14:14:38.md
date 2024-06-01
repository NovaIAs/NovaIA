**Gestion d'un réseau neuronal**

```kotlin
interface Neurone {
    fun calculerSortie(entrees: Array<Double>): Double
    fun mettreAJourPoids(erreurs: Array<Double>, tauxApprentissage: Double)
}

class Perceptron(
    private val poids: Array<Double>,
    private val seuil: Double,
    private val fonctionActivation: (Double) -> Double
) : Neurone {

    override fun calculerSortie(entrees: Array<Double>): Double {
        var sommePonderee = 0.0
        for (i in entrees.indices) {
            sommePonderee += entrees[i] * poids[i]
        }
        return fonctionActivation(sommePonderee - seuil)
    }

    override fun mettreAJourPoids(erreurs: Array<Double>, tauxApprentissage: Double) {
        for (i in poids.indices) {
            poids[i] -= tauxApprentissage * erreurs[i]
        }
    }
}

class Couche(
    private val neurones: Array<Neurone>
) {

    fun calculerSorties(entrees: Array<Double>): Array<Double> {
        val sorties = mutableListOf<Double>()
        for (neurone in neurones) {
            sorties.add(neurone.calculerSortie(entrees))
        }
        return sorties.toTypedArray()
    }

    fun mettreAJourPoids(erreurs: Array<Double>, tauxApprentissage: Double) {
        for (i in neurones.indices) {
            neurones[i].mettreAJourPoids(erreurs[i], tauxApprentissage)
        }
    }
}

class RéseauNeuronal(
    private val couches: Array<Couche>,
    private val fonctionCoût: (sorties: Array<Double>, attendues: Array<Double>) -> Double
) {

    fun entraîner(
        donneesEntraînement: Array<Pair<Array<Double>, Array<Double>>>,
        nbÉpoques: Int,
        tauxApprentissage: Double
    ) {
        for (époque in 0 until nbÉpoques) {
            var coûtTotal = 0.0
            for ((entrees, attendues) in donneesEntraînement) {
                // Calcul des sorties du réseau
                var sorties = entrees
                for (couche in couches) {
                    sorties = couche.calculerSorties(sorties)
                }

                // Calcul de l'erreur
                val erreurs = attendues.mapIndexed { i, attendue -> attendue - sorties[i] }

                // Mise à jour des poids
                for ((i, couche) in couches.withIndex()) {
                    couche.mettreAJourPoids(erreurs.subList(i, i + couche.neurones.size), tauxApprentissage)
                }

                // Calcul du coût
                coûtTotal += fonctionCoût(sorties, attendues)
            }
            println("Époque ${époque + 1} : Coût total = $coûtTotal")
        }
    }

    fun prédire(entrees: Array<Double>): Array<Double> {
        var sorties = entrees
        for (couche in couches) {
            sorties = couche.calculerSorties(sorties)
        }
        return sorties
    }
}
```

**Explication du code**

Ce code implémente un réseau neuronal multicouche pour la classification.

* L'interface `Neurone` définit les méthodes pour calculer la sortie d'un neurone et pour mettre à jour ses poids.
* La classe `Perceptron` implémente un neurone de type perceptron.
* La classe `Couche` regroupe plusieurs neurones.
* La classe `RéseauNeuronal` implémente un réseau neuronal multicouche.

La fonction `entraîner()` permet d'entraîner le réseau neuronal sur un ensemble de données d'entraînement. La fonction `prédire()` permet de prédire les sorties du réseau neuronal pour une entrée donnée.

Le code utilise les fonctions lambdas pour définir la fonction d'activation des neurones et la fonction de coût du réseau neuronal.