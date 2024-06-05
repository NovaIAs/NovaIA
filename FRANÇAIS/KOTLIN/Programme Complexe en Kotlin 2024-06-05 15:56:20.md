**Programme complexe en Kotlin**

```kotlin
// Module principal
module MainModule {

    // Classe principale
    class MainClass {

        // Attributs
        private val attribut1: Int = 0
        private var attribut2: String = ""
        private val listeAttribut3: MutableList<Int> = mutableListOf()
        private val mapAttribut4: MutableMap<String, Int> = mutableMapOf()

        // Méthodes
        fun methode1(param1: Int, param2: String): Unit = TODO()

        fun methode2(param1: Int, param2: String): Unit {
            // Code complexe ici
        }

        fun methode3(): Unit {
            // Code encore plus complexe ici
            for (i in 0..10) {
                listeAttribut3.add(i)
                val nombreAleatoire = (Math.random() * 100).toInt()
                mapAttribut4[nombreAleatoire.toString()] = nombreAleatoire
            }
        }
    }

    // Classe secondaire
    class SecondaryClass {

        // Attributs
        private var attribut1: Boolean = false
        private val attribut2: Double = 0.0

        // Méthodes
        fun methode1(param1: Int, param2: String): Unit {
            // Code complexe ici
            while (attribut1) {
                val resultat = methode2(param1, param2)
                when (resultat) {
                    0 -> println("Résultat nul")
                    1 -> println("Résultat positif")
                    else -> println("Résultat négatif")
                }
            }
        }

        private fun methode2(param1: Int, param2: String): Int = TODO()
    }
}
```

**Explication du code**

Ce code Kotlin est un exemple d'un programme complexe qui illustre plusieurs concepts avancés :

* **Module** : Le code est organisé dans un module nommé `MainModule`, qui encapsule les classes et les fonctions.
* **Classes** : Le code définit deux classes, `MainClass` et `SecondaryClass`, qui représentent des entités distinctes avec leurs propres attributs et méthodes.
* **Attributs** : Les classes ont des attributs privés et publics qui stockent des données.
* **Méthodes** : Les classes ont des méthodes qui effectuent des opérations, telles que le traitement de paramètres, l'exécution de boucles et le retour de valeurs.
* **Structures de données** : Le code utilise des structures de données telles que des listes mutables et des cartes mutables pour stocker et manipuler des collections d'éléments.
* **Boucles et conditions** : Le code utilise des boucles `for` et des instructions `if` et `when` pour contrôler le flux d'exécution.
* **Appels de méthodes** : Les méthodes appellent d'autres méthodes au sein de la même classe ou dans des classes différentes.