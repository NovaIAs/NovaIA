**Gestionnaire de Fichiers Avancé**

```kotlin
class GestionnaireFichiers {

    private val lecteur: BufferReader
    private val ecrivain: BufferWriter

    constructor(fichier: File) {
        lecteur = BufferReader(FileReader(fichier))
        ecrivain = BufferWriter(FileWriter(fichier))
    }

    fun lire(): List<String> {
        val lignes = mutableListOf<String>()
        var ligne: String? = lecteur.readLine()
        while (ligne != null) {
            lignes.add(ligne)
            ligne = lecteur.readLine()
        }
        return lignes
    }

    fun ecrire(lignes: List<String>) {
        lignes.forEach { ligne ->
            ecrivain.write(ligne)
            ecrivain.newLine()
        }
    }

    fun rechercher(motif: String): List<Int> {
        val lignes = lire()
        val resultats = mutableListOf<Int>()
        for ((index, ligne) in lignes.withIndex()) {
            if (ligne.contains(motif)) {
                resultats.add(index)
            }
        }
        return resultats
    }

    fun supprimer(lignesASupprimer: List<Int>) {
        val lignes = lire()
        lignes.removeAll { lignesASupprimer.contains(it) }
        ecrire(lignes)
    }

    fun trier(comparateur: Comparator<String>) {
        val lignes = lire()
        lignes.sort(comparateur)
        ecrire(lignes)
    }

    fun fermeture() {
        lecteur.close()
        ecrivain.close()
    }
}
```

**Explication :**

Ce code implémente un gestionnaire de fichiers avancé qui peut effectuer diverses opérations sur un fichier texte.

* **Lecture (méthode `lire`):** Lit toutes les lignes du fichier dans une liste.
* **Écriture (méthode `ecrire`):** Écrit une liste de lignes dans le fichier.
* **Recherche (méthode `rechercher`):** Recherche une chaîne de caractères dans le fichier et renvoie les indices des lignes correspondantes.
* **Suppression (méthode `supprimer`):** Supprime les lignes spécifiées du fichier.
* **Tri (méthode `trier`):** Trie les lignes du fichier à l'aide d'un comparateur personnalisé.
* **Fermeture (méthode `fermeture`):** Ferme les flux de lecture et d'écriture associés au fichier.