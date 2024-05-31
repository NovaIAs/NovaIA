**Code complexe en Scala**

```scala
// Importer les bibliothèques nécessaires
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

// Définir un objet principal
object CodeComplexe {

  // Fonction principale
  def main(args: Array[String]): Unit = {

    // Créer un tableau mutable de chaînes
    val tableau = ArrayBuffer("Scala", "est", "un", "langage", "puissant")

    // Filtrer le tableau pour ne conserver que les éléments commençant par "S"
    val tableauFiltre = tableau.filter(_.startsWith("S"))

    // Trier le tableau filtré par ordre alphabétique
    val tableauTrie = tableauFiltre.sorted

    // Créer une expression régulière pour rechercher les mots contenant "a"
    val regex = new Regex(".*a.*")

    // Filtrer le tableau trié pour ne conserver que les éléments correspondant à l'expression régulière
    val tableauAvecA = tableauTrie.filter(regex.matches(_))

    // Afficher le tableau final
    for (element <- tableauAvecA) {
      println(element)
    }
  }
}
```

**Explication du code**

Ce code Scala effectue les opérations suivantes :

1. **Importation des bibliothèques** : Les bibliothèques `ArrayBuffer` et `Regex` sont importées pour travailler respectivement avec des tableaux mutables et des expressions régulières.

2. **Fonction principale** : La fonction `main` est l'entrée principale du programme.

3. **Création d'un tableau mutable** : Un tableau mutable `tableau` est créé et contient les éléments `Scala`, `est`, `un`, `langage` et `puissant`.

4. **Filtrage du tableau** : Le tableau est filtré pour ne conserver que les éléments commençant par "S" à l'aide de la méthode `filter`. Le résultat est enregistré dans le tableau `tableauFiltre`.

5. **Tri du tableau** : Le tableau filtré est trié par ordre alphabétique à l'aide de la méthode `sorted`. Le résultat est enregistré dans le tableau `tableauTrie`.

6. **Création d'une expression régulière** : Une expression régulière `regex` est créée pour rechercher les mots contenant "a".

7. **Filtrage du tableau trié** : Le tableau trié est filtré à l'aide de l'expression régulière pour ne conserver que les éléments correspondant au motif. Le résultat est enregistré dans le tableau `tableauAvecA`.

8. **Affichage du tableau final** : Le tableau final `tableauAvecA` est affiché sur la console à l'aide d'une boucle `for`.