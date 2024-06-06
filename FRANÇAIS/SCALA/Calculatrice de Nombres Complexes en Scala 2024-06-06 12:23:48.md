```scala
object CalculatriceComplexe {

  // Définition des types de données complexes
  type Complex = (Double, Double)

  // Addition de deux nombres complexes
  def add(a: Complex, b: Complex): Complex = {
    (a._1 + b._1, a._2 + b._2)
  }

  // Soustraction de deux nombres complexes
  def subtract(a: Complex, b: Complex): Complex = {
    (a._1 - b._1, a._2 - b._2)
  }

  // Multiplication de deux nombres complexes
  def multiply(a: Complex, b: Complex): Complex = {
    (a._1 * b._1 - a._2 * b._2, a._1 * b._2 + a._2 * b._1)
  }

  // Division de deux nombres complexes
  def divide(a: Complex, b: Complex): Complex = {
    val denominator = b._1 * b._1 + b._2 * b._2
    ((a._1 * b._1 + a._2 * b._2) / denominator, (a._2 * b._1 - a._1 * b._2) / denominator)
  }

  // Calcul de la norme d'un nombre complexe
  def norm(a: Complex): Double = {
    Math.sqrt(a._1 * a._1 + a._2 * a._2)
  }

  // Calcul de l'argument d'un nombre complexe
  def arg(a: Complex): Double = {
    Math.atan2(a._2, a._1)
  }

  // Exemple d'utilisation
  def main(args: Array[String]): Unit = {
    val a = (3.0, 4.0)
    val b = (5.0, -2.0)

    println("Addition : " + add(a, b))
    println("Soustraction : " + subtract(a, b))
    println("Multiplication : " + multiply(a, b))
    println("Division : " + divide(a, b))
    println("Norme : " + norm(a))
    println("Argument : " + arg(a))
  }
}
```

**Explication du code :**

Ce code implémente une calculatrice complexe en Scala. Il définit les opérations arithmétiques courantes (addition, soustraction, multiplication, division) ainsi que les fonctions pour calculer la norme et l'argument d'un nombre complexe.

Voici un aperçu de chaque fonction :

* **add(a: Complex, b: Complex): Complex** : Ajoute deux nombres complexes (représentés comme des tuples de doubles) et renvoie le résultat sous forme de tuple.
* **subtract(a: Complex, b: Complex): Complex** : Soustrait deux nombres complexes et renvoie le résultat sous forme de tuple.
* **multiply(a: Complex, b: Complex): Complex** : Multiplie deux nombres complexes en utilisant les formules de multiplication complexe et renvoie le résultat sous forme de tuple.
* **divide(a: Complex, b: Complex): Complex** : Divise deux nombres complexes en utilisant les formules de division complexe et renvoie le résultat sous forme de tuple.
* **norm(a: Complex): Double** : Calcule la norme (module) d'un nombre complexe en utilisant le théorème de Pythagore.
* **arg(a: Complex): Double** : Calcule l'argument (angle) d'un nombre complexe en utilisant la fonction `atan2`.

La fonction `main` est utilisée pour tester la calculatrice en effectuant des opérations sur deux nombres complexes d'exemple.