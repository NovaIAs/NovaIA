**Programme de simulation de système de particules complexe**

**Objectif :** Simuler un système de particules interagissant avec des forces de gravité et de répulsion.

**Code :**

```scala
import scala.collection.mutable.ArrayBuffer

class Particule(val masse : Double, val position : Vecteur2D, val vitesse : Vecteur2D) {
  var acceleration = Vecteur2D(0, 0)

  def miseAJour(dt : Double) : Unit = {
    vitesse += acceleration * dt
    position += vitesse * dt
  }
}

class SystemeParticules(var particules : ArrayBuffer[Particule]) {

  def miseAJour(dt : Double) : Unit = {
    for (i <- 0 until particules.length) {
      val particule = particules(i)
      particule.acceleration = Vecteur2D(0, 0)

      for (j <- 0 until particules.length if i != j) {
        val autreParticule = particules(j)
        val distance = (autreParticule.position - particule.position).norme()
        val force = G * particule.masse * autreParticule.masse / (distance * distance)
        val direction = (autreParticule.position - particule.position) / distance

        particule.acceleration += force * direction
      }
    }

    for (particule <- particules) {
      particule.miseAJour(dt)
    }
  }
}

object SimulationParticules {

  // Constantes de simulation
  val G = 6.674e-11 // Constante gravitationnelle

  def main(args: Array[String]): Unit = {

    // Création du système de particules
    val systeme = new SystemeParticules(ArrayBuffer())

    // Ajout de particules au système
    systeme.particules += new Particule(1.0, Vecteur2D(0, 0), Vecteur2D(0, 0))
    systeme.particules += new Particule(1.0, Vecteur2D(1, 0), Vecteur2D(0, 0))
    systeme.particules += new Particule(1.0, Vecteur2D(0, 1), Vecteur2D(0, 0))

    // Boucle de simulation
    for (i <- 0 until 1000) {
      systeme.miseAJour(0.01) // Pas de temps
    }

    // Affichage des résultats
    for (particule <- systeme.particules) {
      println(s"Particule ${particule.masse} : ${particule.position}")
    }
  }
}
```

**Explication du code :**

**Classes et objets :**

* **Particule :** Représente une particule avec des attributs de masse, de position et de vitesse.
* **SystemeParticules :** Représente un système de particules et gère leur mise à jour.
* **SimulationParticules :** Classe d'application contenant les constantes de simulation et la boucle de simulation principale.

**Fonctions :**

* **miseAJour(dt)** (Particule) : Met à jour la position et la vitesse de la particule en fonction de son accélération.
* **miseAJour(dt)** (SystemeParticules) : Met à jour l'accélération de toutes les particules en fonction de la force gravitationnelle entre elles.

**Constantes :**

* **G :** Constante gravitationnelle.

**Boucle de simulation :**

La boucle de simulation principale met à jour le système de particules à chaque pas de temps pendant un certain nombre d'étapes.

**Affichage des résultats :**

Après la simulation, les positions des particules sont affichées à la console.

**Fonctionnalités avancées :**

* Le système peut être étendu pour inclure des forces de répulsion.
* Les particules peuvent avoir des masses et des positions initiales différentes.
* Le pas de temps peut être ajusté pour optimiser la précision et les performances.