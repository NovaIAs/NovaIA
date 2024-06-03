**Gestion d'un système de billetterie pour événements**

```scala
import java.time.LocalDateTime
import scala.collection.mutable.ListBuffer

class SystemeDeBilletterie {

  private val evenements: ListBuffer[Evenement] = ListBuffer()

  def creerEvenement(nom: String, date: LocalDateTime, capacite: Int): Evenement = {
    val evenement = new Evenement(nom, date, capacite)
    evenements += evenement
    evenement
  }

  def trouverEvenement(nom: String): Option[Evenement] = {
    evenements.find(_.nom == nom)
  }

  def reserverBillet(evenement: Evenement, quantite: Int): Boolean = {
    if (evenement.capacite >= quantite) {
      evenement.capacite -= quantite
      true
    } else {
      false
    }
  }

  def annulerReservation(evenement: Evenement, quantite: Int): Boolean = {
    if (quantite <= evenement.capaciteMax - evenement.capacite) {
      evenement.capacite += quantite
      true
    } else {
      false
    }
  }

  def obtenirCapaciteRestante(evenement: Evenement): Int = {
    evenement.capacite
  }
}

class Evenement(val nom: String, val date: LocalDateTime, val capaciteMax: Int) {
  var capacite: Int = capaciteMax
}
```

**Utilisation du système de billetterie**

```scala
val systemeDeBilletterie = new SystemeDeBilletterie()

val concert = systemeDeBilletterie.creerEvenement("Concert de rock", LocalDateTime.now(), 1000)

systemeDeBilletterie.reserverBillet(concert, 200)
systemeDeBilletterie.annulerReservation(concert, 50)

val capaciteRestante = systemeDeBilletterie.obtenirCapaciteRestante(concert)

println(s"Capacité restante pour le concert : $capaciteRestante")
```