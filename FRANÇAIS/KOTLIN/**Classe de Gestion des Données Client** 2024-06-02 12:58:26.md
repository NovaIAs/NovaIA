**Classe de gestion des données client**

```kotlin
class GestionClients {
    private val clients = mutableListOf<Client>()

    fun ajouterClient(client: Client) {
        clients.add(client)
    }

    fun supprimerClient(client: Client) {
        clients.remove(client)
    }

    fun modifierClient(client: Client, nouveauClient: Client) {
        val index = clients.indexOf(client)
        if (index >= 0) {
            clients[index] = nouveauClient
        }
    }

    fun rechercherClient(critere: String): List<Client> {
        return clients.filter { it.nom.contains(critere) || it.prenom.contains(critere) }
    }

    fun listerClients(): List<Client> {
        return clients.sortedBy { it.nom }
    }
}
```

**Classe représentant un client**

```kotlin
class Client(val nom: String, val prenom: String, val adresse: String) {
    override fun toString(): String {
        return "$nom $prenom - $adresse"
    }
}
```

**Utilisation des classes**

```kotlin
// Créer une instance de la classe de gestion des clients
val gestionClients = GestionClients()

// Ajouter quelques clients
gestionClients.ajouterClient(Client("Dupond", "Jean", "1 rue des Roses"))
gestionClients.ajouterClient(Client("Durand", "Pierre", "2 rue des Lilas"))
gestionClients.ajouterClient(Client("Martin", "Sophie", "3 rue des Champs"))

// Rechercher des clients par nom ou prénom
val clientsTrouves = gestionClients.rechercherClient("Du")

// Afficher les clients trouvés
for (client in clientsTrouves) {
    println("$client")
}
```

**Explications du code**

* La classe `GestionClients` gère une liste mutable de clients.
* La classe `Client` représente un client avec les attributs `nom`, `prenom` et `adresse`.
* La méthode `rechercherClient` utilise la fonction `filter` pour rechercher les clients dont le nom ou le prénom contient le critère donné.
* La méthode `listerClients` trie les clients par leur nom avant de les renvoyer.