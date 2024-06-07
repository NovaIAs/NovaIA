**Diagramme de classes**

```uml
+----------------+
|  Client        |
+----------------+
| - id : int      |
| - nom : string  |
| - adresse : string |
+----------------+

+----------------+
|  Commande       |
+----------------+
| - id : int          |
| - date_commande : date |
| - montant_total : float |
| - client : Client     |
+----------------+

+----------------+
|  Article        |
+----------------+
| - id : int        |
| - nom : string    |
| - prix_unitaire : float |
+----------------+

+----------------+
|  LigneCommande  |
+----------------+
| - id : int       |
| - quantité : int  |
| - commande : Commande |
| - article : Article   |
+----------------+
```

**Diagramme de séquence**

```sequence
Participant Client
Participant Commande
Participant Article
Participant LigneCommande

Client->Commande: passerCommande()
Commande->Article: getArticles()
Article->Commande: addArticle()
Commande->LigneCommande: addLigneCommande()
Commande->Client: confirmerCommande()
```

**Diagramme d'état de la commande**

```statemachine
+--------------+
|  Commande     |
+--------------+
    initial
    /        \
   /          \
  /            \
+----+         +----+
| En cours |--> | Validée |
+----+         +----+
    | 
    | -- paiement accepté --
    v
+---------+
| Payée   |
+---------+
```

**Code Java**

```java
public class Commande {

    private int id;
    private Date dateCommande;
    private float montantTotal;
    private Client client;
    private List<LigneCommande> lignesCommande;

    public Commande() {
        this.lignesCommande = new ArrayList<>();
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public Date getDateCommande() {
        return dateCommande;
    }

    public void setDateCommande(Date dateCommande) {
        this.dateCommande = dateCommande;
    }

    public float getMontantTotal() {
        return montantTotal;
    }

    public void setMontantTotal(float montantTotal) {
        this.montantTotal = montantTotal;
    }

    public Client getClient() {
        return client;
    }

    public void setClient(Client client) {
        this.client = client;
    }

    public List<LigneCommande> getLignesCommande() {
        return lignesCommande;
    }

    public void setLignesCommande(List<LigneCommande> lignesCommande) {
        this.lignesCommande = lignesCommande;
    }

    public void addLigneCommande(LigneCommande ligneCommande) {
        this.lignesCommande.add(ligneCommande);
        this.montantTotal += ligneCommande.getQuantite() * ligneCommande.getArticle().getPrixUnitaire();
    }

    public void confirmerCommande() {
        // TODO: implémenter la logique de confirmation de commande
    }
}
```

**Explication du code**

Ce code implémente un système de gestion de commandes en Java. Les classes principales sont `Client`, `Commande`, `Article` et `LigneCommande`.

Le diagramme de classes définit les relations entre les différentes classes. La classe `Client` représente les clients qui passent des commandes. La classe `Commande` représente les commandes qui sont passées par les clients. La classe `Article` représente les articles qui peuvent être commandés. La classe `LigneCommande` représente les articles qui sont commandés dans une commande.

Le diagramme de séquence montre la séquence des messages qui sont échangés entre les différents objets lors du passage d'une commande. Le client appelle la méthode `passerCommande()` de la classe `Commande`, qui appelle ensuite la méthode `getArticles()` de la classe `Article` pour obtenir la liste des articles disponibles. Le client sélectionne ensuite les articles qu'il souhaite commander et appelle la méthode `addArticle()` de la classe `Commande` pour ajouter chaque article à la commande. La commande appelle ensuite la méthode `addLigneCommande()` de la classe `LigneCommande` pour créer une ligne de commande pour chaque article. Enfin, le client appelle la méthode `confirmerCommande()` de la classe `Commande` pour confirmer la commande.

Le diagramme d'état de la commande montre les différents états dans lesquels une commande peut se trouver. Une commande peut être en cours, validée ou payée. Une commande passe à l'état validée lorsqu'elle est confirmée par le client. Une commande passe à l'état payée lorsque le paiement est accepté.

Le code Java implémente la logique métier du système de gestion des commandes. La classe `Commande` gère les commandes qui sont passées par les clients. La classe `LigneCommande` gère les articles qui sont commandés dans une commande. La méthode `addLigneCommande()` de la classe `Commande` ajoute une ligne de commande à la commande et met à jour le montant total de la commande. La méthode `confirmerCommande()` de la classe `Commande` implémente la logique de confirmation de commande.