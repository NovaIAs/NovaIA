**Diagramme de Classes**

```
+-------------------+
| *Personne*         |
+-------------------+
| - id: int          |
| - nom: string      |
| - age: int          |
| - adresse: Adresse  |
+-------------------+

+--------------------+
| *Adresse*         |
+--------------------+
| - id: int           |
| - rue: string        |
| - ville: string      |
| - code_postal: string |
+--------------------+

+--------------------------------------+
| *Commande*                           |
+--------------------------------------+
| - id: int                             |
| - date_commande: LocalDateTime         |
| - montant_total: double              |
| - client: Personne                    |
| - articles_commandes: List<Article>    |
| - etat: EtatCommande                  |
+--------------------------------------+

+----------------------+
| *Article*             |
+----------------------+
| - id: int             |
| - nom: string          |
| - description: string   |
| - prix: double         |
| - stock: int           |
+----------------------+

+--------------------+
| *EtatCommande*     |
+--------------------+
| - EN_COURS          |
| - PAYEE             |
| - ANNULEE           |
| - LIVREE            |
+--------------------+
```

**Diagramme de Séquence**

```
participant Client
participant Serveur

activate Client
Client->Serveur: Envoyer commande
Serveur->Client: Accuser réception de la commande
Serveur: Générer facture
Serveur: Envoyer facture au client
Client: Payer facture
Serveur: Envoyer confirmation de paiement
Serveur: Préparer la commande
Serveur: Expédier la commande
Serveur: Envoyer notification d'expédition
deactivate Client
```

**Diagramme d'Activité**

```
start
[Initialiser la commande]
> [Récupérer les informations du client]
>> [Vérifier l'existence du client]
>> [Créer un nouveau client]
> [Récupérer les articles commandés]
> [Calculer le montant total]
> [Créer la commande]
[Valider la commande]
> [Envoyer la facture au client]
> [Recevoir le paiement]
[Préparer la commande]
> [Récupérer les articles en stock]
> [Emballer les articles]
[Expédier la commande]
> [Générer l'étiquette d'expédition]
> [Expédier la commande]
[Notifier le client]
> [Envoyer la notification d'expédition]
end
```

**Explication**

Ce code UML représente un système de commande en ligne.

* **Diagramme de Classes** : Définit les classes et leurs attributs et méthodes.
* **Diagramme de Séquence** : Montre l'interaction entre les acteurs (Client et Serveur) dans le temps.
* **Diagramme d'Activité** : Décrit le flux d'activités du système de commande.

Le système permet à un client de passer une commande en sélectionnant des articles dans un catalogue et en fournissant ses informations de paiement. Le serveur traite la commande, génère une facture, prépare et expédie la commande, puis notifie le client de l'expédition.