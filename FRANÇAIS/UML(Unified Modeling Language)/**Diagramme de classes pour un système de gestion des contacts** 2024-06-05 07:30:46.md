**Diagramme de classes**

```
+-------------------------------------------------------------------+
| Interface IEntité                                                   |
+-------------------------------------------------------------------+
| + getId() : int                                                     |
+-------------------------------------------------------------------+

+-------------------------------------------------------------------+
| Classe abstraite Entité                                             |
+-------------------------------------------------------------------+
| - id : int                                                           |
| - version : int                                                        |
|                                                                    |
| + Entité(id: int, version: int)                                         |
| - getId() : int                                                     |
| - setVersion() : void                                                  |
+-------------------------------------------------------------------+

+-------------------------------------------------------------------+
| Classe Adresse                                                   |
+-------------------------------------------------------------------+
| - id : int                                                           |
| - version : int                                                        |
| - rue : String                                                          |
| - codePostal : String                                                     |
| - ville : String                                                          |
|                                                                    |
| + Adresse(id: int, version: int, rue: String, codePostal: String,      |
|           ville: String)                                            |
| - getId() : int                                                     |
| - setRue() : void                                                      |
| - setVille() : void                                                    |
| - setCodePostal() : void                                                 |
+-------------------------------------------------------------------+

+-------------------------------------------------------------------+
| Classe Personne                                                    |
+-------------------------------------------------------------------+
| - id : int                                                           |
| - version : int                                                        |
| - nom : String                                                          |
| - prenom : String                                                       |
|                                                                    |
| + Personne(id: int, version: int, nom: String, prenom: String)         |
| - getId() : int                                                     |
| - setNom() : void                                                      |
| - setPrenom() : void                                                   |
+-------------------------------------------------------------------+

+-------------------------------------------------------------------+
| Classe Contact                                                    |
+-------------------------------------------------------------------+
| - id : int                                                           |
| - version : int                                                        |
| - personne : Personne                                                    |
| - adresse : Adresse                                                       |
| - email : String                                                         |
| - telephone : String                                                      |
|                                                                    |
| + Contact(id: int, version: int, personne: Personne, adresse: Adresse,   |
|           email: String, telephone: String)                        |
| - getId() : int                                                     |
| - setPersonne() : void                                                   |
| - setAdresse() : void                                                   |
| - setEmail() : void                                                     |
| - setTelephone() : void                                                  |
+-------------------------------------------------------------------+
```

**Diagramme de séquence**

```
participant Personne
participant Contact

Personne -> Contact: Demande de contact
Contact -> Personne: Renvoi de contact
```

**Diagramme d'activités**

```
                 +---------------------+
                 | Créer un nouveau contact |
                 +---------------------+
                                   |
                   +---------------------+
                   | Initialiser les champs |
                   +---------------------+
                                   |
                   +---------------------+
                   | Sauvegarder le contact |
                   +---------------------+
```

**Explication**

Ce code UML modélise un système de gestion des contacts. Il définit les classes principales suivantes :

* **IEntité** : Interface définissant une méthode `getId()` pour obtenir l'identifiant d'une entité.
* **Entité** : Classe abstraite fournissant une implémentation commune pour les classes d'entité.
* **Adresse** : Classe représentant une adresse postale.
* **Personne** : Classe représentant une personne.
* **Contact** : Classe représentant un contact, qui associe une personne à une adresse et des informations de contact (email, téléphone).

Le diagramme de séquence illustre la séquence d'interactions entre les classes **Personne** et **Contact** lors de la demande et du renvoi d'un contact.

Le diagramme d'activités modélise le processus de création d'un nouveau contact, comprenant les étapes d'initialisation des champs, de sauvegarde du contact et de retour du contact.