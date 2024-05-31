**Classe Personne**

```uml
+ Personne
    - nom: String
    - prenom: String
    - age: Integer
    - adresse: Adresse
    - contacts: List<Contact>
```

**Classe Adresse**

```uml
+ Adresse
    - rue: String
    - numero: Integer
    - codePostal: String
    - ville: String
```

**Classe Contact**

```uml
+ Contact
    - type: String (email, telephone, etc.)
    - valeur: String
```

**Classe Société**

```uml
+ Société
    - nom: String
    - adresse: Adresse
    - siret: String
    - employes: List<Personne>
```

**Classe Employe**

```uml
+ Employe
    - personne: Personne
    - matricule: String
    - poste: String
    - salaire: Float
```

**Classe Client**

```uml
+ Client
    - personne: Personne
    - numeroClient: Integer
    - commandes: List<Commande>
```

**Classe Commande**

```uml
+ Commande
    - numero: Integer
    - date: Date
    - produits: List<Produit>
    - client: Client
```

**Classe Produit**

```uml
+ Produit
    - reference: String
    - nom: String
    - prix: Float
```

**Diagramme de classes**

```uml
+-----------------+
| + Personne       |
+-----------------+
| - nom           |
| - prenom        |
| - age           |
| - adresse       |
| - contacts      |
+-----------------+

+-----------------+
| + Adresse        |
+-----------------+
| - rue           |
| - numero        |
| - codePostal    |
| - ville         |
+-----------------+

+-----------------+
| + Contact        |
+-----------------+
| - type          |
| - valeur        |
+-----------------+

+-----------------+
| + Société        |
+-----------------+
| - nom           |
| - adresse       |
| - siret         |
| - employes      |
+-----------------+

+-----------------+
| + Employe        |
+-----------------+
| - personne      |
| - matricule     |
| - poste         |
| - salaire       |
+-----------------+

+-----------------+
| + Client        |
+-----------------+
| - personne      |
| - numeroClient  |
| - commandes     |
+-----------------+

+-----------------+
| + Commande       |
+-----------------+
| - numero        |
| - date          |
| - produits      |
| - client        |
+-----------------+

+-----------------+
| + Produit        |
+-----------------+
| - reference     |
| - nom           |
| - prix          |
+-----------------+
```

**Explication du code**

Ce code UML modélise un système simple de gestion d'entreprise. Il définit les classes suivantes :

* **Personne** : Représente les individus, qu'ils soient clients, employés ou autre.
* **Adresse** : Représente les adresses postales.
* **Contact** : Représente les moyens de contacter une personne.
* **Société** : Représente les entreprises.
* **Employe** : Représente les employés d'une entreprise.
* **Client** : Représente les clients de l'entreprise.
* **Commande** : Représente les commandes passées par les clients.
* **Produit** : Représente les produits vendus par l'entreprise.

Le diagramme de classes montre les relations entre ces classes. Par exemple, une **Personne** peut avoir une ou plusieurs **Adresse** et **Contact**. Une **Société** peut avoir plusieurs **Employe** et **Client**. Une **Commande** peut contenir plusieurs **Produit**.