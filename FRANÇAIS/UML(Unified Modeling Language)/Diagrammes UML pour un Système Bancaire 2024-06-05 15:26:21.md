**Diagramme de classes**

```uml
+----------------+
| Personne       |
+----------------+
| + id: int      |
| + nom: string  |
| + prenom: string|
+----------------+

+-----------------------------------+
| CompteBancaire                   |
+-----------------------------------+
| + id: int                        |
| + solde: float                    |
| + proprietaire: Personne          |
| + operations: List<Operation>     |
+-----------------------------------+

+-----------------------------+
| Operation                  |
+-----------------------------+
| + id: int                   |
| + type: string               |
| + montant: float             |
| + date: Date                 |
| + compte: CompteBancaire     |
+-----------------------------+
```

**Diagramme de séquence**

```uml
participant Personne
participant CompteBancaire
participant Operation

Personne -> CompteBancaire : Ouvrir un compte
CompteBancaire -> Personne : Fournir un numéro de compte

Personne -> CompteBancaire : Déposer de l'argent
CompteBancaire -> Personne : Mettre à jour le solde

Personne -> CompteBancaire : Retirer de l'argent
CompteBancaire -> Personne : Vérifier le solde
CompteBancaire -> Personne : Mettre à jour le solde
```

**Machine à états**

```uml
[État initial] --> État A
État A --> État B [condition]
État B --> État C [condition]
État C --> État D [condition]
État D --> [État final]
```

**Diagramme d'activité**

```uml
[Activité initiale] --> Activité 1
Activité 1 --> Activité 2
Activité 2 --> Activité 3 [condition]
Activité 3 --> Activité 4
Activité 4 --> [Activité finale]
```

**Diagramme de déploiement**

```uml
+----------------------+
| Serveur d'applications |
+----------------------+
| + Serveur web         |
| + Base de données      |
+----------------------+

+----------------+
| Client          |
+----------------+

Serveur d'applications -- Base de données
Serveur web -- Serveur d'applications
Client -- Serveur web
```

**Explication**

Ce code UML permet de modéliser un système bancaire simplifié.

* **Diagramme de classes** : Définit les classes Personne, CompteBancaire et Operation.
* **Diagramme de séquence** : Décrit les interactions entre les objets Personne, CompteBancaire et Operation.
* **Machine à états** : Modélise le cycle de vie d'un compte bancaire.
* **Diagramme d'activité** : Décrit le processus de retrait d'argent d'un compte bancaire.
* **Diagramme de déploiement** : Représente l'architecture du système bancaire, incluant les serveurs, les clients et les bases de données.