**Diagramme de Classes**

```
+--------------------+
| Personne            |
+--------------------+
| - idPersonne : int  |
| - nom              : string |
| - prenom           : string |
| - dateNaissance    : date   |
+--------------------+

+------------------+
| Adresse           |
+------------------+
| - idAdresse        : int  |
| - rue             : string |
| - codePostal       : int  |
| - ville            : string |
+------------------+

+-----------------------+
| Voiture                |
+-----------------------+
| - idVoiture           : int  |
| - marque              : string |
| - modele             : string |
| - dateMiseEnCirculation: date   |
+-----------------------+

+-------------------------+
| Garage                  |
+-------------------------+
| - idGarage              : int  |
| - nomGarage            : string |
| - adresseGarage        : Adresse |
+-------------------------+

+----------------------------+
| Entretien                 |
+----------------------------+
| - idEntretien             : int  |
| - dateEntretien          : date   |
| - typeEntretien          : string |
| - coutEntretien          : float  |
| - voitureEntretien       : Voiture |
| - garageEntretien        : Garage |
+----------------------------+
```

**Diagramme des Cas d'Utilisation**

```
+---------------------------------------+
| Gestionnaire de Garage et d'Entretien |
+---------------------------------------+

Acteurs :
- Client
- Mécanicien
- Responsable du Garage

Cas d'Utilisation :
- Enregistrer une Personne
- Enregistrer une Adresse
- Enregistrer une Voiture
- Enregistrer un Garage
- Enregistrer un Entretien
- Consulter les Entretiens d'une Voiture
- Consulter les Voitures d'un Garage
```

**Diagramme de Séquence**

```
Participant : Client
Participant : Mécanicien
Participant : Base de Données

Client -> Mécanicien : Demande d'enregistrement d'une Voiture
Mécanicien -> Base de Données : Enregistrement de la Voiture
Mécanicien -> Client : Confirmation d'enregistrement
```

**Diagramme d'Activité**

```
Début
Enregistrer une Personne
Enregistrer une Adresse
Enregistrer une Voiture
Enregistrer un Garage
Enregistrer un Entretien
Fin
```

**Explication du Code**

**Diagramme de Classes** : Ce diagramme représente les classes et leurs attributs de base. Les classes concernées sont Personne, Adresse, Voiture, Garage et Entretien.

**Diagramme des Cas d'Utilisation** : Ce diagramme identifie les acteurs et les cas d'utilisation du système. Les cas d'utilisation représentent les fonctionnalités principales que le système doit fournir.

**Diagramme de Séquence** : Ce diagramme illustre l'interaction entre les participants et la base de données lorsque le client demande l'enregistrement d'une voiture.

**Diagramme d'Activité** : Ce diagramme représente le flux de travail d'enregistrement des informations dans le système.