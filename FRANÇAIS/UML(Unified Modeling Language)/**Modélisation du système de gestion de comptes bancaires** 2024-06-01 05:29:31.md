**Diagramme de classes**

```
+---------------------+
|     Personne        |
+---------------------+
| - nom: String        |
| - prénom: String     |
| - date_naissance: Date |
+---------------------+

+------------------------+
|     Adresse           |
+------------------------+
| - numéro: int          |
| - rue: String          |
| - ville: String        |
| - code_postal: String   |
| - complément: String    |
+------------------------+

+---------------------------+
|     CompteBancaire       |
+---------------------------+
| - numéro: String           |
| - solde: float             |
| - opérationsJournalières: List<Opération> |
+---------------------------+

+---------------------+
|     Opération        |
+---------------------+
| - date: Date           |
| - montant: float         |
| - type: TypeOpération   |
+---------------------+

+-----------------------+
|     TypeOpération     |
+-----------------------+
| - type: String         |
| - sens: SensOpération   |
+-----------------------+

+----------------+
|     SensOpération |
+----------------+
| - débit           |
| - crédit          |
| - paiement        |
| - virement        |
+----------------+
```

**Diagramme de séquence**

```
participant Personne
participant Adresse
participant CompteBancaire
participant Opération

Personne->Adresse: instancier une nouvelle adresse
Adresse->Personne: retourner un objet Adresse

Personne->CompteBancaire: instancier un nouveau compte bancaire
CompteBancaire->Personne: retourner un objet CompteBancaire

Personne->Opération: instancier une nouvelle opération
Opération->CompteBancaire: enregistrer l'opération sur le compte bancaire

Personne->CompteBancaire: obtenir le solde du compte bancaire
CompteBancaire->Personne: retourner le solde
```

**Diagramme de cas d'utilisation**

```
Acteur: Personne

Cas d'utilisation: Gérer ses comptes bancaires

Flux principal:

1. La personne accède à l'application bancaire.
2. La personne sélectionne le compte bancaire qu'elle souhaite gérer.
3. La personne effectue une opération (dépôt, retrait, virement).
4. L'application enregistre l'opération sur le compte bancaire.
5. La personne peut consulter le solde du compte bancaire.

Flux alternatif:

1. La personne ne peut pas accéder à l'application bancaire.
2. La personne sélectionne un compte bancaire qui n'est pas le sien.
3. La personne effectue une opération non autorisée.
```