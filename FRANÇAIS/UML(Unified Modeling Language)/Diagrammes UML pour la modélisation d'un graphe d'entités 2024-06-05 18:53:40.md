**Diagramme de classes**

```
+----------------+
| Classe Relation |
+----------------+
| - id : int      |
| - origine : int  |
| - destination : int |
| - poids : float  |
+----------------+

+-----------------+
| Classe Entité    |
+-----------------+
| - id : int       |
| - nom : string   |
| - description : string |
| - relations : list[Relation] |
+-----------------+

+------------------------+
| Classe GrapheEntités  |
+------------------------+
| - entités : list[Entité]      |
| - relations : list[Relation]  |
| - calculerPlusCourtChemin(...) |
| - calculerComposanteConnexe(...) |
+------------------------+
```

**Diagramme de séquence**

```
participant Client
participant Serveur

Client -> Serveur : Obtenir graphe d'entités
Serveur -> Client : Envoyer graphe d'entités

Client -> Serveur : Calculer plus court chemin entre entités A et B
Serveur -> Client : Envoyer plus court chemin

Client -> Serveur : Calculer composante connexe contenant l'entité C
Serveur -> Client : Envoyer composante connexe
```

**Diagramme de collaboration**

```
                   +---------------------------+
                   |                           |
                   |                           |
                   |    Classe Relation         |
                   |                           |
                   |                           |
                   +---------------------------+

           +----------------+     +----------------+
           | Classe Entité  |     | Classe Entité  |
           +----------------+     +----------------+

                    /|\                   /|\
                  /   \                 /   \
                 /     \               /     \
             +-------------------+ +-------------------+
             |                    |                    |
             |     GrapheEntités   |     GrapheEntités   |
             |                    |                    |
             +-------------------+ +-------------------+

                   /|\                   /|\
                  /   \                 /   \
                 /     \               /     \
            +----------------+     +----------------+
            | Classe Relation |     | Classe Entité  |
            +----------------+     +----------------+
```

**Explication du code**

Le diagramme de classes définit les classes `Relation`, `Entité` et `GrapheEntités`.

La classe `Relation` représente une relation entre deux entités, avec un poids qui indique la force de la relation.

La classe `Entité` représente une entité dans le graphe, avec un nom et une description.

La classe `GrapheEntités` représente un graphe d'entités, avec une liste d'entités et de relations.

Le diagramme de séquence montre la communication entre un client et un serveur pour obtenir un graphe d'entités et effectuer des calculs sur celui-ci.

Le diagramme de collaboration montre les dépendances entre les classes.