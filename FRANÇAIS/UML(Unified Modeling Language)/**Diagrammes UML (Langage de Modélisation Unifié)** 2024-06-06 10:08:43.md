**Diagramme de Classes**

```
+----------------+     +-------------------+  +------------------------+
| Classe A        |     | Classe B           |  | Classe C                |
+----------------+     +-------------------+  +------------------------+
| - attribut1      |     | - attribut2 | attr3  |  | - attribut4             |
| + méthode1()      |     | + méthode2() |  + méthode5()             |
|                  |     | + méthode3() | attr5  |  | + méthode6()             |
|                  |     |                  |  | + méthode7()             |
|                  |     |-------------------+  |------------------------+
```

**Diagramme de Séquence**

```
Acteur 1 -> Classe A : appeler méthode1()
Classe A -> Classe B : appeler méthode2()
Classe B -> Classe A : renvoyer résultat
Classe A -> Acteur 1 : afficher résultat
```

**Diagramme d'État**

```
+----------------+
| État Initial  |
+----------------+

      +--------------+
      | État 1        |
      +--------------+

            +----------------+            +----------------+
            | Transition 1    |            | Transition 2    |
            +----------------+            +----------------+

      +--------------+
      | État 2        |
      +--------------+

                  +----------------+
                  | Transition 3    |
                  +----------------+

      +--------------+
      | État Final   |
      +--------------+
```

**Diagramme d'Activité**

```
Début

Action 1

Branchement (condition)
    Vrai -> Action 2
    Faux -> Action 3

Action 4

Fusion

Action 5

Fin
```

**Diagramme de Collaboration**

```
+----------------+   +-------------------+
| Objet 1        |   | Objet 2           |
+----------------+   +-------------------+

           +----------------+
           | Association 1    |
           +----------------+

                        +----------------+
                        | Lien 1            |
                        +----------------+
```

**Explication du Code**

* **Diagramme de Classes:** Définit les classes, leurs attributs et méthodes.
* **Diagramme de Séquence:** Représente l'interaction temporelle entre les objets.
* **Diagramme d'État:** Montre les différents états d'un objet et les transitions entre eux.
* **Diagramme d'Activité:** Décrit le flux de travail et les conditions dans un processus.
* **Diagramme de Collaboration:** Affiche comment les objets communiquent et interagissent entre eux.