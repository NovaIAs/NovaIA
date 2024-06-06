**Schéma de Classe**

```
+----------------+
| Classe Mère    |
+----------------+
| + attribut1    |
| + attribut2    |
| + méthode1     |
| + méthode2     |
+----------------+

+-----------------+
| Classe Enfant1  |
+-----------------+
| + attribut3    |
| + attribut4    |
| + méthode3     |
| + méthode4     |
+-----------------+

+-----------------+
| Classe Enfant2  |
+-----------------+
| + attribut5    |
| + attribut6    |
| + méthode5     |
| + méthode6     |
+-----------------+
```

**Relations**

* Classe Mère est la superclasse de Classe Enfant1 et Classe Enfant2.
* Classe Enfant1 et Classe Enfant2 sont des sous-classes de Classe Mère.
* Classe Mère a une relation d'héritage avec Classe Enfant1 et Classe Enfant2.
* Classe Enfant1 et Classe Enfant2 ont une relation d'héritage avec Classe Mère.

**Explication**

Le diagramme de classe décrit une hiérarchie de classes avec une classe mère et deux classes enfants. La classe mère possède deux attributs (attribut1 et attribut2) et deux méthodes (méthode1 et méthode2). Les classes enfants héritent des attributs et des méthodes de la classe mère, et possèdent également leurs propres attributs et méthodes supplémentaires.

* Classe Enfant1 possède deux attributs supplémentaires (attribut3 et attribut4) et deux méthodes supplémentaires (méthode3 et méthode4).
* Classe Enfant2 possède deux attributs supplémentaires (attribut5 et attribut6) et deux méthodes supplémentaires (méthode5 et méthode6).

Ce diagramme de classe illustre les concepts de généralisation, de spécialisation et d'héritage en programmation orientée objet.