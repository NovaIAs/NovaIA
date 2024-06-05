**Fonction de tri personnalisée pour une liste d'objets, basée sur une clé arbitraire**

```python
def tri_personnalisé(liste, clé, ordre='croissant'):
    """Trie une liste d'objets selon une clé arbitraire.

    Args:
        liste (list): La liste à trier.
        clé (str): Le nom de l'attribut à utiliser comme clé de tri.
        ordre (str, optionnel): L'ordre de tri ('croissant' ou 'décroissant').

    Returns:
        list: La liste triée.
    """

    def get_clé(obj):
        return getattr(obj, clé)

    if ordre == 'croissant':
        return sorted(liste, key=get_clé)
    else:
        return sorted(liste, key=get_clé, reverse=True)
```

**Exemple d'utilisation**

```python
class Personne:
    def __init__(self, nom, âge):
        self.nom = nom
        self.âge = âge

# Liste de personnes
personnes = [
    Personne("Alice", 25),
    Personne("Bob", 30),
    Personne("Charles", 22),
]

# Tri de la liste par nom (croissant)
personnes_triees_par_nom = tri_personnalisé(personnes, "nom")

# Tri de la liste par âge (décroissant)
personnes_triees_par_âge = tri_personnalisé(personnes, "âge", "décroissant")
```

**Explication**

* La fonction `tri_personnalisé` prend trois arguments :

    * **liste** : La liste à trier.
    * **clé** : Le nom de l'attribut de l'objet à utiliser comme clé de tri.
    * **ordre** : L'ordre de tri (par défaut 'croissant').

* La fonction `get_clé` est une fonction imbriquée qui renvoie la valeur de l'attribut `clé` pour un objet donné.

* Si `ordre` est 'croissant', la liste est triée dans l'ordre croissant de la clé. Si `ordre` est 'décroissant', la liste est triée dans l'ordre décroissant de la clé.

* La fonction `sorted()` est utilisée pour trier la liste en fonction de la clé et de l'ordre spécifiés.