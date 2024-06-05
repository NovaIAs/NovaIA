**Code Python complexe**

```python
# Définition de la classe Arbre avec des nœuds et des branches
class Arbre:
    def __init__(self, valeur, enfants=[]):
        self.valeur = valeur
        self.enfants = enfants

    def ajouter_enfant(self, enfant):
        self.enfants.append(enfant)

    def imprimer(self):
        print(self.valeur)
        for enfant in self.enfants:
            enfant.imprimer()

# Définition d'une fonction pour créer un arbre binaire de recherche
def creer_arbre_binaire_de_recherche(liste):
    if not liste:
        return None

    racine = Arbre(liste[0])
    for valeur in liste[1:]:
        insérer_dans_arbre_binaire_de_recherche(racine, valeur)

    return racine

# Fonction pour insérer un élément dans un arbre binaire de recherche
def insérer_dans_arbre_binaire_de_recherche(racine, valeur):
    if valeur < racine.valeur:
        if racine.gauche is None:
            racine.gauche = Arbre(valeur)
        else:
            insérer_dans_arbre_binaire_de_recherche(racine.gauche, valeur)
    else:
        if racine.droite is None:
            racine.droite = Arbre(valeur)
        else:
            insérer_dans_arbre_binaire_de_recherche(racine.droite, valeur)

# Fonction pour rechercher un élément dans un arbre binaire de recherche
def rechercher_dans_arbre_binaire_de_recherche(racine, valeur):
    if racine is None:
        return False
    elif racine.valeur == valeur:
        return True
    elif valeur < racine.valeur:
        return rechercher_dans_arbre_binaire_de_recherche(racine.gauche, valeur)
    else:
        return rechercher_dans_arbre_binaire_de_recherche(racine.droite, valeur)

# Fonction pour trouver le plus petit élément d'un arbre binaire de recherche
def trouver_plus_petit_élément(racine):
    if racine is None:
        return None
    elif racine.gauche is None:
        return racine
    else:
        return trouver_plus_petit_élément(racine.gauche)

# Fonction pour supprimer un élément d'un arbre binaire de recherche
def supprimer_dans_arbre_binaire_de_recherche(racine, valeur):
    if racine is None:
        return None
    elif valeur < racine.valeur:
        racine.gauche = supprimer_dans_arbre_binaire_de_recherche(racine.gauche, valeur)
        return racine
    elif valeur > racine.valeur:
        racine.droite = supprimer_dans_arbre_binaire_de_recherche(racine.droite, valeur)
        return racine
    else:
        if racine.gauche is None:
            return racine.droite
        else:
            plus_petit_droite = trouver_plus_petit_élément(racine.droite)
            racine.valeur = plus_petit_droite.valeur
            racine.droite = supprimer_dans_arbre_binaire_de_recherche(racine.droite, plus_petit_droite.valeur)
            return racine

# Exemple d'utilisation
ma_liste = [50, 30, 20, 40, 70, 60, 80]
mon_arbre = creer_arbre_binaire_de_recherche(ma_liste)
mon_arbre.imprimer()  # Imprime l'arbre
rechercher_dans_arbre_binaire_de_recherche(mon_arbre, 40)  # Recherche l'élément 40
supprimer_dans_arbre_binaire_de_recherche(mon_arbre, 40)  # Supprime l'élément 40
mon_arbre.imprimer()  # Réimprime l'arbre
```

**Explication du code**

Le code implémente un arbre binaire de recherche, une structure de données courante pour stocker et rechercher des données. Il définit une classe `Arbre` avec des nœuds (valeurs) et des branches (enfants).

La fonction `creer_arbre_binaire_de_recherche` crée un arbre binaire de recherche à partir d'une liste d'éléments, en insérant chaque élément à sa place appropriée basée sur la valeur de l'élément.

La fonction `insérer_dans_arbre_binaire_de_recherche` insère un élément dans l'arbre, en le plaçant comme enfant gauche ou droit du nœud parent approprié.

La fonction `rechercher_dans_arbre_binaire_de_recherche` recherche un élément dans l'arbre, en retournant `True` s'il est trouvé et `False` sinon.

La fonction `trouver_plus_petit_élément` trouve le plus petit élément de l'arbre.

La fonction `supprimer_dans_arbre_binaire_de_recherche` supprime un élément de l'arbre, en le remplaçant par l'élément le plus petit de son sous-arbre droit si le nœud a deux enfants.

Le code fournit également un exemple d'utilisation, créant un arbre binaire de recherche à partir d'une liste, imprimant l'arbre, recherchant et supprimant un élément, puis réimprimant l'arbre.