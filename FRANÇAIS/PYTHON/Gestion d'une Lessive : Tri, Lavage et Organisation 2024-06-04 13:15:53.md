```python
from typing import Union, List, Dict, Tuple, Optional
from dataclasses import dataclass
from enum import Enum

class EtatVêtement(Enum):
    """Énumération représentant les états possibles d'un vêtement."""
    PROPRE = 1
    SALE = 2
    DETERIORE = 3

@dataclass
class Vêtement:
    """Classe représentant un vêtement.

    Attributs:
        nom (str): Nom du vêtement.
        etat (EtatVêtement): État du vêtement.
        prix_achat (float): Prix d'achat du vêtement.
    """
    nom: str
    etat: EtatVêtement
    prix_achat: float

    def __str__(self) -> str:
        """Méthode de représentation du vêtement sous forme de chaîne de caractères."""
        return f'{self.nom} ({self.etat.name})'

@dataclass
class MachineALaver:
    """Classe représentant une machine à laver.

    Attributs:
        capacite_max (int): Capacité maximale de la machine à laver en kilogrammes.
        programmes (List[str]): Liste des programmes de lavage disponibles.
        vêtements (List[Vêtement]): Liste des vêtements actuellement dans la machine.
    """
    capacite_max: int
    programmes: List[str]
    vêtements: List[Vêtement]

    def ajouter_vetement(self, vetement: Vêtement) -> None:
        """Méthode pour ajouter un vêtement à la machine à laver.

        Args:
            vetement (Vêtement): Vêtement à ajouter.
        """
        if self.capacite_restante >= vetement.poids:
            self.vêtements.append(vetement)

    def retirer_vetement(self, vetement: Vêtement) -> None:
        """Méthode pour retirer un vêtement de la machine à laver.

        Args:
            vetement (Vêtement): Vêtement à retirer.
        """
        self.vêtements.remove(vetement)

    def lancer_programme(self, programme: str) -> None:
        """Méthode pour lancer un programme de lavage.

        Args:
            programme (str): Nom du programme de lavage à lancer.
        """
        if programme in self.programmes:
            # Lancer le programme de lavage
            for vetement in self.vêtements:
                vetement.etat = EtatVêtement.PROPRE

    @property
    def capacite_restante(self) -> int:
        """Propriété calculant la capacité restante de la machine à laver en kilogrammes."""
        return self.capacite_max - sum(vetement.poids for vetement in self.vêtements)

class PanierALinge:
    """Classe représentant un panier à linge.

    Attributs:
        vêtements (List[Vêtement]): Liste des vêtements dans le panier.
    """
    def __init__(self):
        self.vêtements: List[Vêtement] = []

    def ajouter_vetement(self, vetement: Vêtement) -> None:
        """Méthode pour ajouter un vêtement au panier.

        Args:
            vetement (Vêtement): Vêtement à ajouter.
        """
        self.vêtements.append(vetement)

    def retirer_vetement(self, vetement: Vêtement) -> None:
        """Méthode pour retirer un vêtement du panier.

        Args:
            vetement (Vêtement): Vêtement à retirer.
        """
        self.vêtements.remove(vetement)

    def trier_vetements(self, etats: List[EtatVêtement]) -> Dict[EtatVêtement, List[Vêtement]]:
        """Méthode pour trier les vêtements dans le panier par état.

        Args:
            etats (List[EtatVêtement]): Liste des états à prendre en compte pour le tri.

        Returns:
            Dict[EtatVêtement, List[Vêtement]]: Dictionnaire contenant les vêtements triés par état.
        """
        return {etat: [vetement for vetement in self.vêtements if vetement.etat == etat] for etat in etats}

class GestionnaireDeLessive:
    """Classe gérant le processus de lessive.

    Attributs:
        machine_a_laver (MachineALaver): Machine à laver utilisée pour le processus de lessive.
        panier_a_linge (PanierALinge): Panier à linge contenant les vêtements à laver.
    """
    def __init__(self, machine_a_laver: MachineALaver, panier_a_linge: PanierALinge):
        self.machine_a_laver = machine_a_laver
        self.panier_a_linge = panier_a_linge

    def lancer_lessive(self) -> None:
        """Méthode lançant le processus de lessive."""

        # Trier les vêtements par état
        vetements_tries = self.panier_a_linge.trier_vetements([EtatVêtement.SALE, EtatVêtement.DETERIORE])

        # Ajouter les vêtements sales à la machine à laver
        for vetement in vetements_tries[EtatVêtement.SALE]:
            self.machine_a_laver.ajouter_vetement(vetement)

        # Lancer le programme de lavage
        self.machine_a_laver.lancer_programme('Lavage normal')

        # Retirer les vêtements propres de la machine à laver
        for vetement in self.machine_a_laver.vêtements:
            self.machine_a_laver.retirer_vetement(vetement)

        # Ajouter les vêtements propres au panier
        for vetement in self.machine_a_laver.vêtements:
            self.panier_a_linge.ajouter_vetement(vetement)


# Créer une instance de gestionnaire de lessive
gestionnaire_lessive = GestionnaireDeLessive(MachineALaver(10, ['Lavage normal', 'Lavage délicat'], []), PanierALinge())

# Ajouter des vêtements au panier
gestionnaire_lessive.panier_a_linge.ajouter_vetement(Vêtement('Chemise', EtatVêtement.SALE, 1.5))
gestionnaire_lessive.panier_a_linge.ajouter_vetement(Vêtement('Pantalon', EtatVêtement.PROPRE, 2.0))
gestionnaire_lessive.panier_a_linge.ajouter_vetement(Vêtement('T-shirt', EtatVêtement.DETERIORE, 0.5))

# Lancer le processus de lessive
gestionnaire_lessive.lancer_lessive()

# Afficher les vêtements propres
print("Vêtements propres :")
for vetement in gestionnaire_lessive.panier_a_linge.vêtements:
    print(f'- {vetement}')
```

**Explication du code :**

Ce code implémente un système de gestion de lessive en Python. Il comprend les classes suivantes :

* **`Vêtement`** : Représente un vêtement avec son nom, son état et son prix d'achat.
* **`MachineALaver`** : Représente une machine à laver avec sa capacité maximale, ses programmes de lavage et une liste de vêtements actuellement dans la machine.
* **`PanierALinge`** : Représente un panier à linge contenant une liste de vêtements.
* **`GestionnaireDeLessive`** : Gère le processus de lessive en interagissant avec la machine à laver et le panier à linge.

Le code crée une instance de `GestionnaireDeLessive` et ajoute des vêtements au panier à linge. Le gestionnaire lance ensuite le processus de lessive, qui tri les vêtements, ajoute les vêtements sales à la machine à laver, lance un programme de lavage, retire les vêtements propres de la machine et les ajoute au panier.

Enfin, le code affiche les vêtements propres.