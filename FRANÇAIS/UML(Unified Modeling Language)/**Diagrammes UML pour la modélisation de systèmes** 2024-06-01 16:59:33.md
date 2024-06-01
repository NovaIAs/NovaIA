**Diagramme de cas d'utilisation**

![Diagramme de cas d'utilisation](img/usecase.png)

Le diagramme de cas d'utilisation représente les différents acteurs et leurs interactions avec le système.

* **Acteurs :**
    * Administrateur : Gère le système
    * Utilisateur : Utilise le système
* **Cas d'utilisation :**
    * Gestion des utilisateurs : L'administrateur crée, modifie et supprime des utilisateurs.
    * Gestion des données : L'utilisateur saisit et modifie des données dans le système.

**Diagramme de séquence**

![Diagramme de séquence](img/sequence.png)

Le diagramme de séquence illustre la séquence des interactions entre les objets dans le cas d'utilisation "Gestion des données".

* **Objets :**
    * Formulaire de saisie : Interface utilisateur pour la saisie des données
    * Gestionnaire de données : Gère la persistance des données
    * Base de données : Stocke les données
* **Messages :**
    * L'utilisateur clique sur le bouton "Envoyer" du formulaire de saisie.
    * Le formulaire de saisie envoie les données au gestionnaire de données.
    * Le gestionnaire de données enregistre les données dans la base de données.

**Diagramme de classe**

![Diagramme de classe](img/class.png)

Le diagramme de classe représente les classes et leurs relations dans le système.

* **Classes :**
    * Utilisateur : Représente les informations sur les utilisateurs
    * Données : Représente les données saisies par l'utilisateur
    * Formulaire de saisie : Interface utilisateur pour la saisie des données
    * Gestionnaire de données : Gère la persistance des données
* **Relations :**
    * Association : Lie les utilisateurs aux données qu'ils saisissent
    * Composition : Le formulaire de saisie est composé d'un gestionnaire de données

**Diagramme d'état**

![Diagramme d'état](img/state.png)

Le diagramme d'état représente les différents états d'un objet et les transitions entre ces états.

* **Objet :** Formulaire de saisie
* **États :**
    * Initial
    * Saisie
    * Validation
    * Envoi
* **Transitions :**
    * L'utilisateur clique sur le bouton "Saisir" pour passer de l'état Initial à l'état Saisie.
    * L'utilisateur clique sur le bouton "Valider" pour passer de l'état Saisie à l'état Validation.
    * L'utilisateur clique sur le bouton "Envoyer" pour passer de l'état Validation à l'état Envoi.