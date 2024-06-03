**Code JavaScript Complexe**

```javascript
// Définir un objet représentant un graphe orienté
class Graphe {
    constructor() {
        this.sommets = new Map(); // Dictionnaire de sommets avec des listes de prédécesseurs et successeurs
    }

    ajouterSommet(sommet) {
        // Si le sommet n'existe pas, l'ajouter avec des listes vides
        if (!this.sommets.has(sommet)) {
            this.sommets.set(sommet, { predecesseurs: [], successeurs: [] });
        }
    }

    ajouterArc(source, destination) {
        // Vérifier que les deux sommets existent
        if (!this.sommets.has(source) || !this.sommets.has(destination)) {
            throw new Error("Les sommets source ou destination n'existent pas.");
        }

        // Ajouter la destination aux successeurs de la source
        this.sommets.get(source).successeurs.push(destination);

        // Ajouter la source aux prédécesseurs de la destination
        this.sommets.get(destination).predecesseurs.push(source);
    }

    // Algorithme de recherche en profondeur
    parcoursProfondeur(sommetInitial, visiteur) {
        // Initialiser l'ensemble des sommets visités
        const visites = new Set();

        // Appel récursif pour parcourir le graphe
        function visiterProfondeur(sommet) {
            if (visites.has(sommet)) {
                return; // Déjà visité
            }

            visites.add(sommet); // Marquer le sommet comme visité
            visiteur(sommet); // Exécuter la fonction visiteur sur le sommet

            // Parcourir récursivement les successeurs
            const successeurs = this.sommets.get(sommet).successeurs;
            for (const successeur of successeurs) {
                visiterProfondeur.call(this, successeur);
            }
        }

        // Démarrer le parcours depuis le sommet initial
        visiterProfondeur.call(this, sommetInitial);
    }

    // Algorithme de tri topologique
    triTopologique() {
        // Initialiser une file d'attente pour les sommets sans prédécesseur
        const fileAttente = [];

        // Initialiser un ensemble de sommets triés
        const sommetsTries = new Set();

        // Parcourir tous les sommets et ajouter ceux sans prédécesseur à la file d'attente
        this.sommets.forEach((infosSommet, sommet) => {
            if (infosSommet.predecesseurs.length === 0) {
                fileAttente.push(sommet);
            }
        });

        // Tant que la file d'attente n'est pas vide
        while (fileAttente.length > 0) {
            // Défiler le sommet de la file d'attente
            const sommet = fileAttente.shift();

            // Ajouter le sommet aux sommets triés
            sommetsTries.add(sommet);

            // Parcourir les successeurs du sommet
            const successeurs = this.sommets.get(sommet).successeurs;
            for (const successeur of successeurs) {
                // Réduire le nombre de prédécesseurs du successeur
                this.sommets.get(successeur).predecesseurs.splice(this.sommets.get(successeur).predecesseurs.indexOf(sommet), 1);

                // Si le successeur n'a plus de prédécesseur, l'ajouter à la file d'attente
                if (this.sommets.get(successeur).predecesseurs.length === 0) {
                    fileAttente.push(successeur);
                }
            }
        }

        // Vérifier si le graphe est cyclique
        if (sommetsTries.size !== this.sommets.size) {
            throw new Error("Le graphe contient un cycle.");
        }

        // Retourner la liste des sommets triés
        return [...sommetsTries];
    }
}

// Démonstration du graphe orienté
const graphe = new Graphe();
graphe.ajouterSommet("A");
graphe.ajouterSommet("B");
graphe.ajouterSommet("C");
graphe.ajouterSommet("D");
graphe.ajouterSommet("E");
graphe.ajouterArc("A", "B");
graphe.ajouterArc("B", "C");
graphe.ajouterArc("C", "D");
graphe.ajouterArc("D", "E");
graphe.ajouterArc("E", "A");

// Parcours en profondeur du graphe
graphe.parcoursProfondeur("A", (sommet) => {
    console.log(`Visité le sommet : ${sommet}`);
});

// Tri topologique du graphe
const ordreTopolologique = graphe.triTopologique();
console.log("Ordre topologique :", ordreTopolologique);
```

**Explication du Code**

Ce code crée un graphe orienté à l'aide de la classe `Graphe`. Un graphe orienté est une structure de données qui représente des relations unidirectionnelles entre des objets appelés sommets.

Le code définit des fonctions pour ajouter des sommets et des arcs au graphe. Il implémente également deux algorithmes :

* **Parcours en profondeur (DFS)** : Visite récursivement un graphe en partant d'un sommet donné.
* **Tri topologique** : Trie les sommets d'un graphe acyclique (sans cycle) dans un ordre tel que, pour tout arc dans le graphe, le sommet source précède le sommet destination dans l'ordre trié.

**Utilisation du Code**

Dans la démonstration, un graphe orienté cyclique est créé et les deux algorithmes sont appliqués. Le parcours en profondeur affiche l'ordre dans lequel les sommets sont visités, tandis que le tri topologique renvoie une liste ordonnée des sommets.