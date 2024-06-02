**Système de gestion de bibliothèque**

**Classe principale : Bibliothèque**

```swift
class Bibliothèque {
    private var livres: [Livre]
    private var emprunts: [Emprunt]
    private var usagers: [Usager]

    // Initialisation
    init() {
        self.livres = []
        self.emprunts = []
        self.usagers = []
    }

    // Ajout d'un livre
    func ajouterLivre(livre: Livre) {
        livres.append(livre)
    }

    // Ajout d'un emprunt
    func enregistrerEmprunt(emprunt: Emprunt) {
        emprunts.append(emprunt)
    }

    // Ajout d'un usager
    func enregistrerUsager(usager: Usager) {
        usagers.append(usager)
    }

    // Recherche d'un livre par titre
    func rechercherLivreParTitre(titre: String) -> Livre? {
        for livre in livres {
            if livre.titre == titre {
                return livre
            }
        }
        return nil
    }

    // Recherche d'un usager par nom
    func rechercherUsagerParNom(nom: String) -> Usager? {
        for usager in usagers {
            if usager.nom == nom {
                return usager
            }
        }
        return nil
    }

    // Vérification de la disponibilité d'un livre
    func estDisponible(livre: Livre) -> Bool {
        for emprunt in emprunts {
            if emprunt.livre == livre && emprunt.dateRetour == nil {
                return false
            }
        }
        return true
    }

    // Emprunt d'un livre
    func emprunterLivre(livre: Livre, usager: Usager) -> Emprunt? {
        if estDisponible(livre: livre) {
            let emprunt = Emprunt(livre: livre, usager: usager)
            enregistrerEmprunt(emprunt: emprunt)
            return emprunt
        } else {
            return nil
        }
    }

    // Retour d'un livre
    func retournerLivre(emprunt: Emprunt) {
        emprunt.dateRetour = Date()
    }
}
```

**Classe : Livre**

```swift
class Livre {
    private var titre: String
    private var auteur: String

    // Initialisation
    init(titre: String, auteur: String) {
        self.titre = titre
        self.auteur = auteur
    }

    // Accesseurs
    var Titre: String {
        get {
            return titre
        }
    }
    var Auteur: String {
        get {
            return auteur
        }
    }
}
```

**Classe : Usager**

```swift
class Usager {
    private var nom: String
    private var prenom: String

    // Initialisation
    init(nom: String, prenom: String) {
        self.nom = nom
        self.prenom = prenom
    }

    // Accesseurs
    var Nom: String {
        get {
            return nom
        }
    }
    var Prenom: String {
        get {
            return prenom
        }
    }
}
```

**Classe : Emprunt**

```swift
class Emprunt {
    private var livre: Livre
    private var usager: Usager
    private var dateEmprunt: Date?
    private var dateRetour: Date?

    // Initialisation
    init(livre: Livre, usager: Usager) {
        self.livre = livre
        self.usager = usager
        dateEmprunt = Date()
    }

    // Accesseurs
    var Livre: Livre {
        get {
            return livre
        }
    }
    var Usager: Usager {
        get {
            return usager
        }
    }
    var DateEmprunt: Date? {
        get {
            return dateEmprunt
        }
    }
    var DateRetour: Date? {
        get {
            return dateRetour
        }
        set {
            dateRetour = value
        }
    }
}
```

**Exemple d'utilisation**

```swift
// Création d'une bibliothèque
let bibliotheque = Bibliothèque()

// Ajout de livres
bibliotheque.ajouterLivre(livre: Livre(titre: "Le Petit Prince", auteur: "Antoine de Saint-Exupéry"))
bibliotheque.ajouterLivre(livre: Livre(titre: "Le Comte de Monte-Cristo", auteur: "Alexandre Dumas"))

// Ajout d'usagers
bibliotheque.enregistrerUsager(usager: Usager(nom: "Dupont", prenom: "Jean"))
bibliotheque.enregistrerUsager(usager: Usager(nom: "Durand", prenom: "Marie"))

// Emprunt d'un livre
let emprunt1 = bibliotheque.emprunterLivre(livre: bibliotheque.livres[0], usager: bibliotheque.usagers[0])

// Retour d'un livre
bibliotheque.retournerLivre(emprunt: emprunt1!)
```

**Explications**

Ce code permet de gérer une bibliothèque. Il implémente les fonctionnalités essentielles d'une bibliothèque, telles que l'ajout de livres, le suivi des emprunts et la gestion des usagers.

La classe principale, `Bibliothèque`, gère la collection de livres, d'emprunts et d'usagers. Elle expose des méthodes pour effectuer des opérations telles que la recherche de livres et d'usagers, la vérification de la disponibilité des livres et l'enregistrement d'emprunts et de retours.

Les classes `Livre`, `Usager` et `Emprunt` représentent respectivement les objets métier associés. Elles encapsulent les données et le comportement associés à ces objets.

L'exemple d'utilisation démontre comment utiliser la bibliothèque pour enregistrer des livres, des usagers, emprunter et retourner des livres.