**Prototype d'une application de tâches complexe en Swift**

**Structure générale:**

```swift
class Tache {
    var titre: String
    var description: String?
    var dateEcheance: Date?
    var priorite: Int
    var estTermine: Bool
}

class GestionnaireTaches {
    var taches: [Tache] = []
    
    func ajouter(tache: Tache) {
        taches.append(tache)
    }
    
    func supprimer(tache: Tache) {
        taches.removeAll(where: { $0 === tache })
    }
    
    func trier(parPriorite: Bool = true) {
        taches.sort(by: { (t1, t2) -> Bool in
            if parPriorite {
                return t1.priorite < t2.priorite
            } else {
                return t1.dateEcheance! < t2.dateEcheance!
            }
        })
    }
}

class ControleurVueListeTaches: UIViewController {
    var gestionnaireTaches: GestionnaireTaches!
    
    @IBOutlet weak var tableView: UITableView!
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        gestionnaireTaches.taches = [
            Tache(titre: "Acheter du lait", description: nil, dateEcheance: nil, priorite: 1, estTermine: false),
            Tache(titre: "Finir le projet", description: "Coder le module de persistance", dateEcheance: Date(), priorite: 5, estTermine: false),
            Tache(titre: "Réserver des billets d'avion", description: "Pour les vacances d'été", dateEcheance: Date(timeIntervalSinceNow: 60 * 60 * 24 * 30), priorite: 3, estTermine: true),
        ]
    }
}

class CelluleTache: UITableViewCell {
    @IBOutlet weak var labelTitre: UILabel!
    @IBOutlet weak var labelDescription: UILabel!
    @IBOutlet weak var labelDateEcheance: UILabel!
    @IBOutlet weak var labelPriorite: UILabel!
    @IBOutlet weak var switchTermine: UISwitch!
}
```

**Explications:**

* **Tache:** Représente une tâche simple avec des propriétés comme le titre, la description, la date d'échéance, la priorité et l'état d'achèvement.
* **GestionnaireTaches:** Gère une collection de tâches, permettant d'ajouter, de supprimer et de trier les tâches.
* **ControleurVueListeTaches:** Contrôleur de vue affichant une liste de tâches. Il récupère les données d'un gestionnaire de tâches et les met à jour dans la vue.
* **CelluleTache:** Cellule de table personnalisée qui affiche les informations sur une tâche.

**Utilisation:**

1. Créez des instances de tâches et ajoutez-les à un gestionnaire de tâches.
2. Affichez les tâches dans une table en utilisant le contrôleur de vue de la liste de tâches.
3. Utilisez les méthodes du gestionnaire de tâches pour trier et manipuler les tâches.
4. Modifiez les propriétés des tâches dans les cellules de table et elles seront mises à jour dans le gestionnaire de tâches.