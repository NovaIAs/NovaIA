```tcl
# Importer les bibliothèques nécessaires
package require Tcl 8.6
package require Tk 8.6
package require TclOO
package require http

# Créer une classe pour gérer les tâches d'arrière-plan
class ::BackgroundTask {
    variable task
    constructor {} {
        # Créer une nouvelle tâche et la stocker dans la variable task
        set task [new Task]
        task addCommand init
        task configure -periodic 1000
        task start
    }
    destructor {} {
        # Arrêter et détruire la tâche lorsque l'objet est détruit
        task stop
        task delete
    }
    method init {} {
        # Initialiser la tâche
        set taskData [list]
        http::geturl http://example.com -async -data $taskData
    }
    method handleData {data} {
        # Gérer les données reçues de la requête HTTP
        # Analyser les données et mettre à jour l'interface utilisateur
        ...
    }
    method handleError {code message} {
        # Gérer toute erreur lors de la requête HTTP
        # Afficher un message d'erreur à l'utilisateur
        ...
    }
}

# Créer une interface graphique pour afficher les données
proc main {} {
    # Créer une fenêtre
    set root [tk root]
    tk toplevel .root

    # Créer un canevas pour afficher les données
    set canvas [canvas .root]
    canvas create rectangle 0 0 500 500 -fill white
    #canvas configure -scrollregion 0 0 500 500

    # Créer une tâche d'arrière-plan pour récupérer les données
    set task [new ::BackgroundTask]

    # Mettre à jour l'interface utilisateur chaque fois que des données sont reçues
    task bind DataReceived handleData
    task bind ErrorReceived handleError

    # Lancer l'interface graphique
    tk mainloop
}

# Lancer l'application
main
```

**Explication du code :**

**Classe BackgroundTask :**

* Cette classe gère les tâches d'arrière-plan, comme la récupération de données à partir d'une URL.
* Elle utilise la bibliothèque Task pour exécuter des tâches à intervalles réguliers.

**Méthode init :**

* Cette méthode est appelée lorsque la tâche est initialisée.
* Elle crée une requête HTTP asynchrone pour récupérer des données à partir d'une URL.

**Méthodes handleData et handleError :**

* Ces méthodes sont appelées lorsque les données sont reçues ou qu'une erreur se produit pendant la requête HTTP.
* Elles gèrent respectivement les données et les erreurs.

**Interface graphique :**

* La procédure main crée une fenêtre, un canevas et lance une tâche d'arrière-plan.
* La tâche met à jour l'interface utilisateur lorsque des données sont reçues.