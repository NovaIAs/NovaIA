**Classe Article**

```smalltalk
Classe Article
    Attributs: nom prix
    Méthodes:
        initialiser: unNom unPrix
            nom <- unNom
            prix <- unPrix

        nom
            ^nom

        prix
            ^prix
```

**Classe Panier**

```smalltalk
Classe Panier
    Attributs: articles

    Méthodes:
        initialiser
            articles <- OrderedCollection new

        ajouter: unArticle
            articles add: unArticle

        total
            ^ articles inject: 0 into: [:total :article | total + article prix]

        articles
            ^articles
```

**Classe Client**

```smalltalk
Classe Client
    Attributs: nom panier

    Méthodes:
        initialiser: unNom
            nom <- unNom
            panier <- Panier new

        ajouterAuPanier: unArticle
            panier ajouter: unArticle

        nom
            ^nom

        panier
            ^panier
```

**Classe Magasin**

```smalltalk
Classe Magasin
    Attributs: articles clients

    Méthodes:
        initialiser
            articles <- articlesCharger
            clients <- clientsCharger

        articles
            ^articles

        clients
            ^clients

        articlesCharger
            ^ {'Livre 1', 'Livre 2', 'Livre 3'} collect: [:nom | Article initialiser: nom prix: 10]

        clientsCharger
            ^ {'Alice', 'Bob'} collect: [:nom | Client initialiser: nom]
```

**Application**

```smalltalk
Object subclass: #Application
    InstanceVariables:
        magasin
    ClassVariables:
        current

    "! Application initialization."
    initialize
        super initialize.
        magasin <- Magasin new.
        Application current := self.
    !^

    "! Run the application."
    run
        Window open: true.
        Window menus: self menu.

    "! Define the menu."
    menu
        Menu new
            add: (MenuItem new label: 'Nouveau client')
            add: (MenuItem new label: 'Nouvel article')
            add: (MenuItem new label: 'Ajouter au panier')
            add: (MenuItem new label: 'Afficher le panier')
            add: (MenuItem new label: 'Quitter')
            value: self.
```

**Explication du code**

* La classe **Article** représente un article avec un nom et un prix.
* La classe **Panier** représente un panier d'articles avec une méthode de calcul du montant total.
* La classe **Client** représente un client avec un nom et un panier.
* La classe **Magasin** représente un magasin avec une liste d'articles et de clients.
* La classe **Application** est le point d'entrée de l'application graphique.
* Le menu de l'application permet d'effectuer des actions telles que l'ajout de clients, d'articles et d'articles au panier.