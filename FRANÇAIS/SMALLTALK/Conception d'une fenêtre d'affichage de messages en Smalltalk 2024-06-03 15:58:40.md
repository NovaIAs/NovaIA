**Classe Fenêtre**

```smalltalk
Classe Fenêtre
  super classe Objet

  variable instance
    height: unEntier
    width: unEntier
    title: uneChaîne
    content: uneCollection

  méthode d'instance
    height: uneHauteur
      height := uneHauteur

    init
      super init
      height := 500
      width := 500
      title := 'Fenêtre'
      content := Collection nouvelle

    afficher
      self ouvert: true

    fermer
      self ouvert: false

    ouvert: unBooléen
      unBooléen siVrai: [
        self poser: true
        self actualiser
        self afficher: Système standardOut
      ] siFaux: [
        self fermerFini
      ]

    poser: unBooléen
      unBooléen siVrai: [
        Fenêtre enregistrement instantiate
        self déplacer: Fenêtre positionSouris
      ]

    position: point
      point demander: 2 put: width
      point demander: 1 put: height

    déplacer: point
      self poser: true
      Système d'Événement translater: point - self position

    actualiser
      self content do: [:aMessage | aMessage actualiser]

    afficher: unAfficheur
      unAfficheur afficher: title de Droite
      unAfficheur sautLigne
      unAfficheur afficher: '-' de Droite * width
      unAfficheur sautLigne
      content do: [:aMessage | unAfficheur afficher: aMessage]
      unAfficheur sautLigne
      unAfficheur afficher: '-' de Droite * width

    fermerFini
      self poser: false
      Système d'Événement enregistrer: self
      self actualiser
      self fermer
      super fermerFini
```

**Classe Message**

```smalltalk
Classe Message
  super classe Objet

  variable instance
    auteur: uneChaîne
    date: uneDate
    contenu: uneChaîne

  méthode d'instance
    auteur: unAuteur
      auteur := unAuteur

    contenu: unContenu
      contenu := unContenu

    date: uneDate
      date := uneDate

    afficher
      self afficher: Système standardOut

    afficher: unAfficheur
      unAfficheur afficher: 'Auteur : ', auteur
      unAfficheur sautLigne
      unAfficheur afficher: 'Date : ', date
      unAfficheur sautLigne
      unAfficheur afficher: 'Contenu : ', contenu
```

**Classe Système**

```smalltalk
Classe Système
  super classe Objet

  méthode de classe
    standardOut
      ^File avecNom: 'sysout.txt'

    standardIn
      ^File avecNom: 'sysin.txt'

  méthode d'instance
    d'Événement
      ^d'Événement siVide: [
        d'Événement := Enregistrement nouvelle
      ]

    afficher: unObjet
      standardOut afficher: unObjet
      standardOut sautLigne
```

**Utilisation**

```smalltalk
fenêtre := Fenêtre nouvelle.
fenêtre author: 'Pierre Dupont'.
fenêtre afficher.
fenêtre contenu add: 'Bonjour le monde !'.
fenêtre contenu add: 'Voici un nouveau message.'.
fenêtre actualiser.
fenêtre ouvrir.
fenêtre attendreFermeture.
```

**Explications**

Ce code crée une fenêtre qui peut afficher des messages. Il utilise la bibliothèque d'événements Smalltalk pour gérer les interactions avec l'utilisateur. Voici les principales classes et méthodes utilisées :

* **Classe Fenêtre :** Représente la fenêtre qui affiche les messages.
* **Classe Message :** Représente un message avec un auteur, une date et un contenu.
* **Classe Système :** Fournit des méthodes utilitaires pour interagir avec le système d'exploitation, comme accéder aux entrées et sorties standard.

**Utilisation**

L'exemple d'utilisation crée une fenêtre, ajoute deux messages à son contenu, l'actualise et l'ouvre. La fenêtre attend ensuite que l'utilisateur la ferme.