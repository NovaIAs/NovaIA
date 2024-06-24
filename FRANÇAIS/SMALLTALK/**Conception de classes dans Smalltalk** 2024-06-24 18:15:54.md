**Définition d'une classe**

```smalltalk
Classe VueBillet : Fenêtre {

    | billets b1 b2 b3 |

    initialiser
        "Constructeur de la classe."
        super initialiser largeur: 300 hauteur: 100.
        billets := Collection créer.
        b1 := Billet créer avecTitre: 'Billet 1' avecTexte: 'Voici le texte du billet 1.'.
        b2 := Billet créer avecTitre: 'Billet 2' avecTexte: 'Voici le texte du billet 2.'.
        b3 := Billet créer avecTitre: 'Billet 3' avecTexte: 'Voici le texte du billet 3.'.
        billets ajouter: b1.
        billets ajouter: b2.
        billets ajouter: b3.
        self ajouterSousVue: b1.
        self ajouterSousVue: b2.
        self ajouterSousVue: b3.
        self miseÀJour.

    miseÀJour
        "Met à jour l'affichage des billets."
        b1 position: Point x: 10 y: 10.
        b2 position: Point x: 110 y: 10.
        b3 position: Point x: 210 y: 10.
}
```

**Classe Billet**

```smalltalk
Classe Billet : VueTexte {

    | titre texte |

    initialiser: unTitre avecTexte: unTexte
        "Constructeur de la classe."
        super initialiser avecTexte: unTexte.
        titre := unTitre.
        super titre: titre; cadre: true.
        police: Police par défaut gras 12.
        self miseÀJour.

    miseÀJour
        "Met à jour l'affichage du billet."
        super miseÀJour.
        bordure: Bordure barreSimple.
        self traits: Collection créer.
        fenêtre parent titre: titre.
}
```

**Classe Point**

```smalltalk
Classe Point {

    | x y |

    initialiser: unX uneY
        "Constructeur de la classe."
        x := unX.
        y := uneY.

    afficher
        "Affiche les coordonnées du point."
        Impression afficher: (x commeString), espace, (y commeString).
}
```

**Programme principal**

```smalltalk
VueBillet instance ouvrir.
```

**Explication du code**

* La classe `VueBillet` définit la fenêtre principale de l'application qui contient trois billets.
* La classe `Billet` définit un composant d'interface graphique qui affiche le titre et le texte d'un billet.
* La classe `Point` est une classe simple qui encapsule les coordonnées d'un point dans un plan.
* Le programme principal crée une instance de `VueBillet` et l'ouvre, lançant ainsi l'application.

Ce code démontre les concepts de base de la programmation dans Smalltalk, notamment l'orientation objet, l'héritage et le polymorphisme.