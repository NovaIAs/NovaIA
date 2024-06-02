**Classe de Point**

```smalltalk
Classe Point
    instanceVariable noms: {
        unePosition: Point
    }

    méthodes: {
        créer unePosition: unePosition "Constructeur"
            unePosition à mon unePosition

        récupérer unePosition "Accesseur en lecture"
            mon unePosition

        assigner unePosition: unePosition "Accesseur en écriture"
            mon unePosition à unePosition

        distanceAvec: unPoint "Calcule la distance entre deux points"
            mon unePosition distanceAvec: unPoint
    }
```

**Classe de Rectangle**

```smalltalk
Classe Rectangle
    superClasse: Point

    instanceVariable noms: {
        uneTaille: Point
    }

    méthodes: {
        créer unePosition: unePosition taille: uneTaille "Constructeur"
            super créer unePosition: unePosition
            uneTaille à mon uneTaille

        récupérer uneTaille "Accesseur en lecture"
            mon uneTaille

        assigner uneTaille: uneTaille "Accesseur en écriture"
            mon uneTaille à uneTaille

        aire "Calcule l'aire du rectangle"
            (mon uneTaille x mon uneTaille y)
    }
```

**Classe de Cercle**

```smalltalk
Classe Cercle
    superClasse: Point

    instanceVariable noms: {
        unRayon: Number
    }

    méthodes: {
        créer unePosition: unePosition rayon: unRayon "Constructeur"
            super créer unePosition: unePosition
            unRayon à mon unRayon

        récupérer unRayon "Accesseur en lecture"
            mon unRayon

        assigner unRayon: unRayon "Accesseur en écriture"
            mon unRayon à unRayon

        aire "Calcule l'aire du cercle"
            (mon unRayon * mon unRayon) * Math pi
    }
```

**Classe de Forme**

```smalltalk
Classe Forme
    instanceVariable noms: {
        forme: Collection
    }

    méthodes: {
        créer "Constructeur par défaut"
            moi créer uneForme: { }

        créer uneForme: uneForme "Constructeur pour une forme donnée"
            uneForme à mon forme

        ajouter: uneForme "Ajoute une forme à la collection de formes"
            mon forme ajouter: uneForme

        aire "Calcule l'aire totale de toutes les formes"
            mon forme collect: [:uneForme | uneForme aire] sum
    }
```

**Utilisation des classes**

```smalltalk
| point gauche, haut |

point gauche := Point créer unePosition: (10@10).
point haut := Point créer unePosition: (100@50).

rectangle1 := Rectangle créer unePosition: point gauche taille: (point haut - point gauche).

cercle1 := Cercle créer unePosition: (50@50) rayon: 20.

forme1 := Forme créer.
forme1 ajouter: rectangle1.
forme1 ajouter: cercle1.

 aireDeForme := forme1 aire. "Calcule l'aire de la forme"
```