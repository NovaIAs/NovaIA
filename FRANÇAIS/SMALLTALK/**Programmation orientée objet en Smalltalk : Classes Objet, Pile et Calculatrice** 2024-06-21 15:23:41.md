**Classe Objet**

```smalltalk
Objet subclass: MonObjet [
  | attribut |
  attribut := 0.
]

MonObjet >> initialise [
  super initialise.
]

MonObjet >> attribut [
  ^attribut
]

MonObjet >> ajouter: uneValeur [
  attribut := attribut + uneValeur.
]
```

**Classe Pile**

```smalltalk
Collection subclass: Pile [
  | pile |
  pile := OrderedCollection new.
]

Pile >> initialise [
  super initialise.
]

Pile >> empiler: unObjet [
  pile add: unObjet.
]

Pile >> depiler [
  | dernierObjet |
  dernierObjet := pile last.
  pile remove: dernierObjet.
  ^dernierObjet
]
```

**Classe Calculatrice**

```smalltalk
Calculatrice subclass: MaCalculatrice [
  | pile |
  pile := Pile new.
]

MaCalculatrice >> initialise [
  super initialise.
]

MaCalculatrice >> executer: uneExpression [
  self evaluer: uneExpression.
]

MaCalculatrice >> evaluer: uneExpression [
  uneExpression do: [ :token |
    self executerToken: token ]
]

MaCalculatrice >> executerToken: token [
  token match: '\\d+' ifTrue: [
    | nombre |
    nombre := String >> fromString: token asNumber.
    pile empiler: nombre
  ] ifFalse: [
    token match: '+' ifTrue: [
      pile empiler: pile depiler + pile depiler
    ] ifFalse: [
      token match: '-' ifTrue: [
        pile empiler: pile depiler - pile depiler
      ] ifFalse: [
        token match: '*' ifTrue: [
          pile empiler: pile depiler * pile depiler
        ] ifFalse: [
          token match: '/' ifTrue: [
            | a b c |
            a := pile depiler.
            b := pile depiler.
            c := b / a.
            pile empiler: c
          ]
        ]
      ]
    ]
  ]
]

MaCalculatrice >> resultat [
  ^pile first
]
```

**Exemple d'utilisation**

```smalltalk
| maCalculatrice uneExpression |
maCalculatrice := MaCalculatrice new.
uneExpression := '25 21 + 15 - * 3 /'.
maCalculatrice executer: uneExpression.
maCalculatrice resultat printNl.
```

**Explication du code**

* La classe `Objet` définit un attribut et des méthodes pour le manipuler.
* La classe `Pile` implémente une pile d'objets.
* La classe `Calculatrice` implémente une calculatrice qui utilise une pile pour stocker les valeurs.
* La méthode `evaluer` de la calculatrice analyse une expression et appelle la méthode `executerToken` pour chaque jeton.
* La méthode `executerToken` effectue les opérations arithmétiques en fonction du jeton fourni.
* La méthode `resultat` renvoie le résultat du calcul.

Ce code illustre des concepts de base de la programmation orientée objet, tels que les classes, les attributs, les méthodes et l'héritage. Il démontre également la mise en œuvre d'un calculateur simple à l'aide de ces concepts.