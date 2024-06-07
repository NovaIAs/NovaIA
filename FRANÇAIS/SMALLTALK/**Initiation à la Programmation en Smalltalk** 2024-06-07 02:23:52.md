**Tutoriel de programmation Smalltalk**

Smalltalk est un langage de programmation orienté objet dynamique et réflexif. Il est connu pour sa syntaxe élégante et sa facilité d'apprentissage. Ce tutoriel vous guidera à travers les concepts de base de Smalltalk.

**Variables**

Les variables en Smalltalk sont créées en utilisant le mot-clé `|` suivi du nom de la variable. Par exemple :

```smalltalk
| uneVariable |
```

**Classes et objets**

Les classes définissent la structure et le comportement des objets. Les objets sont des instances de classes. Pour créer une classe, utilisez le mot-clé `class` :

```smalltalk
class Point {
    | x y |
}
```

Pour créer un objet d'une classe, utilisez le mot-clé `new` :

```smalltalk
| unPoint |
unPoint := Point new.
```

**Messages**

Les objets communiquent entre eux en envoyant des messages. Les messages sont des sélecteurs suivis de paramètres, par exemple :

```smalltalk
unPoint getX.
```

Le message `getX` interroge l'objet `unPoint` sur la valeur de sa propriété `x`.

**Méthodes**

Les méthodes sont des blocs de code qui définissent le comportement des objets lorsqu'ils reçoivent des messages. Elles sont définies à l'aide du mot-clé `:` :

```smalltalk
Point class >> getX: unArgument [
    ^ x * unArgument
]
```

Cette méthode multiplie la propriété `x` de l'objet par l'argument `unArgument` et renvoie le résultat.

**Héritage**

Les classes peuvent hériter des propriétés et du comportement d'autres classes. Pour créer une classe héritée, utilisez le mot-clé `subclass` :

```smalltalk
class Point3D : subclass [Point] {
    | z |
}
```

La classe `Point3D` hérite des propriétés `x` et `y` de la classe `Point` et ajoute une nouvelle propriété `z`.

**Blocs**

Les blocs sont des structures de données anonymes contenant du code. Ils peuvent être passés en tant qu'arguments aux méthodes. Le mot-clé `[]` est utilisé pour créer des blocs :

```smalltalk
Point3D new foreach: [ :unPoint | unPoint getX ].
```

Ce bloc parcourt tous les objets `Point3D` et récupère leur propriété `x`.

**Métaclasses**

Les métaclasses sont des classes qui décrivent la structure des classes. Elles peuvent être utilisées pour modifier le comportement des classes dynamiquement. Le mot-clé `metaclass` est utilisé pour accéder à la métaclasse d'une classe :

```smalltalk
Point class metaclass >> become: #( + * )
```

Cette expression modifie la métaclasse de `Point` pour ajouter les sélecteurs `+` et `*`. Cela permet aux objets `Point` d'être additionnés et multipliés.

**Évaluation dynamique**

Smalltalk est un langage à évaluation dynamique, ce qui signifie que le code peut être modifié pendant l'exécution. Cela permet une grande flexibilité et une extensibilité. Par exemple :

```smalltalk
x := 1.
eval: 'x := x + 1'.
```

Cette expression évalue la chaîne `'x := x + 1'` et modifie la valeur de `x` à 2.

**Conclusion**

Ce tutoriel de base vous a permis de découvrir les concepts fondamentaux de Smalltalk. Pour approfondir vos connaissances, je vous recommande de vous référer à la documentation Smalltalk officielle et à des ressources d'apprentissage en ligne.