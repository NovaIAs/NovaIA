```smalltalk
**Classe Objet**
```smalltalk
Objet subclass: #MonObjet
```

**Instances**
```smalltalk
m := MonObjet new.
```

**Variables d'instance**
```smalltalk
m nombreAttribute → 10
```

**Accesseurs**
```smalltalk
m nombreAttribute
m nombreAttribute: 20
```

**Méthodes**
```smalltalk
MonObjet class>>maMéthode
```

**Messages**
```smalltalk
m maMéthode
```

**Structures conditionnelles**
```smalltalk
m nombreAttribute ifTrue: [ self erreur ]
m nombreAttribute = 10 ifTrue: [ self autreMethode ]
```

**Boucles**
```smalltalk
1 to: 10 do: [ :i | i afficher ]
```

**Collections**
```smalltalk
tab := OrderedCollection new.
tab add: 1.
tab at: 1
```

**Dictionnaires**
```smalltalk
dico := Dictionary new.
dico at: 'clé' put: 'valeur'
dico at: 'clé'
```

**Closures**
```smalltalk
[ :arg | arg afficher ] value: 10
```

**Exceptions**
```smalltalk
[ self erreur ] on: Error do: [ :e | e afficher ]
```

**Métaclasses**
```smalltalk
MonObjet metaclass>>maMéthodeDeClasse
```

**Héritage multiple**
```smalltalk
NouvelleClasse subclass: #MonMixIn
```

**Surcharges**
```smalltalk
MonObjet class>>maMéthode
MonMixIn class>>maMéthode
```

**Allocateurs**
```smalltalk
nouvelleInstance := MonObjet basicNew: MyClass.
```

**Inspecteurs**
```smalltalk
nouvelleInstance inspector
```

**Traitement des événements**
```smalltalk
ui := UIWindow new.
ui on: #clic do: [ self maMéthode ].
```

**Variables globales**
```smalltalk
GlobalVariable := 10
```

**Utilisation de la bibliothèque standard**
```smalltalk
t := TimeZone currentZone.
c := DateAndTime new.
```