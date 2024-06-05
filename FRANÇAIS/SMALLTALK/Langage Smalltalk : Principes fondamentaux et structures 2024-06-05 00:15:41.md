```smalltalk
**Objets et classes**

Objet >> monNom
    ^ unNom

**Méthodes de classe**

Objet class >> allInstances
    ^ (OrderedCollection new) addAll: self instances

**Méthodes d'instance**

Objet >> asString
    ^ self class name , '(', self printString, ')'

**Boucles**

```
| j k l |
Do: [ j < 5 | j +: 1 ]
Do: [ j < 5 |
        k := j * j
        l := k + j
        j +: 1 ]
```

**Gestion des exceptions**

```
[[ self error: unMessage ] on: Exception |
    unMessage printString
]
```

**Structures de données**

**Collection**

```
(Bag new) addAll: #('a' 'b' 'c')
(ByteArray new: 100) copyFrom: #('a' 'b' 'c') at: 20 count: 3
(Dictionary new) at: #'key' put: #'val'
(OrderedCollection new) addAll: #('a' 'b' 'c')
(SequenceableCollection new) addAll: #('a' 'b' 'c')
(Set new) addAll: #('a' 'b' 'c')
```

**Flux**

```
(FileStream new: 'myfile.st') openFor: 'write'
(StringStream new) put: #'Hello world'
```

**Fonctions**

```
(: +) value: 3 value: 4
```

**Métaprogrammation**

```
(Metaclass class) >> globalMethodAt: #'test' put: (MethodBlock new)
((self class class) new) methodAt: #'foo' rename: #'bar'
```

**Réflexion**

```
unObject isKindOf: Objet class
unObject class
unObject conformsTo: #'Serializable'
unObject perform: #'test' with: #'arg1' with: #'arg2'
```

**Syntaxe des blocs**

```
[* self | | self *] value
[ :a :b | | b * a ] value
[ :a [ :b | | self * b ] ] value
```

**Textes**

```
(String new) with: #'Hello world'
(String new) with: NBSP "< --espace sans rupture -->"
(String new) with: '"apostrophe"'
(String new) with: #'lineFeed'
```

**Temps**

```
(Time new)
(Clock tick)
(Clock totalSeconds)
```

**IU**

```
(Window new) color: Color blue
(Button new) label: 'OK'
(Canvas new) drawOn: [:aCanvas | | aCanvas | aCanvas fillOval: (Rectangle new) extent: 100@100]
```