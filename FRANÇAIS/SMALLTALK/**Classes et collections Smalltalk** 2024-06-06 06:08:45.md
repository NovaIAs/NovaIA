```smalltalk
**Classe Objet**

Objet subclass: #Point
instanceVariableNames: 'x y'
classVariableNames: ''
poolDictionaries: ''

**Méthodes d'instance**

translateByX: deltaX y: deltaY
  x := x + deltaX.
  y := y + deltaY.

translateBy: displacement
  translateByX: displacement x y: displacement y.

**Classe Collection**

Collection subclass: #Ensemble
instanceVariableNames: 'elements'
classVariableNames: ''
poolDictionaries: ''

**Méthodes d'instance**

contenu: aCollection
  elements := aCollection copy.

ajouter: unObjet
  elements add: unObjet.

nombre
  ^elements size.

**Classe Dictionary**

Dictionary subclass: #TableDeHachage
instanceVariableNames: 'keys values size'
classVariableNames: ''
poolDictionaries: ''

**Méthodes d'instance**

contenu: aTable
  keys := aTable keys copy.
  values := aTable values copy.
  size := 0.

ajouter: uneClef aUneValeur
  si: [keys contains: uneClef] false: [size := (size + 1)].
  keys add: uneClef.
  values add: aUneValeur.

valeurPour: uneClef
  ^values at: (keys indexOf: uneClef).

nombre
  ^size.

**Classe String**

String subclass: #ChaineDeCaractere
instanceVariableNames: 'characters'
classVariableNames: ''
poolDictionaries: ''

**Méthodes d'instance**

valeur
  ^characters.

mettre: unCaractère a: unIndex
  characters at: unIndex put: unCaractère.

ajouter: unCaractère
  characters add: unCaractère.

**Classe UI**

UI subclass: #InterfaceUtilisateur
instanceVariableNames: ''
classVariableNames: ''
poolDictionaries: ''

**Méthodes de classe**

ouvrir
  ^new open.

**Méthodes d'instance**

ouvrir
  ^self.

**Classe Traversable**

Traversable subclass: #Iterateur
instanceVariableNames: 'collection'
classVariableNames: ''
poolDictionaries: ''

**Méthodes d'instance**

contenu: uneCollection
  collection := uneCollection.

suivant
  ^collection next.

hasSuivant
  ^collection hasNext.

réinitialiser
  collection reset.
```

**Explication**

Ce code définit les classes suivantes en Smalltalk :

* **Objet** : Une classe de base pour tous les objets.
* **Point** : Une classe représentant un point dans un espace à deux dimensions.
* **Ensemble** : Une classe de collection qui contient un ensemble d'objets.
* **TableDeHachage** : Une classe de collection qui implémente une table de hachage pour stocker des paires clé-valeur.
* **ChaineDeCaractere** : Une classe représentant une séquence de caractères.
* **InterfaceUtilisateur** : Une classe représentant une fenêtre d'interface utilisateur.
* **Iterateur** : Une classe représentant un itérateur sur une collection.

Les méthodes d'instance et de classe sont définies pour chaque classe, spécifiant le comportement de ces classes. Ce code démontre les fonctionnalités de base des classes, des collections et de l'interface utilisateur en Smalltalk.