```smalltalk

Objet définition de Classe SimpleCarre
	instanceVariableNames: 'cote'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exemples'
	
	initialiser: unCote
		super initialiser.
		cote := unCote.
	
	cote
		^cote
	
	côté: unCote
		cote := unCote.
	
	afficher
		Script afficher: 'Carré de côté ', cote printString.
		Script nl.
```