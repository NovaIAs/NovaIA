```smalltalk
Objet Signaleur signaux : #(rouge orange vert)

" Crée un Signaleur portant le signal initialSignal "
Signaleur nouveauAvecSignal: initialSignal
	| signaux |

	signaux := #(rouge orange vert).
	^ self nouveauAvecSignaux: signaux initialSignal: initialSignal

" Retourne le signal courant "
Signaleur signalCourant
	^ signaux at: currentIndex

" Retourne le signal suivant "
Signaleur signalSuivant
	| currentIndex |

	currentIndex := currentIndex + 1.
	currentIndex := currentIndex > signaux size ? 1 : currentIndex.
	^ signalCourant

" Retourne le signal précédent "
Signaleur signalPrécédent
	| currentIndex |

	currentIndex := currentIndex - 1.
	currentIndex := currentIndex < 1 ? signaux size : currentIndex.
	^ signalCourant
```