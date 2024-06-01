**Système de gestion de bibliothèque (en Smalltalk)**

**Classe Livre**

```smalltalk
Classe Livre {
	instance
		| titre auteur annee |

	initialiser: unTitre unAuteur uneAnnee {
		titre := unTitre.
		auteur := unAuteur.
		annee := uneAnnee
	}

	afficher {
		Transcript display: titre, ' par ', auteur, ', ', annee.
	}
}
```

**Classe Bibliotheque**

```smalltalk
Classe Bibliotheque {
	instance
		| livres |

	initialiser {
		livres := Bag new.
	}

	ajouter: unLivre {
		livres add: unLivre.
	}

	rechercherParTitre: unTitre {
		livres reject: [:l | l titre ~= unTitre]
	}
}
```

**Classe InterfaceUtilisateur**

```smalltalk
Classe InterfaceUtilisateur {
	instance
		| bibliotheque |

	initialiser {
		bibliotheque := Bibliotheque new.
	}

	menuPrincipal {
		| choix |

		Transcript display: 'Options disponibles : '.
		Transcript display: '1. Ajouter un livre'; nl.
		Transcript display: '2. Rechercher un livre'; nl.
		Transcript display: '0. Quitter'.
		Transcript display: 'Votre choix : '.
		[choix := Transcript readLine.
		choix = '0' ifTrue: [Transcript display: 'Au revoir !'. Self quit].
		choix = '1' ifTrue: [Self ajouterLivre].
		choix = '2' ifTrue: [Self rechercherLivre]]. value = choix
	}

	ajouterLivre {
		| titre auteur annee |

		Transcript display: 'Titre du livre : '.
		titre := Transcript readLine.
		Transcript display: 'Auteur du livre : '.
		auteur := Transcript readLine.
		Transcript display: 'Année de publication : '.
		annee := Transcript readLine asInteger.

		bibliotheque ajouter: Livre new initialise: titre auteur: auteur annee: annee.
	}

	rechercherLivre {
		| titre livres trouves |

		Transcript display: 'Titre du livre recherché : '.
		titre := Transcript readLine.

		trouves := bibliotheque rechercherParTitre: titre.
		trouves isEmpty ifTrue: [
			Transcript display: 'Aucun livre correspondant trouvé.'.
		] ifFalse: [
			Transcript display: 'Livres trouvés : '.
			trouves displayOn: Transcript
		]
	}
}
```

**Main**

```smalltalk
InterfaceUtilisateur new menuPrincipal.
```

**Explications**

* La **classe Livre** représente un livre, avec ses attributs (titre, auteur, année) et ses méthodes (initialisation, affichage).
* La **classe Bibliothèque** gère une collection de livres et permet de les ajouter et de les rechercher par titre.
* La **classe InterfaceUtilisateur** fournit une interface utilisateur textuelle pour interagir avec la bibliothèque : ajouter un livre, rechercher un livre et quitter.
* Le **main** crée un objet InterfaceUtilisateur et lance la boucle principale de l'interface utilisateur.