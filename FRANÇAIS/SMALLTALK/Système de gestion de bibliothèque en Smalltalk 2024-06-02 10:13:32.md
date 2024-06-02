```smalltalk
**Système de gestion de bibliothèque**

**Classe Bibliothèque**

```smalltalk
Bibliothèque est une Classe qui contient les livres disponibles.

attributs
	livres: une Collection

méthodes
	constructeur [arg]
		livres := arg

	ajouterLivre: unLivre
		livres add: unLivre

	retirerLivre: unLivre
		livres remove: unLivre

	listerLivres
		livres do: [:unLivre | unLivre >> titre printString]
```

**Classe Livre**

```smalltalk
Livre est une Classe représentant un livre dans la bibliothèque.

attributs
	titre: une Chaîne
	auteur: une Chaîne
	nombreExemplaires: un Entier

méthodes
	constructeur [titreArg auteurArg nbExemArg]
		titre := titreArg
		auteur := auteurArg
		nombreExemplaires := nbExemArg

	emprunter
		nombreExemplaires := nombreExemplaires - 1

	rendre
		nombreExemplaires := nombreExemplaires + 1
```

**Classe GestionnaireEmprunts**

```smalltalk
GestionnaireEmprunts est une Classe qui gère les emprunts et les rendus de livres.

attributs
	livresEmpruntes: une Collection

méthodes
	initialiser
		livresEmpruntes := une Nouvelle Collection

	emprunterLivre: unLivre pour: unMembre
		livresEmpruntes add: (Arrondissement de: unMembre avec: unLivre)
		unLivre >> emprunter

	rendreLivre: unLivre pour: unMembre
		livresEmpruntes remove: (Arrondissement de: unMembre avec: unLivre)
		unLivre >> rendre

	listerEmprunts
		livresEmpruntes do: [:unEmprunt | unEmprunt >> afficher]
```

**Classe Arrondissement**

```smalltalk
Arrondissement est une Classe qui associe un membre à un livre emprunté.

attributs
	membre: un Membre
	livre: un Livre

méthodes
	constructeur [membreArg livreArg]
		membre := membreArg
		livre := livreArg

	afficher
		'Le membre ', membre >> nom, ' a emprunté le livre ', livre >> titre printString
```

**Classe InterfaceUtilisateur**

```smalltalk
InterfaceUtilisateur est une Classe qui fournit une interface utilisateur pour gérer les livres et les emprunts.

attributs
	bibliothèque: une Bibliothèque
	gestionnaire: un GestionnaireEmprunts

méthodes
	constructeur [bibliothèqueArg gestionnaireArg]
		bibliothèque := bibliothèqueArg
		gestionnaire := gestionnaireArg

	menuPrincipal
		[   1. Ajouter un livre
	            2. Retirer un livre
	            3. Lister les livres
	            4. Emprunter un livre
	            5. Rendre un livre
	            6. Quitter
		] répéter
		[   l'utilisateur entre un choix
	        cas du choix de 1        [ self ajouterLivre ]
	        cas du choix de 2        [ self retirerLivre ]
	        cas du choix de 3        [ bibliothèque >> listerLivres ]
	        cas du choix de 4        [ self emprunterLivre ]
	        cas du choix de 5        [ self rendreLivre ]
	        cas du choix de 6        [ Quittez ]
	        Sinon        [ Réafficher le menu ] ]

	ajouterLivre
		Récupérer le titre, l'auteur et le nombre d'exemplaires auprès de l'utilisateur
		Ajouter un nouveau livre à la bibliothèque avec ces informations

	retirerLivre
		Récupérer le titre du livre auprès de l'utilisateur
		Retirer le livre de la bibliothèque en fonction de son titre

	emprunterLivre
		Récupérer le titre du livre et le nom du membre auprès de l'utilisateur
		Emprunter le livre via le gestionnaire d'emprunts

	rendreLivre
		Récupérer le titre du livre et le nom du membre auprès de l'utilisateur
		Rendre le livre via le gestionnaire d'emprunts

	listerEmprunts
		Lister les emprunts en cours via le gestionnaire d'emprunts
```

**Utilisation**

```smalltalk
bibliothèque := une Nouvelle Bibliothèque
gestionnaire := un Nouveau GestionnaireEmprunts
interfaceUtilisateur := une Nouvelle InterfaceUtilisateur avec: bibliothèque avec: gestionnaire
interfaceUtilisateur >> menuPrincipal
```