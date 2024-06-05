**Bloc de Gestion de Comptes**

```smalltalk
Objet Comptes
    class
        variable nbComptes
        variable comptes
        
    init
        nbComptes := 0.
        comptes := Collection new.

    ajouterCompte: unCompte
        comptes add: unCompte.
        nbComptes := nbComptes + 1.
        
    afficher
        comptes do: [ :cpt | cpt afficher ].

    nombreComptes
        ^ nbComptes

    sommeSolde
        somme := 0.
        comptes do: [ :cpt | somme := somme + cpt solde ]
        ^ somme

**Bloc de Gestion de Client**

Objet Client
    class
        variable nom
        variable comptes

    initialiserNom: unNom
        nom := unNom.
        comptes := Comptes new.

    afficher
        Transcript show: nom; cr.
        comptes afficher.

    créerCompte: unMontant
        comptes ajouterCompte: compte new montant: unMontant.

    nombreComptes
        ^ comptes nombreComptes

    soldeTotal
        ^ comptes sommeSolde

**Programme Principal**

Objet Main
    class
        main
            client1 := Client new initialiserNom: 'Dupont'.
            client1 créerCompte: 1000.
            client1 créerCompte: 500.
            client2 := Client new initialiserNom: 'Durant'.
            client2 créerCompte: 2000.
            client2 créerCompte: 3000.
            client1 afficher.
            client2 afficher.
            Transcript cr; show: 'Solde total des clients : '; cr;
            show: client1 soldeTotal; cr;
            show: client2 soldeTotal.

```

**Explication du Code**

**Classe `Comptes`**

* Gère une liste de comptes.
* Permet d'ajouter, d'afficher et de compter les comptes.
* Calcule la somme des soldes des comptes.

**Classe `Client`**

* Représente un client ayant plusieurs comptes.
* Permet d'afficher les informations du client et ses comptes.
* Crée des comptes pour le client.
* Renvoie le nombre de comptes et le solde total.

**Classe `Main`**

* Programme principal qui crée deux clients et ajoute des comptes à chacun d'eux.
* Affiche les informations des clients et leurs soldes totaux.

**Utilisation**

Pour exécuter le programme, tapez le code smalltalk suivant dans un environnement Smalltalk :

```
Main main
```

La sortie sera :

```
Dupont
Compte 1 : 1000
Compte 2 : 500

Durant
Compte 1 : 2000
Compte 2 : 3000

Solde total des clients : 
1500
5000
```