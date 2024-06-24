```typescript

interface IUtilisateur {
  nom: string;
  prenom: string;
  adresse: string;
  telephone: number;
  email: string;
}

class Utilisateur implements IUtilisateur {
  constructor(nom: string, prenom: string, adresse: string, telephone: number, email: string) {
    this.nom = nom;
    this.prenom = prenom;
    this.adresse = adresse;
    this.telephone = telephone;
    this.email = email;
  }
}

interface ICompteBancaire {
  numeroCompte: string;
  solde: number;
  utilisateur: IUtilisateur;
}

class CompteBancaire implements ICompteBancaire {
  constructor(numeroCompte: string, solde: number, utilisateur: IUtilisateur) {
    this.numeroCompte = numeroCompte;
    this.solde = solde;
    this.utilisateur = utilisateur;
  }
}

interface IOperationBancaire {
  type: string;
  montant: number;
  date: Date;
  compteBancaire: ICompteBancaire;
}

class OperationBancaire implements IOperationBancaire {
  constructor(type: string, montant: number, date: Date, compteBancaire: ICompteBancaire) {
    this.type = type;
    this.montant = montant;
    this.date = date;
    this.compteBancaire = compteBancaire;
  }
}

interface ITransaction {
  numeroTransaction: string;
  dateTransaction: Date;
  montantTransaction: number;
  utilisateurExpediteur: IUtilisateur;
  utilisateurReceveur: IUtilisateur;
}

class Transaction implements ITransaction {
  constructor(numeroTransaction: string, dateTransaction: Date, montantTransaction: number, utilisateurExpediteur: IUtilisateur, utilisateurReceveur: IUtilisateur) {
    this.numeroTransaction = numeroTransaction;
    this.dateTransaction = dateTransaction;
    this.montantTransaction = montantTransaction;
    this.utilisateurExpediteur = utilisateurExpediteur;
    this.utilisateurReceveur = utilisateurReceveur;
  }
}

interface IBanque {
  nom: string;
  adresse: string;
  utilisateurs: IUtilisateur[];
  comptesBancaires: ICompteBancaire[];
  operationsBancaires: IOperationBancaire[];
  transactions: ITransaction[];
}

class Banque implements IBanque {
  constructor(nom: string, adresse: string, utilisateurs: IUtilisateur[], comptesBancaires: ICompteBancaire[], operationsBancaires: IOperationBancaire[], transactions: ITransaction[]) {
    this.nom = nom;
    this.adresse = adresse;
    this.utilisateurs = utilisateurs;
    this.comptesBancaires = comptesBancaires;
    this.operationsBancaires = operationsBancaires;
    this.transactions = transactions;
  }
}

interface ICompteCourant extends ICompteBancaire {
  tauxInteret: number;
}

class CompteCourant extends CompteBancaire implements ICompteCourant {
  constructor(numeroCompte: string, solde: number, utilisateur: IUtilisateur, tauxInteret: number) {
    super(numeroCompte, solde, utilisateur);
    this.tauxInteret = tauxInteret;
  }
}

interface ICompteEpargne extends ICompteBancaire {
  montantInterets: number;
}

class CompteEpargne extends CompteBancaire implements ICompteEpargne {
  constructor(numeroCompte: string, solde: number, utilisateur: IUtilisateur, montantInterets: number) {
    super(numeroCompte, solde, utilisateur);
    this.montantInterets = montantInterets;
  }
}

// Création de quelques utilisateurs
let utilisateur1 = new Utilisateur("Dupont", "Jean", "1 rue de la Paix", 0123456789, "jean.dupont@email.com");
let utilisateur2 = new Utilisateur("Martin", "Marie", "2 rue de la République", 0987654321, "marie.martin@email.com");

// Création de quelques comptes bancaires
let compteBancaire1 = new CompteBancaire("FR12345678901234567890", 1000, utilisateur1);
let compteBancaire2 = new CompteBancaire("FR98765432109876543210", 2000, utilisateur2);

// Création de quelques opérations bancaires
let operationBancaire1 = new OperationBancaire("DÉBIT", -500, new Date(), compteBancaire1);
let operationBancaire2 = new OperationBancaire("CRÉDIT", 1000, new Date(), compteBancaire2);

// Création de quelques transactions
let transaction1 = new Transaction("TR1234567890", new Date(), 500, utilisateur1, utilisateur2);
let transaction2 = new Transaction("TR9876543210", new Date(), 1000, utilisateur2, utilisateur1);

// Création d'une banque
let banque = new Banque("La Banque Populaire", "3 rue de la Bourse", [utilisateur1, utilisateur2], [compteBancaire1, compteBancaire2], [operationBancaire1, operationBancaire2], [transaction1, transaction2]);

// Affichage des informations de la banque
console.log(`Nom de la banque : ${banque.nom}`);
console.log(`Adresse de la banque : ${banque.adresse}`);
console.log("Utilisateurs de la banque :");
banque.utilisateurs.forEach(utilisateur => {
  console.log(` - ${utilisateur.nom} ${utilisateur.prenom}`);
});
console.log("Comptes bancaires de la banque :");
banque.comptesBancaires.forEach(compteBancaire => {
  console.log(` - Numéro de compte : ${compteBancaire.numeroCompte}`);
  console.log(`   Solde du compte : ${compteBancaire.solde}`);
  console.log(`   Utilisateur associé au compte : ${compteBancaire.utilisateur.nom} ${compteBancaire.utilisateur.prenom}`);
});
console.log("Opérations bancaires de la banque :");
banque.operationsBancaires.forEach(operationBancaire => {
  console.log(` - Type d'opération : ${operationBancaire.type}`);
  console.log(`   Montant de l'opération : ${operationBancaire.montant}`);
  console.log(`   Date de l'opération : ${operationBancaire.date}`);
  console.log(`   Compte bancaire concerné : ${operationBancaire.compteBancaire.numeroCompte}`);
});
console.log("Transactions de la banque :");
banque.transactions.forEach(transaction => {
  console.log(` - Numéro de transaction : ${transaction.numeroTransaction}`);
  console.log(`   Date de la transaction : ${transaction.dateTransaction}`);
  console.log(`   Montant de la transaction : ${transaction.montantTransaction}`);
  console.log(`   Utilisateur expéditeur : ${transaction.utilisateurExpediteur.nom} ${transaction.utilisateurExpediteur.prenom}`);
  console.log(`   Utilisateur receveur : ${transaction.utilisateurReceveur.nom} ${transaction.utilisateurReceveur.prenom}`);
});

```

**Explications du code :**

Ce code définit une série d'interfaces et de classes pour représenter une banque, ses utilisateurs, ses comptes bancaires, ses opérations bancaires et ses transactions.

* **Interfaces :**

  * `IUtilisateur` : Définit les propriétés d'un utilisateur (nom, prénom, adresse, téléphone, email).
  * `ICompteBancaire` : Définit les propriétés d'un compte bancaire (numéro de compte, solde, utilisateur).
  * `IOperationBancaire` : Définit les propriétés d'une opération bancaire (type, montant, date, compte bancaire).
  * `ITransaction` : Définit les propriétés d'une transaction (numéro de transaction, date de transaction, montant de transaction, utilisateur expéditeur, utilisateur receveur).
  * `IBanque` : Définit les propriétés d'une banque (nom, adresse, utilisateurs, comptes bancaires, opérations bancaires, transactions).

* **Classes :**

  * `Utilisateur` : Implémente l'interface `IUtilisateur` et représente un utilisateur de la banque.
  * `CompteBancaire` : Implémente l'interface `ICompteBancaire` et représente un compte bancaire.
  * `OperationBancaire` : Implémente l'interface `IOperationBancaire` et représente une opération bancaire.
  * `Transaction` : Implémente l'interface `ITransaction` et représente une transaction.
  * `Banque` : Implémente l'interface `IBanque` et représente une banque.
  * `CompteCourant` : Une sous-classe de `CompteBancaire` qui représente un compte courant avec un taux d'intérêt.
  * `CompteEpargne`