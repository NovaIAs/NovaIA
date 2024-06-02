**Système de gestion des étudiants et des cours (en français)**

**Types**

```typescript
type Etudiant = {
  id: number;
  nom: string;
  prenom: string;
};

type Cours = {
  id: number;
  nom: string;
  credit: number;
};

type Inscription = {
  idEtudiant: number;
  idCours: number;
  note: number;
};
```

**Classes**

```typescript
class GestionEtudiants {
  private etudiants: Etudiant[];

  constructor() {
    this.etudiants = [];
  }

  ajouterEtudiant(etudiant: Etudiant): void {
    if (this.etudiantExiste(etudiant.id)) {
      console.error("L'étudiant existe déjà.");
    } else {
      this.etudiants.push(etudiant);
    }
  }

  supprimerEtudiant(id: number): void {
    const index = this.trouverIndexEtudiant(id);
    if (index === -1) {
      console.error("L'étudiant n'existe pas.");
    } else {
      this.etudiants.splice(index, 1);
    }
  }

  trouverEtudiant(id: number): Etudiant | undefined {
    for (const etudiant of this.etudiants) {
      if (etudiant.id === id) {
        return etudiant;
      }
    }
    return undefined;
  }

  private etudiantExiste(id: number): boolean {
    return this.trouverIndexEtudiant(id) !== -1;
  }

  private trouverIndexEtudiant(id: number): number {
    for (let i = 0; i < this.etudiants.length; i++) {
      if (this.etudiants[i].id === id) {
        return i;
      }
    }
    return -1;
  }
}

class GestionCours {
  private cours: Cours[];

  constructor() {
    this.cours = [];
  }

  ajouterCours(cours: Cours): void {
    if (this.coursExiste(cours.id)) {
      console.error("Le cours existe déjà.");
    } else {
      this.cours.push(cours);
    }
  }

  supprimerCours(id: number): void {
    const index = this.trouverIndexCours(id);
    if (index === -1) {
      console.error("Le cours n'existe pas.");
    } else {
      this.cours.splice(index, 1);
    }
  }

  trouverCours(id: number): Cours | undefined {
    for (const cours of this.cours) {
      if (cours.id === id) {
        return cours;
      }
    }
    return undefined;
  }

  private coursExiste(id: number): boolean {
    return this.trouverIndexCours(id) !== -1;
  }

  private trouverIndexCours(id: number): number {
    for (let i = 0; i < this.cours.length; i++) {
      if (this.cours[i].id === id) {
        return i;
      }
    }
    return -1;
  }
}

class GestionInscriptions {
  private inscriptions: Inscription[];

  constructor() {
    this.inscriptions = [];
  }

  ajouterInscription(inscription: Inscription): void {
    this.inscriptions.push(inscription);
  }

  supprimerInscription(etudiantId: number, coursId: number): void {
    const index = this.trouverIndexInscription(etudiantId, coursId);
    if (index === -1) {
      console.error("L'inscription n'existe pas.");
    } else {
      this.inscriptions.splice(index, 1);
    }
  }

  trouverInscription(etudiantId: number, coursId: number): Inscription | undefined {
    for (const inscription of this.inscriptions) {
      if (inscription.idEtudiant === etudiantId && inscription.idCours === coursId) {
        return inscription;
      }
    }
    return undefined;
  }

  private trouverIndexInscription(etudiantId: number, coursId: number): number {
    for (let i = 0; i < this.inscriptions.length; i++) {
      if (this.inscriptions[i].idEtudiant === etudiantId && this.inscriptions[i].idCours === coursId) {
        return i;
      }
    }
    return -1;
  }
}
```

**Interface**

```typescript
interface IApplication {
  gestionEtudiants: GestionEtudiants;
  gestionCours: GestionCours;
  gestionInscriptions: GestionInscriptions;
}
```

**Application**

```typescript
class Application implements IApplication {
  gestionEtudiants: GestionEtudiants;
  gestionCours: GestionCours;
  gestionInscriptions: GestionInscriptions;

  constructor() {
    this.gestionEtudiants = new GestionEtudiants();
    this.gestionCours = new GestionCours();
    this.gestionInscriptions = new GestionInscriptions();
  }

  ajouterEtudiant(etudiant: Etudiant): void {
    this.gestionEtudiants.ajouterEtudiant(etudiant);
  }

  ajouterCours(cours: Cours): void {
    this.gestionCours.ajouterCours(cours);
  }

  ajouterInscription(inscription: Inscription): void {
    this.gestionInscriptions.ajouterInscription(inscription);
  }
}
```

**Utilisation**

```typescript
const application = new Application();

application.ajouterEtudiant({ id: 1, nom: "DUPONT", prenom: "Jean" });
application.ajouterCours({ id: 1, nom: "Mathématiques", credit: 6 });
application.ajouterInscription({ idEtudiant: 1, idCours: 1, note: 15 });

const etudiant = application.gestionEtudiants.trouverEtudiant(1);
if (etudiant) {
  console.log(`Nom de l'étudiant : ${etudiant.nom}, Prénom : ${etudiant.prenom}`);
}
```