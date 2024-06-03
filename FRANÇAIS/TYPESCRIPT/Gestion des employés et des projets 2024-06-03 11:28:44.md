```typescript
// Interface Utilisateur
interface IUtilisateur {
  nom: string;
  courriel: string;
  contrat: IContrat;
}

// Interface Contrat
interface IContrat {
  type: "permanent" | "CDD";
  dateEmbauche: Date;
  dateFin: Date | null;
}

// Enumération des types de services
enum TypeService {
  Conseil,
  Intégration,
  Formation
}

// Interface Service
interface IService {
  nom: string;
  type: TypeService;
  description: string;
  tarifHoraire: number;
}

// Classe Employé
class Employe implements IUtilisateur {
  nom: string;
  courriel: string;
  contrat: IContrat;

  constructor(nom: string, courriel: string, contrat: IContrat) {
    this.nom = nom;
    this.courriel = courriel;
    this.contrat = contrat;
  }

  getSalaireMensuel(): number {
    switch (this.contrat.type) {
      case "permanent":
        return 2000;
      case "CDD":
        const nbJoursTravailles = (this.contrat.dateFin! - this.contrat.dateEmbauche) / (1000 * 60 * 60 * 24);
        return (nbJoursTravailles * 8) * this.tarifHoraire;
      default:
        throw new Error("Type de contrat inconnu");
    }
  }

  getTarifHoraire(): number {
    return this.contrat.type === "CDD" ? 10 : 20;
  }
}

// Classe Service
class Service implements IService {
  nom: string;
  type: TypeService;
  description: string;
  tarifHoraire: number;

  constructor(nom: string, type: TypeService, description: string, tarifHoraire: number) {
    this.nom = nom;
    this.type = type;
    this.description = description;
    this.tarifHoraire = tarifHoraire;
  }
}

// Classe Gestionnaire
class Gestionnaire extends Employe {
  services: IService[];

  constructor(nom: string, courriel: string, contrat: IContrat, services: IService[]) {
    super(nom, courriel, contrat);
    this.services = services;
  }

  getServices(): IService[] {
    return this.services;
  }
}

// Classe Projet
class Projet {
  nom: string;
  description: string;
  services: IService[];
  employes: Employe[];

  constructor(nom: string, description: string, services: IService[], employes: Employe[]) {
    this.nom = nom;
    this.description = description;
    this.services = services;
    this.employes = employes;
  }

  getCoûtTotal(): number {
    let coût = 0;
    this.employes.forEach(employe => {
      coût += employe.getSalaireMensuel();
    });
    this.services.forEach(service => {
      coût += service.tarifHoraire * 40 * 4;
    });
    return coût;
  }
}

// Utilisation des classes
const employe1 = new Employe("Dupont", "dupont@gmail.com", { type: "CDD", dateEmbauche: new Date("2023-01-01"), dateFin: new Date("2023-12-31") });
const service1 = new Service("Conseil", TypeService.Conseil, "Prestation de conseil en stratégie d'entreprise", 50);
const gestionnaire1 = new Gestionnaire("Martin", "martin@gmail.com", { type: "permanent", dateEmbauche: new Date("2021-01-01"), dateFin: null }, [service1]);
const projet1 = new Projet("Projet X", "Projet de transformation digitale", [service1], [employe1, gestionnaire1]);

console.log(`Coût total du projet ${projet1.nom} : ${projet1.getCoûtTotal()} €`);
```

**Explication du code :**

Ce code TypeScript est un exemple d'un programme complexe qui modélise un système de gestion d'employés et de projets. Il utilise des interfaces, des classes, des énumérations et des méthodes enchaînées pour créer un code structuré et facile à maintenir.

* **Interfaces** : Les interfaces `IUtilisateur`, `IContrat` et `IService` définissent des contrats pour les classes qui les implémentent. Ils garantissent que les classes ont les propriétés et les méthodes nécessaires.
* **Classes** : Les classes `Employe`, `Service`, `Gestionnaire` et `Projet` implémentent les interfaces et encapsulent le comportement et les données des différents objets du système.
* **Enumération** : L'énumération `TypeService` définit les différents types de services proposés par l'entreprise.
* **Méthodes enchaînées** : Les méthodes `getSalaireMensuel()`, `getTarifHoraire()` et `getCoûtTotal()` utilisent l'enchaînement de méthodes pour effectuer des calculs complexes sur les objets du système.

**Utilisation du code :**

Dans l'exemple d'utilisation, nous créons des instances des classes `Employe`, `Service`, `Gestionnaire` et `Projet` pour représenter un employé, un service, un gestionnaire et un projet. Enfin, nous calculons le coût total du projet à l'aide de la méthode `getCoûtTotal()` et l'affichons dans la console.