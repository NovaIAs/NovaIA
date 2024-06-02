**Types et interfaces**

```typescript
// Interface définissant les propriétés d'un utilisateur
interface Utilisateur {
  id: number;
  nom: string;
  email: string;
  roles: string[];
}

// Type représentant une liste d'utilisateurs
type ListeUtilisateurs = Utilisateur[];

// Interface définissant les options de filtrage pour les utilisateurs
interface OptionsFiltrageUtilisateurs {
  nomPartiel?: string;
  rolesInclus?: string[];
  rolesExclus?: string[];
}
```

**Fonctions**

```typescript
// Fonction de filtrage des utilisateurs en fonction des options fournies
const filtrerUtilisateurs = (
  utilisateurs: ListeUtilisateurs,
  optionsFiltrage: OptionsFiltrageUtilisateurs
): ListeUtilisateurs => {
  return utilisateurs.filter((utilisateur) => {
    if (optionsFiltrage.nomPartiel) {
      return utilisateur.nom.includes(optionsFiltrage.nomPartiel);
    }

    if (optionsFiltrage.rolesInclus && optionsFiltrage.rolesInclus.length > 0) {
      return optionsFiltrage.rolesInclus.some((role) => utilisateur.roles.includes(role));
    }

    if (optionsFiltrage.rolesExclus && optionsFiltrage.rolesExclus.length > 0) {
      return !optionsFiltrage.rolesExclus.some((role) => utilisateur.roles.includes(role));
    }

    return true;
  });
};

// Fonction asynchrone simulant un appel de base de données pour récupérer les utilisateurs
const recupererUtilisateurs = async (): Promise<ListeUtilisateurs> => {
  const reponse = await fetch('https://example.com/api/utilisateurs');
  return await reponse.json();
};
```

**Classes**

```typescript
class GestionnaireUtilisateurs {
  private utilisateurs: ListeUtilisateurs;

  constructor(utilisateurs: ListeUtilisateurs) {
    this.utilisateurs = utilisateurs;
  }

  recupererTousLesUtilisateurs(): ListeUtilisateurs {
    return this.utilisateurs;
  }

  recupererUtilisateurParId(id: number): Utilisateur | undefined {
    return this.utilisateurs.find((utilisateur) => utilisateur.id === id);
  }

  ajouterUtilisateur(utilisateur: Utilisateur): void {
    this.utilisateurs.push(utilisateur);
  }

  modifierUtilisateur(id: number, utilisateurModifie: Utilisateur): void {
    const index = this.utilisateurs.findIndex((utilisateur) => utilisateur.id === id);
    if (index !== -1) {
      this.utilisateurs[index] = utilisateurModifie;
    }
  }

  supprimerUtilisateur(id: number): void {
    const index = this.utilisateurs.findIndex((utilisateur) => utilisateur.id === id);
    if (index !== -1) {
      this.utilisateurs.splice(index, 1);
    }
  }

  filtrerUtilisateurs(optionsFiltrage: OptionsFiltrageUtilisateurs): ListeUtilisateurs {
    return filtrerUtilisateurs(this.utilisateurs, optionsFiltrage);
  }
}
```

**Exemple d'utilisation**

```typescript
// Créer un gestionnaire d'utilisateurs avec des données fictives
const gestionnaireUtilisateurs = new GestionnaireUtilisateurs([
  {
    id: 1,
    nom: 'Jean Dupont',
    email: 'jean.dupont@example.com',
    roles: ['utilisateur'],
  },
  {
    id: 2,
    nom: 'Marie Martin',
    email: 'marie.martin@example.com',
    roles: ['utilisateur', 'administrateur'],
  },
]);

// Récupérer tous les utilisateurs
const tousLesUtilisateurs = gestionnaireUtilisateurs.recupererTousLesUtilisateurs();

// Récupérer un utilisateur par son ID
const utilisateurParId = gestionnaireUtilisateurs.recupererUtilisateurParId(1);

// Ajouter un nouvel utilisateur
gestionnaireUtilisateurs.ajouterUtilisateur({
  id: 3,
  nom: 'Pierre Dubois',
  email: 'pierre.dubois@example.com',
  roles: ['utilisateur'],
});

// Modifier un utilisateur
gestionnaireUtilisateurs.modifierUtilisateur(2, {
  nom: 'Marie Durand',
  roles: ['administrateur'],
});

// Supprimer un utilisateur
gestionnaireUtilisateurs.supprimerUtilisateur(1);

// Filtrer les utilisateurs
const utilisateursFiltres = gestionnaireUtilisateurs.filtrerUtilisateurs({
  nomPartiel: 'Marie',
  rolesInclus: ['administrateur'],
});
```