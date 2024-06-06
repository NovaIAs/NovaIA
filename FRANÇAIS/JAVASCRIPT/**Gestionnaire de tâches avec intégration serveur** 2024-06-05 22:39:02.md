```javascript
// Module pour gérer les requêtes HTTP asynchrones
import axios from 'axios';

// Classe pour représenter une tâche
class Tache {
  constructor(id, titre, description, statut, priorite) {
    this.id = id;
    this.titre = titre;
    this.description = description;
    this.statut = statut;
    this.priorite = priorite;
  }
  
  // Méthode pour formater la tâche en objet JSON
  toJSONObject() {
    return {
      id: this.id,
      titre: this.titre,
      description: this.description,
      statut: this.statut,
      priorite: this.priorite
    };
  }
}

// Classe pour gérer la liste des tâches
class ListeTaches {
  constructor() {
    this.taches = [];
  }
  
  // Méthode pour ajouter une tâche à la liste
  ajouterTache(tache) {
    this.taches.push(tache);
  }
  
  // Méthode pour supprimer une tâche de la liste
  supprimerTache(id) {
    this.taches = this.taches.filter((t) => t.id !== id);
  }
  
  // Méthode pour modifier une tâche de la liste
  modifierTache(tache) {
    const index = this.taches.findIndex((t) => t.id === tache.id);
    if (index !== -1) {
      this.taches[index] = tache;
    }
  }
  
  // Méthode pour récupérer toutes les tâches
  recupererTaches() {
    return this.taches;
  }
}

// Classe pour gérer le rendu HTML
class Afficheur {
  constructor(listeTaches) {
    this.listeTaches = listeTaches;
    this.elementListe = document.querySelector('#liste-taches');
  }
  
  // Méthode pour afficher la liste des tâches dans le HTML
  afficherTaches() {
    this.elementListe.innerHTML = '';
    this.listeTaches.recupererTaches().forEach((tache) => {
      this.elementListe.appendChild(this.creerElementTache(tache));
    });
  }
  
  // Méthode pour créer un élément HTML représentant une tâche
  creerElementTache(tache) {
    const elementTache = document.createElement('li');
    elementTache.classList.add('tache');
    
    const elementTitre = document.createElement('h3');
    elementTitre.textContent = tache.titre;
    
    const elementDescription = document.createElement('p');
    elementDescription.textContent = tache.description;
    
    const elementStatut = document.createElement('span');
    elementStatut.classList.add('statut', tache.statut);
    elementStatut.textContent = tache.statut;
    
    const elementPriorite = document.createElement('span');
    elementPriorite.classList.add('priorite', tache.priorite);
    elementPriorite.textContent = tache.priorite;
    
    const elementActions = document.createElement('div');
    elementActions.classList.add('actions');
    
    const elementModifier = document.createElement('button');
    elementModifier.classList.add('modifier');
    elementModifier.textContent = 'Modifier';
    
    const elementSupprimer = document.createElement('button');
    elementSupprimer.classList.add('supprimer');
    elementSupprimer.textContent = 'Supprimer';
    
    elementModifier.addEventListener('click', () => this.modifierTache(tache));
    elementSupprimer.addEventListener('click', () => this.supprimerTache(tache));
    
    elementActions.appendChild(elementModifier);
    elementActions.appendChild(elementSupprimer);
    
    elementTache.appendChild(elementTitre);
    elementTache.appendChild(elementDescription);
    elementTache.appendChild(elementStatut);
    elementTache.appendChild(elementPriorite);
    elementTache.appendChild(elementActions);
    
    return elementTache;
  }
  
  // Méthode pour modifier une tâche
  modifierTache(tache) {
    const titre = prompt('Nouveau titre :', tache.titre);
    const description = prompt('Nouvelle description :', tache.description);
    const statut = prompt('Nouveau statut :', tache.statut);
    const priorite = prompt('Nouvelle priorité :', tache.priorite);
    
    if (titre || description || statut || priorite) {
      tache.titre = titre ? titre : tache.titre;
      tache.description = description ? description : tache.description;
      tache.statut = statut ? statut : tache.statut;
      tache.priorite = priorite ? priorite : tache.priorite;
      
      this.afficherTaches();
    }
  }
  
  // Méthode pour supprimer une tâche
  supprimerTache(tache) {
    if (confirm('Êtes-vous sûr de vouloir supprimer cette tâche ?')) {
      this.listeTaches.supprimerTache(tache.id);
      this.afficherTaches();
    }
  }
}

// Création d'un nouveau gestionnaire de tâches
const gestionnaireTaches = new ListeTaches();

// Création d'un nouvel afficheur
const afficheur = new Afficheur(gestionnaireTaches);

// Gestion des événements
document.querySelector('#formulaire-tache').addEventListener('submit', (e) => {
  e.preventDefault();
  
  const titre = e.target.querySelector('input[name="titre"]').value;
  const description = e.target.querySelector('textarea[name="description"]').value;
  const statut = e.target.querySelector('select[name="statut"]').value;
  const priorite = e.target.querySelector('select[name="priorite"]').value;
  
  const tache = new Tache(Date.now(), titre, description, statut, priorite);
  
  gestionnaireTaches.ajouterTache(tache);
  afficheur.afficherTaches();
  
  e.target.reset();
});

// Récupération des tâches depuis le serveur
axios.get('http://localhost:3000/taches')
  .then((response) => {
    response.data.forEach((tache) => {
      gestionnaireTaches.ajouterTache(new Tache(parseInt(tache.id), tache.titre, tache.description, tache.statut, tache.priorite));
    });
    afficheur.afficherTaches();
  })
  .catch((error) => {
    console.error(error);
  });

// Envoi des tâches vers le serveur
document.querySelector('#sauvegarder').addEventListener('click', () => {
  axios.post('http://localhost:3000/taches', gestionnaireTaches.recupererTaches())
    .then(() => {
      alert('Tâches sauvegardées avec succès');
    })
    .catch((error) => {
      console.error(error);
    });
});
```

**Explications :**

Ce code crée une application JavaScript complète pour gérer une liste de tâches. Il utilise des classes, des événements et des requêtes HTTP asynchrones pour interagir avec l'utilisateur et un serveur.

**Classes :**

* `Tache` : Représente une tâche avec son titre, sa description, son statut et sa priorité.
* `ListeTaches` : Gère la liste des tâches, permettant l'ajout, la suppression et la modification.
* `Afficheur` : Gère le rendu HTML de la liste des tâches.

**Événements :**

* Le formulaire de création de tâche ajoute une nouvelle tâche à la liste.
* Les boutons de modification et de suppression modifient ou suppriment une tâche.
* Le bouton "Sauvegarder" envoie les tâches vers un serveur.

**Requêtes HTTP :**

* Une requête GET récupère les tâches depuis un serveur.
* Une requête POST envoie les tâches vers un serveur.

**Autres :**

* Le code utilise axios pour gérer les requêtes HTTP asynchrones.
* Le code affiche les tâches dans une liste HTML.
* L'utilisateur peut modifier et supprimer des tâches.
* Les tâches peuvent être sauvegardées sur un serveur.

Ce code est complexe et offre une variété de fonctionnalités, ce qui le rend difficile à reproduire.