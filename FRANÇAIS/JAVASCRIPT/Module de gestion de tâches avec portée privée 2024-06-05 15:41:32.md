```javascript
// Création d'un module avec une portée privée
export default (function() {

  // Fonction privée pour générer un ID unique
  const generateId = () => {
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
      const r = Math.random() * 16 | 0;
      return (c === 'x' ? r : (r & 0x3 | 0x8)).toString(16);
    });
  };

  // Classe privée pour représenter une tâche
  class Task {
    constructor(title, description, priority, dueDate) {
      this.id = generateId();
      this.title = title;
      this.description = description;
      this.priority = priority;
      this.dueDate = dueDate;
      this.completed = false;
    }

    // Méthode pour marquer la tâche comme terminée
    markAsCompleted() {
      this.completed = true;
    }

    // Méthode pour obtenir une représentation JSON de la tâche
    toJSON() {
      return {
        id: this.id,
        title: this.title,
        description: this.description,
        priority: this.priority,
        dueDate: this.dueDate,
        completed: this.completed
      };
    }
  }

  // Classe privée pour gérer la liste de tâches
  class TaskList {
    constructor() {
      this.tasks = [];
    }

    // Méthode pour ajouter une nouvelle tâche
    addTask(task) {
      this.tasks.push(new Task(...arguments));
    }

    // Méthode pour supprimer une tâche par ID
    deleteTask(id) {
      this.tasks = this.tasks.filter((task) => task.id !== id);
    }

    // Méthode pour obtenir une tâche par ID
    getTask(id) {
      return this.tasks.find((task) => task.id === id);
    }

    // Méthode pour filtrer les tâches en fonction de critères
    filterTasks(predicate) {
      return this.tasks.filter(predicate);
    }

    // Méthode pour obtenir toutes les tâches
    getAllTasks() {
      return [...this.tasks];
    }

    // Méthode pour trier les tâches en fonction d'un champ
    sortTasks(field, order) {
      this.tasks.sort((a, b) => {
        const aValue = a[field];
        const bValue = b[field];
        if (aValue === bValue) {
          return 0;
        } else if (order === 'asc') {
          return aValue < bValue ? -1 : 1;
        } else {
          return bValue < aValue ? -1 : 1;
        }
      });
    }
  }

  // Instance publique de la liste de tâches
  const taskList = new TaskList();

  // Fonction publique pour ajouter une tâche
  export const addTask = (task) => {
    taskList.addTask(...arguments);
  };

  // Fonction publique pour supprimer une tâche
  export const deleteTask = (id) => {
    taskList.deleteTask(id);
  };

  // Fonction publique pour obtenir une tâche
  export const getTask = (id) => {
    return taskList.getTask(id);
  };

  // Fonction publique pour filtrer les tâches
  export const filterTasks = (predicate) => {
    return taskList.filterTasks(predicate);
  };

  // Fonction publique pour obtenir toutes les tâches
  export const getAllTasks = () => {
    return taskList.getAllTasks();
  };

  // Fonction publique pour trier les tâches
  export const sortTasks = (field, order) => {
    taskList.sortTasks(field, order);
  };

  // Méthode d'initialisation (appelée automatiquement)
  (function() {
    // Ajout de quelques tâches de démonstration
    addTask(new Task('Apprendre JavaScript', 'Apprendre les bases du langage JavaScript', 'Moyenne', new Date()));
    addTask(new Task('Créer un projet', 'Créer un projet JavaScript simple', 'Haute', new Date()));
  })();

  // Retourne les fonctions publiques du module
  return {
    addTask,
    deleteTask,
    getTask,
    filterTasks,
    getAllTasks,
    sortTasks
  };
})();
```

**Explication du code :**

Ce code crée un module JavaScript qui gère une liste de tâches. Il utilise les fermetures pour fournir une encapsulation et une portée privée, ce qui permet au module de maintenir son propre état interne.

**Fonctionnalités principales :**

* Ajout, suppression, mise à jour et récupération de tâches
* Filtrage des tâches en fonction de critères
* Tri des tâches en fonction d'un champ
* Persistance des tâches dans un format JSON

**Structure du module :**

* Une fonction auto-exécutée crée un module avec une portée privée.
* Les fonctions et classes privées encapsulent la logique métier du module.
* Les fonctions publiques sont exposées via l'objet retourné par la fonction auto-exécutée.

**Classes :**

* `Task` représente une tâche individuelle avec des propriétés telles que le titre, la description, la priorité et la date d'échéance.
* `TaskList` gère la liste de tâches, permettant d'ajouter, de supprimer, de filtrer, de trier et de récupérer des tâches.

**Fonctionnalités publiques :**

* `addTask` : Ajoute une nouvelle tâche à la liste.
* `deleteTask` : Supprime une tâche par son ID.
* `getTask` : Récupère une tâche par son ID.
* `filterTasks` : Filtre les tâches en fonction d'un prédicat.
* `getAllTasks` : Récupère toutes les tâches.
* `sortTasks` : Trie les tâches en fonction d'un champ et d'un ordre.

Ce module fournit une interface complète pour gérer une liste de tâches de manière structurée et efficace.