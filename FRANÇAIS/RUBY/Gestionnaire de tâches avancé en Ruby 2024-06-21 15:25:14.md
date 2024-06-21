**Gestionnaire de tâches complexe en Ruby**

Ce code Ruby met en œuvre un gestionnaire de tâches complexe doté des fonctionnalités suivantes :

* **Création, mise à jour et suppression de tâches :** Chaque tâche a un titre, une description, une priorité et une date d'échéance.
* **Gestion des dépendances entre tâches :** Les tâches peuvent dépendre les unes des autres, ce qui détermine leur ordre d'exécution.
* **Suivi de l'avancement des tâches :** Le statut de chaque tâche peut être suivi (créée, en cours, terminée).
* **Notifications par e-mail :** L'utilisateur peut être averti par e-mail lorsque les tâches sont créées, modifiées ou terminées.

**Code :**

```ruby
class TaskManager
  attr_accessor :tasks

  def initialize
    @tasks = []
  end

  def create_task(title, description, priority, due_date)
    task = Task.new(title, description, priority, due_date)
    @tasks << task
    notify_email(task, "created")
  end

  def update_task(id, title, description, priority, due_date)
    task = find_task(id)
    task.title = title
    task.description = description
    task.priority = priority
    task.due_date = due_date
    notify_email(task, "updated")
  end

  def delete_task(id)
    task = find_task(id)
    @tasks.delete(task)
    notify_email(task, "deleted")
  end

  def find_task(id)
    @tasks.find { |task| task.id == id }
  end

  def notify_email(task, action)
    # Implémentation de l'envoi d'e-mails (non incluse dans ce code)
  end

  # Gestion des dépendances...

  # Suivi de l'avancement...
end

class Task
  attr_accessor :id, :title, :description, :priority, :due_date, :status

  def initialize(title, description, priority, due_date)
    @id = SecureRandom.uuid
    @title = title
    @description = description
    @priority = priority
    @due_date = due_date
    @status = "créée"
  end

  # Méthodes d'accès et de modification...
end
```

**Explication du code :**

* La classe `TaskManager` gère la collection de tâches.
* La classe `Task` représente une tâche individuelle.
* Les méthodes `create_task`, `update_task` et `delete_task` permettent de manipuler les tâches.
* La méthode `find_task` recherche une tâche par son identifiant.
* La méthode `notify_email` envoie des notifications par e-mail (non implémentée dans ce code).
* Les dépendances et le suivi de l'avancement sont traités séparément (non inclus dans ce code).
* Le module `SecureRandom` génère des identifiants uniques pour les tâches.