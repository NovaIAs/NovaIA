**Gestionnaire hiérarchique de tâches avec système de notification et journalisation**

**Description :**

Ce code met en œuvre un gestionnaire hiérarchique de tâches avec un système de notification et de journalisation sophistiqué. Il permet aux utilisateurs de créer, attribuer, suivre et gérer des tâches de manière structurée et efficace.

**Structure du code :**

Le code est organisé en plusieurs classes et interfaces :

* **TâcheManager:** La classe principale qui gère toutes les opérations liées aux tâches, telles que la création, la mise à jour et la suppression.
* **Tâche:** L'interface qui représente une tâche individuelle, avec des méthodes pour accéder et modifier ses propriétés (nom, description, statut, etc.).
* **Utilisateur:** L'interface qui représente un utilisateur du système, avec des méthodes pour accéder et modifier ses informations (nom, e-mail, etc.).
* **NotificationManager:** La classe qui gère les notifications, en envoyant des e-mails ou des SMS aux utilisateurs concernés.
* **Logger:** La classe qui gère la journalisation, en enregistrant les événements importants dans un fichier ou une base de données.

**Flux de travail :**

1. Création de tâches : Les utilisateurs peuvent créer de nouvelles tâches à l'aide de la méthode `TâcheManager::createTâche()`.
2. Attribution de tâches : Les tâches peuvent être attribuées à des utilisateurs spécifiques à l'aide de la méthode `TâcheManager::assignerTâche()`.
3. Suivi des tâches : Les utilisateurs peuvent visualiser et mettre à jour l'état des tâches qui leur sont attribuées à l'aide de la méthode `TâcheManager::getMesTâches()`.
4. Notifications : Lorsqu'une tâche est créée, modifiée ou terminée, un e-mail ou un SMS est envoyé aux utilisateurs concernés à l'aide de la classe `NotificationManager`.
5. Journalisation : Les événements importants, tels que la création ou la mise à jour d'une tâche, sont enregistrés dans un fichier ou une base de données à l'aide de la classe `Logger`.

**Exemple d'utilisation :**

```php
<?php

use App\TâcheManager;
use App\NotificationManager;
use App\Logger;

// Création d'un gestionnaire de tâches
$tâcheManager = new TâcheManager();

// Création d'une nouvelle tâche
$tâche = $tâcheManager->createTâche("Acheter du lait");

// Attribution de la tâche à un utilisateur
$utilisateur = new Utilisateur("Jean Dupont");
$tâcheManager->assignerTâche($tâche, $utilisateur);

// Mise à jour de la tâche
$tâche->setStatut("En cours");

// Envoi d'une notification à l'utilisateur
$notificationManager = new NotificationManager();
$notificationManager->envoyerNotification($utilisateur, $tâche);

// Enregistrement de l'événement dans le journal
$logger = new Logger();
$logger->enregistrer("Mise à jour de la tâche \"Acheter du lait\"");

?>
```

**Explication du code :**

* Les espaces de noms `use` sont utilisés pour importer les classes nécessaires.
* Le gestionnaire de tâches est créé à l'aide de `new TâcheManager()`.
* La tâche est créée à l'aide de `$tâcheManager->createTâche()` et attribuée à un utilisateur à l'aide de `$tâcheManager->assignerTâche()`.
* L'état de la tâche est mis à jour à l'aide de `$tâche->setStatut()`.
* Une notification est envoyée à l'utilisateur à l'aide de `$notificationManager->envoyerNotification()`.
* L'événement est enregistré dans le journal à l'aide de `$logger->enregistrer()`.

Ce code fournit un exemple de base d'utilisation du gestionnaire de tâches, mais il peut être facilement étendu pour inclure des fonctionnalités supplémentaires, telles que les listes de tâches hiérarchiques, les rappels et les rapports de progression.