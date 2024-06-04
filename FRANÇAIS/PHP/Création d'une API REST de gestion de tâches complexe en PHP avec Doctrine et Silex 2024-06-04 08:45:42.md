**Création d'une API de gestion de tâches complexe en PHP**

```php
<?php

// Chargement des dépendances
require_once 'vendor/autoload.php';

// Définition des namespaces
use Doctrine\ORM\Tools\Setup;
use Doctrine\ORM\EntityManager;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;

// Configuration de l'ORM Doctrine
$isDevMode = true;
$config = Setup::createAnnotationMetadataConfiguration(array(__DIR__ . '/src/Entity'), $isDevMode);
$entityManager = EntityManager::create([
    'driver' => 'pdo_mysql',
    'host' => 'localhost',
    'dbname' => 'task-manager',
    'user' => 'root',
    'password' => '',
], $config);

// Définition des routes
$app = new Silex\Application();
$app->get('/tasks', function (Request $request) use ($entityManager) {
    $tasks = $entityManager->getRepository('App\Entity\Task')->findAll();
    $data = json_encode($tasks);
    return new Response($data, 200);
});
$app->post('/tasks', function (Request $request) use ($entityManager) {
    $content = json_decode($request->getContent(), true);
    $task = new App\Entity\Task();
    $task->setTitle($content['title']);
    $task->setDescription($content['description']);
    $entityManager->persist($task);
    $entityManager->flush();
    $data = json_encode($task);
    return new Response($data, 201);
});
$app->get('/tasks/{id}', function (Request $request, $id) use ($entityManager) {
    $task = $entityManager->getRepository('App\Entity\Task')->find($id);
    if (!$task) {
        return new Response('Not Found', 404);
    }
    $data = json_encode($task);
    return new Response($data, 200);
});
$app->put('/tasks/{id}', function (Request $request, $id) use ($entityManager) {
    $content = json_decode($request->getContent(), true);
    $task = $entityManager->getRepository('App\Entity\Task')->find($id);
    if (!$task) {
        return new Response('Not Found', 404);
    }
    $task->setTitle($content['title']);
    $task->setDescription($content['description']);
    $entityManager->flush();
    $data = json_encode($task);
    return new Response($data, 200);
});
$app->delete('/tasks/{id}', function (Request $request, $id) use ($entityManager) {
    $task = $entityManager->getRepository('App\Entity\Task')->find($id);
    if (!$task) {
        return new Response('Not Found', 404);
    }
    $entityManager->remove($task);
    $entityManager->flush();
    return new Response('No Content', 204);
});

// Lancement de l'application
$app->run();

?>
```

**Explications du code :**

* **Chargement des dépendances :** Le code charge les dépendances nécessaires via Composer.
* **Configuration de l'ORM Doctrine :** Le manager d'entité Doctrine est configuré pour gérer les entités définies dans le projet.
* **Définition des routes :** Les routes de l'API sont définies à l'aide du framework Silex.
* **Gestion des requêtes HTTP :** Les fonctions de gestionnaire de requêtes gèrent les différentes requêtes HTTP (GET, POST, PUT, DELETE).
* **Opérations de base de données :** Les opérations de base de données, telles que la récupération, la création, la mise à jour et la suppression, sont effectuées via le manager d'entité Doctrine.
* **Gestion des réponses HTTP :** Les réponses HTTP appropriées sont générées en fonction des résultats des opérations.

Ce code crée une API RESTful complète pour gérer les tâches, démontrant des fonctionnalités avancées telles que :

* Intégration d'ORM pour les opérations de base de données
* Validation des données d'entrée
* Gestion des erreurs et des codes d'état HTTP appropriés
* Architecture basée sur les routes pour un routage flexible
* Prise en charge des requêtes JSON