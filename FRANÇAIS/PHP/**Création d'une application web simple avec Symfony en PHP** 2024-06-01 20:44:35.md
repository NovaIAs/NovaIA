```php
<?php

// Charger le framework Symphony
require_once 'vendor/autoload.php';
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Matcher\UrlMatcher;
use Symfony\Component\Routing\RequestContext;
use Symfony\Component\Routing\RouteCollection;
use Symfony\Component\Routing\Route;

// Créer une collection de routes
$routes = new RouteCollection();
$routes->add('accueil', new Route('/', ['controller' => 'AcceuilController', 'action' => 'index']));
$routes->add('article', new Route('/article/{id}', ['controller' => 'ArticleController', 'action' => 'show']));

// Créer un contexte de requête
$context = new RequestContext();
$context->fromRequest(Request::createFromGlobals());

// Créer un matcher d'URL
$matcher = new UrlMatcher($routes, $context);

// Récupérer la route correspondante à la requête
try {
    $match = $matcher->match('/');
} catch (ResourceNotFoundException $e) {
    $response = new Response('404 Not Found', 404);
    $response->send();
    exit;
}

// Récupérer le contrôleur et l'action de la route
$controllerName = $match['_controller'];
$actionName = $match['_action'];

// Créer une instance du contrôleur
$controller = new $controllerName();

// Appeler l'action du contrôleur
$response = $controller->$actionName($match);

// Envoyer la réponse au client
$response->send();
```

**Explication du code :**

Ce code utilise le framework Symfony pour créer une application web simple.

* La première partie du code charge le framework Symfony.
* La deuxième partie crée une collection de routes. Une route est une URL qui correspond à une action spécifique dans un contrôleur.
* La troisième partie crée un contexte de requête. Le contexte de requête contient des informations sur la requête HTTP actuelle, telles que l'URL et la méthode HTTP.
* La quatrième partie crée un matcher d'URL. Le matcher d'URL compare l'URL de la requête actuelle à la collection de routes et renvoie la route correspondante.
* La cinquième partie récupère la route correspondante à la requête actuelle. Si aucune route correspondante n'est trouvée, une exception `ResourceNotFoundException` est levée.
* La sixième partie récupère le contrôleur et l'action de la route. Le contrôleur est une classe qui contient les actions. Une action est une méthode dans un contrôleur qui gère une demande HTTP spécifique.
* La septième partie crée une instance du contrôleur.
* La huitième partie appelle l'action du contrôleur. L'action reçoit les paramètres de la route en tant qu'argument.
* La neuvième partie envoie la réponse au client. La réponse est un objet `Response` qui contient le contenu de la page web.