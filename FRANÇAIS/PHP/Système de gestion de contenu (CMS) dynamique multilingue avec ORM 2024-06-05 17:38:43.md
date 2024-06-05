**Système de gestion de contenu (CMS) dynamique avec prise en charge multi-langues et ORM**

Ce code crée un CMS avancé qui permet de gérer des pages, des articles, des images et d'autres types de contenu de manière dynamique. Il prend en charge plusieurs langues et utilise un ORM (Object-Relational Mapping) pour faciliter l'accès et la manipulation des données.

**Connexion à la base de données et ORM**

```php
// Connexion à la base de données
$db = new PDO('mysql:host=localhost;dbname=cms', 'root', 'password');

// Création de l'ORM (Doctrine 2)
$em = EntityManager::create($db);
```

**Classe de base de contenu**

```php
abstract class Content {
    /** @var int L'identifiant du contenu */
    protected $id;

    /** @var string Le titre du contenu */
    protected $title;

    /** @var string Le contenu HTML du contenu */
    protected $body;

    /** @var DateTime La date de création du contenu */
    protected $createdAt;

    /** @var DateTime La date de mise à jour du contenu */
    protected $updatedAt;

    // ... Constructeur, setters et getters
}
```

**Classes spécifiques de contenu (Exemple : Page et Article)**

```php
class Page extends Content {
    /** @var string Le slug de la page */
    protected $slug;
}

class Article extends Content {
    /** @var Category La catégorie de l'article */
    protected $category;
}
```

**Gestionnaire de contenu**

```php
class ContentManager {
    /** @var EntityManager L'ORM */
    protected $em;

    public function __construct(EntityManager $em) {
        $this->em = $em;
    }

    public function createContent(Content $content) {
        $this->em->persist($content);
        $this->em->flush();
    }

    public function updateContent(Content $content) {
        $this->em->merge($content);
        $this->em->flush();
    }

    public function deleteContent(Content $content) {
        $this->em->remove($content);
        $this->em->flush();
    }

    // ... Autres méthodes de recherche, filtrage et gestion
}
```

**Gestionnaire de langue**

```php
class LanguageManager {
    /** @var array Liste des langues supportées */
    protected $languages = ['en', 'fr', 'es'];

    public function getLanguages() {
        return $this->languages;
    }

    public function getCurrentLanguage() {
        // Obtention de la langue courante à partir de la requête HTTP
        return $_GET['lang'] ?? 'en';
    }

    public function translateContent(Content $content, string $lang) {
        // Traduction du titre et du corps du contenu dans la langue spécifiée
        $content->setTitle(translate($content->getTitle(), $lang));
        $content->setBody(translate($content->getBody(), $lang));
    }
}
```

**Mise en œuvre du CMS**

```php
// Initialisation du CMS
$langManager = new LanguageManager();
$currentLang = $langManager->getCurrentLanguage();
$contentManager = new ContentManager($em);

// Gestion des actions de l'interface utilisateur (par exemple, créer, modifier ou supprimer du contenu)

// Affichage du contenu dans la langue courante
foreach ($contentManager->findAll() as $content) {
    $langManager->translateContent($content, $currentLang);
    echo $content->getTitle() . '<br>' . $content->getBody();
}
```

**Ce code permet de créer un CMS dynamique et évolutif, avec une prise en charge multi-langues et un ORM pour une manipulation facile des données.**