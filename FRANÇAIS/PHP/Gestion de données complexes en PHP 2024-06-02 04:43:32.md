**Classe de gestion des données complexes en PHP**

```php
<?php

/**
 * Classe de gestion des données complexes
 *
 * Cette classe permet de manipuler des données complexes de manière efficace et extensible.
 */
class DataManager
{
    /**
     * Tableau des données
     *
     * @var array
     */
    private $data = [];

    /**
     * Ajoute un élément de données
     *
     * @param string $key Clé de l'élément
     * @param mixed $value Valeur de l'élément
     */
    public function add(string $key, $value): void
    {
        $this->data[$key] = $value;
    }

    /**
     * Récupère un élément de données
     *
     * @param string $key Clé de l'élément
     * @return mixed|null Valeur de l'élément ou null si l'élément n'existe pas
     */
    public function get(string $key)
    {
        return $this->data[$key] ?? null;
    }

    /**
     * Supprime un élément de données
     *
     * @param string $key Clé de l'élément
     */
    public function remove(string $key): void
    {
        if (isset($this->data[$key])) {
            unset($this->data[$key]);
        }
    }

    /**
     * Itère sur les éléments de données
     *
     * @param callable $callback Fonction appelée pour chaque élément
     */
    public function foreach(callable $callback): void
    {
        foreach ($this->data as $key => $value) {
            $callback($key, $value);
        }
    }

    /**
     * Filtre les éléments de données
     *
     * @param callable $callback Fonction de filtrage
     * @return array Tableau des éléments filtrés
     */
    public function filter(callable $callback): array
    {
        return array_filter($this->data, $callback);
    }

    /**
     * Trie les éléments de données
     *
     * @param callable $callback Fonction de tri
     */
    public function sort(callable $callback): void
    {
        usort($this->data, $callback);
    }

    /**
     * Convertit les données en format JSON
     *
     * @return string Chaîne JSON représentant les données
     */
    public function toJson(): string
    {
        return json_encode($this->data);
    }
}
```

**Utilisation de la classe**

```php
$dataManager = new DataManager();

// Ajouter des éléments de données
$dataManager->add('nom', 'John Doe');
$dataManager->add('age', 30);
$dataManager->add('adresse', '123 Main Street');

// Récupérer un élément de données
echo $dataManager->get('nom'); // Affiche "John Doe"

// Supprimer un élément de données
$dataManager->remove('age');

// Itérer sur les éléments de données
$dataManager->foreach(function ($key, $value) {
    echo "$key: $value\n";
});

// Filtrer les éléments de données
$filteredData = $dataManager->filter(function ($key, $value) {
    return is_numeric($value);
});

// Trier les éléments de données
$dataManager->sort(function ($a, $b) {
    return strcmp($a['nom'], $b['nom']);
});

// Convertir les données en format JSON
$jsonString = $dataManager->toJson();

// Afficher les données en format JSON
echo $jsonString;
```

**Explication du code**

Ce code crée une classe appelée `DataManager` qui fournit un moyen flexible et extensible de manipuler des données complexes. Voici une explication brève de certaines des méthodes clés de la classe :

* **`add()` et `get()`:** Ces méthodes permettent d'ajouter et de récupérer des éléments de données individuels.
* **`remove()`:** Cette méthode supprime un élément de données spécifique.
* **`foreach()`:** Cette méthode itère sur tous les éléments de données et appelle une fonction de rappel pour chaque élément.
* **`filter()`:** Cette méthode filtre les éléments de données en fonction d'une fonction de filtrage donnée.
* **`sort()`:** Cette méthode trie les éléments de données en fonction d'une fonction de tri donnée.
* **`toJson()`:** Cette méthode convertit les données en format JSON.

Le code d'utilisation illustre comment utiliser les différentes méthodes de la classe `DataManager` pour manipuler et traiter des données complexes. Il montre également comment convertir les données en format JSON pour une utilisation ultérieure.