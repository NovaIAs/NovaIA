```php
<?php

/**
 * Cette classe représente un utilisateur du système.
 *
 * Elle possède des propriétés pour stocker les informations de l'utilisateur, telles que son nom, son identifiant et son mot de passe.
 * Elle possède également des méthodes pour créer, lire, mettre à jour et supprimer les utilisateurs de la base de données.
 */
class Utilisateur
{
    /**
     * L'identifiant de l'utilisateur.
     *
     * @var int
     */
    private $id;

    /**
     * Le nom de l'utilisateur.
     *
     * @var string
     */
    private $nom;

    /**
     * L'identifiant de l'utilisateur.
     *
     * @var string
     */
    private $login;

    /**
     * Le mot de passe de l'utilisateur.
     *
     * @var string
     */
    private $password;

    /**
     * Crée un nouvel utilisateur.
     *
     * @param string $nom Le nom de l'utilisateur.
     * @param string $login L'identifiant de l'utilisateur.
     * @param string $password Le mot de passe de l'utilisateur.
     */
    public function __construct(string $nom, string $login, string $password)
    {
        $this->nom = $nom;
        $this->login = $login;
        $this->password = $password;
    }

    /**
     * Retourne l'identifiant de l'utilisateur.
     *
     * @return int L'identifiant de l'utilisateur.
     */
    public function getId(): int
    {
        return $this->id;
    }

    /**
     * Retourne le nom de l'utilisateur.
     *
     * @return string Le nom de l'utilisateur.
     */
    public function getNom(): string
    {
        return $this->nom;
    }

    /**
     * Retourne l'identifiant de l'utilisateur.
     *
     * @return string L'identifiant de l'utilisateur.
     */
    public function getLogin(): string
    {
        return $this->login;
    }

    /**
     * Retourne le mot de passe de l'utilisateur.
     *
     * @return string Le mot de passe de l'utilisateur.
     */
    public function getPassword(): string
    {
        return $this->password;
    }

    /**
     * Enregistre l'utilisateur dans la base de données.
     *
     * @param PDO $pdo L'instance PDO de la base de données.
     */
    public function save(PDO $pdo)
    {
        $stmt = $pdo->prepare("INSERT INTO users (nom, login, password) VALUES (?, ?, ?)");
        $stmt->execute([$this->nom, $this->login, $this->password]);
        $this->id = $pdo->lastInsertId();
    }

    /**
     * Charge l'utilisateur depuis la base de données.
     *
     * @param PDO $pdo L'instance PDO de la base de données.
     * @param int $id L'identifiant de l'utilisateur.
     * @return Utilisateur L'utilisateur chargé.
     */
    public static function load(PDO $pdo, int $id): Utilisateur
    {
        $stmt = $pdo->prepare("SELECT * FROM users WHERE id = ?");
        $stmt->execute([$id]);
        $result = $stmt->fetch(PDO::FETCH_ASSOC);
        return new Utilisateur($result['nom'], $result['login'], $result['password']);
    }

    /**
     * Met à jour l'utilisateur dans la base de données.
     *
     * @param PDO $pdo L'instance PDO de la base de données.
     */
    public function update(PDO $pdo)
    {
        $stmt = $pdo->prepare("UPDATE users SET nom = ?, login = ?, password = ? WHERE id = ?");
        $stmt->execute([$this->nom, $this->login, $this->password, $this->id]);
    }

    /**
     * Supprime l'utilisateur de la base de données.
     *
     * @param PDO $pdo L'instance PDO de la base de données.
     */
    public function delete(PDO $pdo)
    {
        $stmt = $pdo->prepare("DELETE FROM users WHERE id = ?");
        $stmt->execute([$this->id]);
    }
}

/**
 * Cette classe représente un article du blog.
 *
 * Elle possède des propriétés pour stocker les informations de l'article, telles que son titre, son contenu et sa date de publication.
 * Elle possède également des méthodes pour créer, lire, mettre à jour et supprimer les articles de la base de données.
 */
class Article
{
    /**
     * L'identifiant de l'article.
     *
     * @var int
     */
    private $id;

    /**
     * Le titre de l'article.
     *
     * @var string
     */
    private $titre;

    /**
     * Le contenu de l'article.
     *
     * @var string
     */
    private $contenu;

    /**
     * La date de publication de l'article.
     *
     * @var string
     */
    private $date_publication;

    /**
     * L'auteur de l'article.
     *
     * @var Utilisateur
     */
    private $auteur;

    /**
     * Crée un nouvel article.
     *
     * @param string $titre Le titre de l'article.
     * @param string $contenu Le contenu de l'article.
     * @param string $date_publication La date de publication de l'article.
     * @param Utilisateur $auteur L'auteur de l'article.
     */
    public function __construct(string $titre, string $contenu, string $date_publication, Utilisateur $auteur)
    {
        $this->titre = $titre;
        $this->contenu = $contenu;
        $this->date_publication = $date_publication;
        $this->auteur = $auteur;
    }

    /**
     * Retourne l'identifiant de l'article.
     *
     * @return int L'identifiant de l'article.
     */
    public function getId(): int
    {
        return $this->id;
    }

    /**
     * Retourne le titre de l'article.
     *
     * @return string Le titre de l'article.
     */
    public function getTitre(): string
    {
        return $this->titre;
    }

    /**
     * Retourne le contenu de l'article.
     *
     * @return string Le contenu de l'article.
     */
    public function getContenu(): string
    {
        return $this->contenu;
    }

    /**
     * Retourne la date de publication de l'article.
     *
     * @return string La date de publication de l'article.
     */
    public function getDatePublication(): string
    {
        return $this->date_publication;
    }

    /**
     * Retourne l'auteur de l'article.
     *
     * @return Utilisateur L'auteur de l'article.
     */
    public function getAuteur(): Utilisateur
    {
        return $this->auteur;
    }

    /**
     * Enregistre l'article dans la base de données.
     *
     * @param PDO $pdo L'instance PDO de la base de données.
     */
    public function save(PDO $pdo)
    {
        $stmt = $pdo->prepare("INSERT INTO articles (titre, contenu, date_publication, auteur_id) VALUES (?, ?, ?, ?)");
        $stmt->execute([$this->titre, $this->contenu, $