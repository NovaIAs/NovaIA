**Système de gestion d'inventaire avancé**

**Classe principale**

```java
public class GestionInventaire {

    // Liste des articles
    private List<Article> articles = new ArrayList<>();

    // Liste des commandes
    private List<Commande> commandes = new ArrayList<>();

    // Liste des clients
    private List<Client> clients = new ArrayList<>();

    // Constructeur
    public GestionInventaire() {
        // Initialisation des listes
        articles.add(new Article("Livre", 10.0, 20));
        articles.add(new Article("Stylo", 5.0, 50));
        articles.add(new Article("Cahier", 7.0, 30));

        clients.add(new Client("Jean", "Dupont"));
        clients.add(new Client("Alice", "Martin"));
    }

    // Méthodes de gestion des articles
    public void ajouterArticle(Article article) {
        articles.add(article);
    }

    public void supprimerArticle(Article article) {
        articles.remove(article);
    }

    public void modifierArticle(Article article) {
        // Recherche de l'article à modifier dans la liste
        int index = articles.indexOf(article);
        if (index != -1) {
            // Remplacement de l'article dans la liste
            articles.set(index, article);
        }
    }

    // Méthodes de gestion des commandes
    public void passerCommande(Commande commande) {
        commandes.add(commande);
    }

    public void annulerCommande(Commande commande) {
        commandes.remove(commande);
    }

    public void modifierCommande(Commande commande) {
        // Recherche de la commande à modifier dans la liste
        int index = commandes.indexOf(commande);
        if (index != -1) {
            // Remplacement de la commande dans la liste
            commandes.set(index, commande);
        }
    }

    // Méthodes de gestion des clients
    public void ajouterClient(Client client) {
        clients.add(client);
    }

    public void supprimerClient(Client client) {
        clients.remove(client);
    }

    public void modifierClient(Client client) {
        // Recherche du client à modifier dans la liste
        int index = clients.indexOf(client);
        if (index != -1) {
            // Remplacement du client dans la liste
            clients.set(index, client);
        }
    }

    // Méthodes de rapport
    public void genererRapportInventaire() {
        // Affichage de la liste des articles avec leur quantité en stock
        System.out.println("Rapport d'inventaire :");
        for (Article article : articles) {
            System.out.println(article.getNom() + " : " + article.getQuantite());
        }
    }

    public void genererRapportCommandes() {
        // Affichage de la liste des commandes avec leurs détails
        System.out.println("Rapport des commandes :");
        for (Commande commande : commandes) {
            System.out.println("Commande n°" + commande.getNumero() + " :");
            for (LigneCommande ligneCommande : commande.getLignesCommande()) {
                System.out.println(" - " + ligneCommande.getArticle().getNom() + " x " + ligneCommande.getQuantite());
            }
        }
    }

    public void genererRapportClients() {
        // Affichage de la liste des clients
        System.out.println("Rapport des clients :");
        for (Client client : clients) {
            System.out.println(client.getPrenom() + " " + client.getNom());
        }
    }
}
```

**Classes d'entité**

```java
public class Article {

    private String nom;
    private double prix;
    private int quantite;

    public Article(String nom, double prix, int quantite) {
        this.nom = nom;
        this.prix = prix;
        this.quantite = quantite;
    }

    // Getters et setters
}

public class Commande {

    private int numero;
    private List<LigneCommande> lignesCommande = new ArrayList<>();
    private Client client;

    public Commande(int numero, Client client) {
        this.numero = numero;
        this.client = client;
    }

    // Getters et setters
}

public class LigneCommande {

    private Article article;
    private int quantite;

    public LigneCommande(Article article, int quantite) {
        this.article = article;
        this.quantite = quantite;
    }

    // Getters et setters
}

public class Client {

    private String prenom;
    private String nom;
    private List<Commande> commandes = new ArrayList<>();

    public Client(String prenom, String nom) {
        this.prenom = prenom;
        this.nom = nom;
    }

    // Getters et setters
}
```

**Utilisation**

```java
public class Main {

    public static void main(String[] args) {
        // Création d'une instance du système de gestion d'inventaire
        GestionInventaire gestionInventaire = new GestionInventaire();

        // Ajout d'articles
        Article article1 = new Article("Livre", 10.0, 20);
        Article article2 = new Article("Stylo", 5.0, 50);
        Article article3 = new Article("Cahier", 7.0, 30);
        gestionInventaire.ajouterArticle(article1);
        gestionInventaire.ajouterArticle(article2);
        gestionInventaire.ajouterArticle(article3);

        // Ajout de clients
        Client client1 = new Client("Jean", "Dupont");
        Client client2 = new Client("Alice", "Martin");
        gestionInventaire.ajouterClient(client1);
        gestionInventaire.ajouterClient(client2);

        // Passage d'une commande
        Commande commande1 = new Commande(1, client1);
        commande1.ajouterLigneCommande(article1, 5);
        commande1.ajouterLigneCommande(article2, 10);
        gestionInventaire.passerCommande(commande1);

        // Génération de rapports
        gestionInventaire.genererRapportInventaire();
        gestionInventaire.genererRapportCommandes();
        gestionInventaire.genererRapportClients();
    }
}
```

**Explication**

Ce code Java implémente un système de gestion d'inventaire avancé qui permet de gérer les articles, les commandes et les clients. Le système comprend les fonctionnalités suivantes :

* **Gestion des articles :** Ajouter, supprimer, modifier des articles et afficher leur quantité en stock.
* **Gestion des commandes :** Passer, annuler, modifier des commandes et afficher leurs détails.
* **Gestion des clients :** Ajouter, supprimer, modifier des clients et afficher leur liste.
* **Génération de rapports :** Générer des rapports d'inventaire, de commandes et de clients.

Le code est structuré en classes d'entité (Article, Commande, LigneCommande, Client) et une classe principale (GestionInventaire) qui gère toutes les fonctionnalités du système. Le code utilise des collections (List) pour stocker les articles, les commandes et les clients, ce qui permet une gestion dynamique de l'inventaire.