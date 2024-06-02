**Système de Gestion de Bibliothèque**

**Classe Livre**

```java
public class Livre {
    private String titre;
    private String auteur;
    private int nbPages;
    private boolean disponible;

    public Livre(String titre, String auteur, int nbPages) {
        this.titre = titre;
        this.auteur = auteur;
        this.nbPages = nbPages;
        this.disponible = true;
    }

    public String getTitre() {
        return titre;
    }

    public String getAuteur() {
        return auteur;
    }

    public int getNbPages() {
        return nbPages;
    }

    public boolean estDisponible() {
        return disponible;
    }

    public void setDisponible(boolean disponible) {
        this.disponible = disponible;
    }

    @Override
    public String toString() {
        return "Livre{" +
                "titre='" + titre + '\'' +
                ", auteur='" + auteur + '\'' +
                ", nbPages=" + nbPages +
                ", disponible=" + disponible +
                '}';
    }
}
```

**Classe Emprunt**

```java
public class Emprunt {
    private Livre livre;
    private Client client;
    private Date dateEmprunt;
    private Date dateRetour;

    public Emprunt(Livre livre, Client client, Date dateEmprunt, Date dateRetour) {
        this.livre = livre;
        this.client = client;
        this.dateEmprunt = dateEmprunt;
        this.dateRetour = dateRetour;
    }

    public Livre getLivre() {
        return livre;
    }

    public Client getClient() {
        return client;
    }

    public Date getDateEmprunt() {
        return dateEmprunt;
    }

    public Date getDateRetour() {
        return dateRetour;
    }

    @Override
    public String toString() {
        return "Emprunt{" +
                "livre=" + livre +
                ", client=" + client +
                ", dateEmprunt=" + dateEmprunt +
                ", dateRetour=" + dateRetour +
                '}';
    }
}
```

**Classe Client**

```java
public class Client {
    private String nom;
    private String prenom;
    private String adresse;
    private String email;

    public Client(String nom, String prenom, String adresse, String email) {
        this.nom = nom;
        this.prenom = prenom;
        this.adresse = adresse;
        this.email = email;
    }

    public String getNom() {
        return nom;
    }

    public String getPrenom() {
        return prenom;
    }

    public String getAdresse() {
        return adresse;
    }

    public String getEmail() {
        return email;
    }

    @Override
    public String toString() {
        return "Client{" +
                "nom='" + nom + '\'' +
                ", prenom='" + prenom + '\'' +
                ", adresse='" + adresse + '\'' +
                ", email='" + email + '\'' +
                '}';
    }
}
```

**Classe Bibliotheque**

```java
public class Bibliotheque {
    private List<Livre> livres;
    private List<Client> clients;
    private List<Emprunt> emprunts;

    public Bibliotheque() {
        this.livres = new ArrayList<>();
        this.clients = new ArrayList<>();
        this.emprunts = new ArrayList<>();
    }

    public void ajouterLivre(Livre livre) {
        this.livres.add(livre);
    }

    public void ajouterClient(Client client) {
        this.clients.add(client);
    }

    public void enregistrerEmprunt(Emprunt emprunt) {
        this.emprunts.add(emprunt);
        livre.setDisponible(false);
    }

    public void enregistrerRetour(Emprunt emprunt) {
        this.emprunts.remove(emprunt);
        livre.setDisponible(true);
    }

    public List<Livre> rechercherLivres(String titre) {
        List<Livre> livresTrouves = new ArrayList<>();
        for (Livre livre : this.livres) {
            if (livre.getTitre().contains(titre)) {
                livresTrouves.add(livre);
            }
        }
        return livresTrouves;
    }

    public List<Client> rechercherClients(String nom) {
        List<Client> clientsTrouves = new ArrayList<>();
        for (Client client : this.clients) {
            if (client.getNom().contains(nom)) {
                clientsTrouves.add(client);
            }
        }
        return clientsTrouves;
    }

    public List<Emprunt> rechercherEmprunts(Livre livre) {
        List<Emprunt> empruntsTrouves = new ArrayList<>();
        for (Emprunt emprunt : this.emprunts) {
            if (emprunt.getLivre().equals(livre)) {
                empruntsTrouves.add(emprunt);
            }
        }
        return empruntsTrouves;
    }

    @Override
    public String toString() {
        return "Bibliotheque{" +
                "livres=" + livres +
                ", clients=" + clients +
                ", emprunts=" + emprunts +
                '}';
    }
}
```

**Interface graphique (GUI) avec JavaFX**

**Classe principale**

```java
public class BibliothequeGUI extends Application {
    private Bibliotheque bibliotheque = new Bibliotheque();

    @Override
    public void start(Stage primaryStage) {
        primaryStage.setTitle("Gestion de bibliothèque");

        // Création du layout principal
        BorderPane root = new BorderPane();

        // Création du menu
        MenuBar menuBar = new MenuBar();
        Menu menuFichier = new Menu("Fichier");
        MenuItem menuFichierQuitter = new MenuItem("Quitter");
        menuFichier.getItems().add(menuFichierQuitter);
        menuBar.getMenus().add(menuFichier);

        // Création du tableau de livres
        TableView<Livre> tableViewLivres = new TableView<>();
        TableColumn<Livre, String> colonneTitre = new TableColumn<>("Titre");
        colonneTitre.setCellValueFactory(new PropertyValueFactory<>("titre"));
        tableViewLivres.getColumns().add(colonneTitre);
        TableColumn<Livre, String> colonneAuteur = new TableColumn<>("Auteur");
        colonneAuteur.setCellValueFactory(new PropertyValueFactory<>("auteur"));
        tableViewLivres.getColumns().add(colonneAuteur);
        TableColumn<Livre, Integer> colonneNbPages = new TableColumn<>("Nb pages");
        colonneNbPages.setCellValueFactory(new PropertyValueFactory<>("nbPages"));
        tableViewLivres.getColumns().add(colonneNbPages);

        // Création du bouton d'ajout de livre
        Button buttonAjouterLivre = new Button("Ajouter un livre");
        buttonAjouterLivre.setOnAction(event -> {
            // Affichage de la fenêtre d'ajout de livre
            Dialog<Livre> dialogAjouterLivre = new Dialog<>();
            dialogAjouterLivre.setTitle("Ajouter un livre");
            dialogAjouterLivre.setHeaderText("Entrez les informations du livre");

            // Création du formulaire
            GridPane gridPane = new GridPane();
            gridPane.setHgap(10);
            gridPane.setVgap(10);
            gridPane.setPadding(new Insets(10, 10, 10, 10));

            TextField textFieldTitre = new TextField();
            textFieldTitre.setPromptText("Titre");
            gridPane.add(textFieldTitre, 0, 0);

            TextField textFieldAuteur = new TextField();
            textFieldAuteur.setPromptText("Auteur");
            gridPane.add(textFieldAuteur, 0, 1);

            TextField textFieldNbPages = new TextField();
            textFieldNbPages.setPromptText("Nombre de pages");
            gridPane.add(textFieldNbPages, 0, 2);

            // Création des boutons
            ButtonType buttonTypeAjouter = new ButtonType("Ajouter");
            ButtonType buttonTypeAnnuler = new ButtonType("Annuler");
            dialogAjouterLivre.getDialogPane().getButtonTypes().addAll(buttonTypeAjouter, buttonTypeAnnuler);

            // Ajout du formulaire au dialogue
            dialogAjouterLivre.getDialogPane().setContent(gridPane);

            // Traitement du résultat
            Optional<Livre> optionalLivre = dialogAjouterLivre.showAndWait();
            if (optionalLivre.isPresent()) {
                Livre livre = optionalLivre.get();
                bibliotheque.ajouterLivre(livre);
                tableViewLivres.getItems().add(livre);
            }
        });

        // Création de la barre d'outils
        ToolBar toolBar = new ToolBar();
        toolBar.getItems().add(buttonAjouterLivre);

        // Ajout des éléments à la fenêtre principale
        root.setTop(menuBar);
        root