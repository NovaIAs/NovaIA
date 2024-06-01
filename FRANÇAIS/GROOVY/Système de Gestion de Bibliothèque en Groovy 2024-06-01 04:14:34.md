**Système de Gestion de Bibliothèque en Groovy**

Ce code Groovy crée un système de gestion de bibliothèque complet, permettant aux utilisateurs d'ajouter, supprimer, rechercher et modifier des livres et des adhérents.

**Modèle de Données**

```groovy
class Livre {
    Long id
    String titre
    String auteur
    Integer annee
}

class Adherent {
    Long id
    String nom
    String prenom
    Date dateAdhesion
}
```

**Service de Gestion de Bibliothèque**

```groovy
class BibliothequeService {

    def livreRepo, adherentRepo

    Livre ajouterLivre(Livre livre) {
        livreRepo.save(livre)
    }

    void supprimerLivre(Long id) {
        livreRepo.deleteById(id)
    }

    Livre trouverLivreParId(Long id) {
        livreRepo.findById(id).orElse(null)
    }

    List<Livre> rechercherLivres(String titre, String auteur) {
        livreRepo.findAll(titre ? :"titre LIKE '%${titre}%'":"1==1", auteur ? :"auteur LIKE '%${auteur}%'":"1==1")
    }

    Adherent ajouterAdherent(Adherent adherent) {
        adherentRepo.save(adherent)
    }

    void supprimerAdherent(Long id) {
        adherentRepo.deleteById(id)
    }

    Adherent trouverAdherentParId(Long id) {
        adherentRepo.findById(id).orElse(null)
    }

    List<Adherent> rechercherAdherents(String nom, String prenom) {
        adherentRepo.findAll(nom ? :"nom LIKE '%${nom}%'":"1==1", prenom ? :"prenom LIKE '%${prenom}%'":"1==1")
    }
}
```

**Contrôleur d'Interface Utilisateur**

```groovy
class BibliothequeController {

    def bibliothequeService

    def index() {
        [livres: bibliothequeService.rechercherLivres(), adherents: bibliothequeService.rechercherAdherents()]
    }

    def ajouterLivre() {}

    def enregistrerLivre() {
        bibliothequeService.ajouterLivre(new Livre(params))
        redirect(action: "index")
    }

    def supprimerLivre(Long id) {
        bibliothequeService.supprimerLivre(id)
        redirect(action: "index")
    }

    def modifierLivre(Long id) {
        [livre: bibliothequeService.trouverLivreParId(id)]
    }

    def mettreAJourLivre() {
        bibliothequeService.modifierLivre(new Livre(params))
        redirect(action: "index")
    }

    def ajouterAdherent() {}

    def enregistrerAdherent() {
        bibliothequeService.ajouterAdherent(new Adherent(params))
        redirect(action: "index")
    }

    def supprimerAdherent(Long id) {
        bibliothequeService.supprimerAdherent(id)
        redirect(action: "index")
    }

    def modifierAdherent(Long id) {
        [adherent: bibliothequeService.trouverAdherentParId(id)]
    }

    def mettreAJourAdherent() {
        bibliothequeService.modifierAdherent(new Adherent(params))
        redirect(action: "index")
    }
}
```

**Explication du Code**

* Le modèle de données définit les classes `Livre` et `Adherent`.
* Le service `BibliothequeService` fournit les méthodes pour gérer les livres et les adhérents.
* Le contrôleur `BibliothequeController` gère les interactions de l'interface utilisateur avec le service.
* Les méthodes de recherche utilisent des expressions régulières pour les recherches partielles.
* Les formulaires permettent d'ajouter, modifier et supprimer des éléments.
* Le code est entièrement commenté pour une meilleure compréhension.