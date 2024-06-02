**Classe de service de gestion des utilisateurs**

```groovy
class ServiceGestionUtilisateurs {

    // Injecte le référentiel d'utilisateurs
    @Autowired
    private UserRepository repUtilisateurs

    // Enregistre un nouvel utilisateur
    Utilisateur enregistrerUtilisateur(Utilisateur utilisateur) {
        // Vérifie si l'utilisateur existe déjà
        Utilisateur utilisateurExistant = repUtilisateurs.findByNomUtilisateur(utilisateur.nomUtilisateur)
        if (utilisateurExistant) {
            throw new Exception("Nom d'utilisateur déjà utilisé")
        }

        // Encode le mot de passe
        utilisateur.motDePasse = encodeurMotDePasse.encoder(utilisateur.motDePasse)

        // Sauvegarde l'utilisateur
        return repUtilisateurs.save(utilisateur)
    }

    // Met à jour les informations d'un utilisateur
    Utilisateur modifierUtilisateur(Utilisateur utilisateur) {
        // Récupère l'utilisateur existant
        Utilisateur utilisateurExistant = repUtilisateurs.findOne(utilisateur.id)

        // Met à jour les informations
        utilisateurExistant.nom = utilisateur.nom
        utilisateurExistant.prenom = utilisateur.prenom
        utilisateurExistant.email = utilisateur.email
        if (utilisateur.motDePasse) {
            // Encode le nouveau mot de passe
            utilisateurExistant.motDePasse = encodeurMotDePasse.encoder(utilisateur.motDePasse)
        }

        // Sauvegarde l'utilisateur
        return repUtilisateurs.save(utilisateurExistant)
    }

    // Supprime un utilisateur
    void supprimerUtilisateur(Long idUtilisateur) {
        // Récupère l'utilisateur
        Utilisateur utilisateur = repUtilisateurs.findOne(idUtilisateur)

        // Supprime l'utilisateur
        repUtilisateurs.delete(utilisateur)
    }

    // Recherche un utilisateur par son nom d'utilisateur
    Utilisateur rechercherParNomUtilisateur(String nomUtilisateur) {
        return repUtilisateurs.findByNomUtilisateur(nomUtilisateur)
    }

    // Recherche tous les utilisateurs
    List<Utilisateur> rechercherTous() {
        return repUtilisateurs.findAll()
    }
}
```

**Classe de contrôleur pour la gestion des utilisateurs**

```groovy
@RestController
@RequestMapping("/utilisateurs")
class ControleurUtilisateurs {

    // Injecte le service de gestion des utilisateurs
    @Autowired
    private ServiceGestionUtilisateurs serviceGestionUtilisateurs

    // Enregistre un nouvel utilisateur
    @PostMapping
    Utilisateur enregistrerUtilisateur(@RequestBody Utilisateur utilisateur) {
        return serviceGestionUtilisateurs.enregistrerUtilisateur(utilisateur)
    }

    // Met à jour les informations d'un utilisateur
    @PutMapping("/{idUtilisateur}")
    Utilisateur modifierUtilisateur(@PathVariable Long idUtilisateur, @RequestBody Utilisateur utilisateur) {
        utilisateur.id = idUtilisateur
        return serviceGestionUtilisateurs.modifierUtilisateur(utilisateur)
    }

    // Supprime un utilisateur
    @DeleteMapping("/{idUtilisateur}")
    void supprimerUtilisateur(@PathVariable Long idUtilisateur) {
        serviceGestionUtilisateurs.supprimerUtilisateur(idUtilisateur)
    }

    // Recherche un utilisateur par son nom d'utilisateur
    @GetMapping("/{nomUtilisateur}")
    Utilisateur rechercherParNomUtilisateur(@PathVariable String nomUtilisateur) {
        return serviceGestionUtilisateurs.rechercherParNomUtilisateur(nomUtilisateur)
    }

    // Recherche tous les utilisateurs
    @GetMapping
    List<Utilisateur> rechercherTous() {
        return serviceGestionUtilisateurs.rechercherTous()
    }
}
```

**Classe Entity Utilisateur**

```groovy
@Entity
class Utilisateur {

    @Id
    @GeneratedValue
    Long id

    String nomUtilisateur
    String motDePasse
    String nom
    String prenom
    String email
}
```

**Interface UserRepository**

```groovy
interface UserRepository extends JpaRepository<Utilisateur, Long> {

    Utilisateur findByNomUtilisateur(String nomUtilisateur)
}
```

**Explication du code**

Ce code définit une classe de service `ServiceGestionUtilisateurs` qui gère les opérations CRUD (Création, Récupération, Mise à jour, Suppression) sur les utilisateurs. Il injecte le référentiel d'utilisateurs `UserRepository` et utilise ses méthodes pour effectuer les opérations de base de données.

Le contrôleur `ControleurUtilisateurs` expose les méthodes de service via des points de terminaison RESTful. Il mappe les demandes HTTP aux méthodes de service correspondantes.

L'entité `Utilisateur` représente le modèle de données pour les utilisateurs, avec des champs pour l'identifiant, le nom d'utilisateur, le mot de passe, le nom, le prénom et l'email.

L'interface `UserRepository` étend `JpaRepository`, qui fournit des méthodes génériques pour les opérations CRUD. Elle définit également une méthode personnalisée pour rechercher un utilisateur par son nom d'utilisateur.

Ce code offre une couche de service et de contrôleur complète pour gérer les utilisateurs dans une application Spring Boot. Il illustre l'utilisation de diverses annotations telles que `@Autowired`, `@RestController`, `@RequestMapping`, `@GetMapping`, `@PostMapping` et `@PutMapping`, ainsi que l'intégration du référentiel JPA pour les opérations de base de données.