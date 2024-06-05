**Gestionnaire d'utilisateurs complexe avec authentification, autorisation et audit**

**Classe Utilisateur**

```swift
class Utilisateur {
    let id: UUID
    let nom: String
    let prenom: String
    let email: String
    let motDePasse: String
    let role: Role // Voir ci-dessous

    init(id: UUID, nom: String, prenom: String, email: String, motDePasse: String, role: Role) {
        self.id = id
        self.nom = nom
        self.prenom = prenom
        self.email = email
        self.motDePasse = motDePasse
        self.role = role
    }
}
```

**Énumération Role**

```swift
enum Role: String {
    case administrateur = "ADMIN"
    case utilisateur = "USER"
}
```

**Interface GestionnaireUtilisateur**

```swift
protocol GestionnaireUtilisateur {
    func créerUtilisateur(utilisateur: Utilisateur) -> Résultat<Utilisateur, ErreurUtilisateur>
    func récupérerUtilisateur(parId id: UUID) -> Résultat<Utilisateur?, ErreurUtilisateur>
    func récupérerUtilisateurs() -> Résultat<[Utilisateur], ErreurUtilisateur>
    func mettreAJourUtilisateur(utilisateur: Utilisateur) -> Résultat<Utilisateur, ErreurUtilisateur>
    func supprimerUtilisateur(parId id: UUID) -> Résultat<Void, ErreurUtilisateur>
}
```

**Implementation de GestionnaireUtilisateur**

```swift
class GestionnaireUtilisateurImpl: GestionnaireUtilisateur {
    private let entrepôt: EntrepôtUtilisateur

    init(entrepôt: EntrepôtUtilisateur) {
        self.entrepôt = entrepôt
    }

    func créerUtilisateur(utilisateur: Utilisateur) -> Résultat<Utilisateur, ErreurUtilisateur> {
        do {
            try self.entrepôt.créer(utilisateur: utilisateur)
            return .succès(utilisateur)
        } catch let erreur as ErreurUtilisateur {
            return .échec(erreur)
        } catch {
            return .échec(ErreurUtilisateur.erreurInconnue)
        }
    }

    func récupérerUtilisateur(parId id: UUID) -> Résultat<Utilisateur?, ErreurUtilisateur> {
        do {
            let utilisateur = try self.entrepôt.récupérer(parId: id)
            return .succès(utilisateur)
        } catch let erreur as ErreurUtilisateur {
            return .échec(erreur)
        } catch {
            return .échec(ErreurUtilisateur.erreurInconnue)
        }
    }

    func récupérerUtilisateurs() -> Résultat<[Utilisateur], ErreurUtilisateur> {
        do {
            let utilisateurs = try self.entrepôt.récupérerTous()
            return .succès(utilisateurs)
        } catch let erreur as ErreurUtilisateur {
            return .échec(erreur)
        } catch {
            return .échec(ErreurUtilisateur.erreurInconnue)
        }
    }

    func mettreAJourUtilisateur(utilisateur: Utilisateur) -> Résultat<Utilisateur, ErreurUtilisateur> {
        do {
            try self.entrepôt.mettreAJour(utilisateur: utilisateur)
            return .succès(utilisateur)
        } catch let erreur as ErreurUtilisateur {
            return .échec(erreur)
        } catch {
            return .échec(ErreurUtilisateur.erreurInconnue)
        }
    }

    func supprimerUtilisateur(parId id: UUID) -> Résultat<Void, ErreurUtilisateur> {
        do {
            try self.entrepôt.supprimer(parId: id)
            return .succès(())
        } catch let erreur as ErreurUtilisateur {
            return .échec(erreur)
        } catch {
            return .échec(ErreurUtilisateur.erreurInconnue)
        }
    }
}
```

**EntrepôtUtilisateur**

```swift
protocol EntrepôtUtilisateur {
    func créer(utilisateur: Utilisateur) throws
    func récupérer(parId id: UUID) throws -> Utilisateur?
    func récupérerTous() throws -> [Utilisateur]
    func mettreAJour(utilisateur: Utilisateur) throws
    func supprimer(parId id: UUID) throws
}
```

**Implementation d'EntrepôtUtilisateur avec CoreData**

```swift
class EntrepôtUtilisateurCoreData: EntrepôtUtilisateur {
    private let contexte: NSManagedObjectContext

    init(contexte: NSManagedObjectContext) {
        self.contexte = contexte
    }

    func créer(utilisateur: Utilisateur) throws {
        let entitéUtilisateur = NSEntityDescription.entity(forEntityName: "Utilisateur", in: self.contexte)!
        let nouvelUtilisateur = NSManagedObject(entity: entitéUtilisateur, insertInto: self.contexte)
        nouvelUtilisateur.setValue(utilisateur.id, forKey: "id")
        nouvelUtilisateur.setValue(utilisateur.nom, forKey: "nom")
        nouvelUtilisateur.setValue(utilisateur.prenom, forKey: "prenom")
        nouvelUtilisateur.setValue(utilisateur.email, forKey: "email")
        nouvelUtilisateur.setValue(utilisateur.motDePasse, forKey: "motDePasse")
        nouvelUtilisateur.setValue(utilisateur.role.rawValue, forKey: "role")

        try self.contexte.save()
    }

    func récupérer(parId id: UUID) throws -> Utilisateur? {
        let demande = NSFetchRequest<NSManagedObject>(entityName: "Utilisateur")
        demande.predicate = NSPredicate(format: "id == %@", id as CVarArg)

        let résultats = try self.contexte.fetch(demande)
        if let utilisateurObjet = résultats.first {
            return Utilisateur(id: utilisateurObjet.value(forKey: "id") as! UUID,
                               nom: utilisateurObjet.value(forKey: "nom") as! String,
                               prenom: utilisateurObjet.value(forKey: "prenom") as! String,
                               email: utilisateurObjet.value(forKey: "email") as! String,
                               motDePasse: utilisateurObjet.value(forKey: "motDePasse") as! String,
                               role: Role(rawValue: utilisateurObjet.value(forKey: "role") as! String)!)
        }

        return nil
    }

    func récupérerTous() throws -> [Utilisateur] {
        let demande = NSFetchRequest<NSManagedObject>(entityName: "Utilisateur")

        let résultats = try self.contexte.fetch(demande)
        return résultats.map {
            Utilisateur(id: $0.value(forKey: "id") as! UUID,
                       nom: $0.value(forKey: "nom") as! String,
                       prenom: $0.value(forKey: "prenom") as! String,
                       email: $0.value(forKey: "email") as! String,
                       motDePasse: $0.value(forKey: "motDePasse") as! String,
                       role: Role(rawValue: $0.value(forKey: "role") as! String)!)
        }
    }

    func mettreAJour(utilisateur: Utilisateur) throws {
        let demande = NSFetchRequest<NSManagedObject>(entityName: "Utilisateur")
        demande.predicate = NSPredicate(format: "id == %@", utilisateur.id as CVarArg)

        let résultats = try self.contexte.fetch(demande)
        if let utilisateurObjet = résultats.first {
            utilisateurObjet.setValue(utilisateur.nom, forKey: "nom")
            utilisateurObjet.setValue(utilisateur.prenom, forKey: "prenom")
            utilisateurObjet.setValue(utilisateur.email, forKey: "email")
            utilisateurObjet.setValue(utilisateur.motDePasse, forKey: "motDePasse")
            utilisateurObjet.setValue(utilisateur.role.rawValue, forKey: "role")

            try self.contexte.save()
        }
    }

    func supprimer(parId id: UUID) throws {
        let demande = NSFetchRequest<NSManagedObject>(entityName: "Utilisateur")
        demande.predicate = NSPredicate(format: "id == %@", id as CVarArg)

        let résultats = try self.contexte.fetch(demande)
        if let utilisateurObjet = résultats.first {
            self.contexte.delete(utilisateurObjet)

            try self.contexte.save()
        }
    }
}
```

**Exemple d'utilisation**

```swift
// Créer un gestionnaire d'utilisateur
let gestionnaireUtilisateur = GestionnaireUtilisateurImpl(entrepôt: EntrepôtUtilisateurCoreData(contexte: persistentContainer.viewContext))

// Créer un nouvel utilisateur
let nouvelUtilisateur = Utilisateur(id: UUID(), nom: "Dupont", prenom: "Jean", email: "jean.dupont@exemple.com", motDePasse: "secret", role: .utilisateur)

// Ajouter l'utilisateur à la base de données
let résultatCréation = gestionnaireUtilisateur.créerUtilisateur(utilisateur: nouvelUtilisateur)

// Récupérer un utilisateur par son ID
let résultatRécupération = gestionnaireUtilisateur