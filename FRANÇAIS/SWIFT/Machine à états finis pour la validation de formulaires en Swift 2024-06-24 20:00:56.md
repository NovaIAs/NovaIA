Voici un code SWIFT complexe et détaillé qui implémente une machine à états finis pour la vérification de formulaires :

```swift
enum FormState {
    case initial
    case nomSaisi
    case prenomSaisi
    case emailSaisi
    case motDePasseSaisi
    case confirmationMotDePasseSaisi
    case formulaireValide
    case erreur
}

class FormValidator {

    private var etatActuel: FormState = .initial

    func validerNom(nom: String) {
        guard !nom.isEmpty else {
            etatActuel = .erreur
            return
        }
        etatActuel = .nomSaisi
    }

    func validerPrenom(prenom: String) {
        guard !prenom.isEmpty else {
            etatActuel = .erreur
            return
        }
        etatActuel = .prenomSaisi
    }

    func validerEmail(email: String) {
        let emailRegex = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}"
        let emailPredicate = NSPredicate(format: "SELF MATCHES %@", emailRegex)
        guard emailPredicate.evaluate(with: email) else {
            etatActuel = .erreur
            return
        }
        etatActuel = .emailSaisi
    }

    func validerMotDePasse(motDePasse: String) {
        guard motDePasse.count >= 8 else {
            etatActuel = .erreur
            return
        }
        etatActuel = .motDePasseSaisi
    }

    func validerConfirmationMotDePasse(confirmationMotDePasse: String) {
        guard confirmationMotDePasse == etatActuel.motDePasseSaisi else {
            etatActuel = .erreur
            return
        }
        etatActuel = .confirmationMotDePasseSaisi
    }

    func validerFormulaire() {
        guard etatActuel == .confirmationMotDePasseSaisi else {
            etatActuel = .erreur
            return
        }
        etatActuel = .formulaireValide
    }

    func obtenirEtatFormulaire() -> FormState {
        return etatActuel
    }
}
```

**Explication du code :**

* **Énumération FormState :** Définit les différents états possibles du formulaire.
* **Classe FormValidator :** Représente la machine à états finis.
* **Variable etatActuel :** Stocke l'état actuel du formulaire.
* **Méthodes de validation :** Valident les entrées de chaque champ du formulaire et mettent à jour l'état actuel en conséquence.
* **Méthode validerFormulaire :** Valide le formulaire dans son ensemble et met à jour l'état actuel.
* **Méthode obtenirEtatFormulaire :** Retourne l'état actuel du formulaire.

Ce code utilise une approche orientée objet pour implémenter une machine à états finis. Chaque état du formulaire est représenté par un élément de l'énumération FormState. La classe FormValidator implémente les transitions d'état en réponse aux entrées de l'utilisateur.