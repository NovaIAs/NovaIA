**Classe principale pour gérer les données de l'application**

```dart
class ApplicationData {
  // Liste des utilisateurs inscrits dans l'application
  List<Utilisateur> utilisateurs = [];

  // Liste des messages échangés dans l'application
  List<Message> messages = [];

  // Ajouter un utilisateur à la liste
  void ajouterUtilisateur(Utilisateur utilisateur) {
    utilisateurs.add(utilisateur);
  }

  // Ajouter un message à la liste
  void ajouterMessage(Message message) {
    messages.add(message);
  }

  // Renvoie la liste des utilisateurs
  List<Utilisateur> obtenirUtilisateurs() {
    return utilisateurs;
  }

  // Renvoie la liste des messages
  List<Message> obtenirMessages() {
    return messages;
  }
}
```

**Classe Utilisateur pour représenter les informations d'un utilisateur**

```dart
class Utilisateur {
  // Nom d'utilisateur
  String nomUtilisateur;

  // Mot de passe
  String motDePasse;

  // Adresse e-mail
  String email;

  // Constructeur avec paramètres
  Utilisateur(this.nomUtilisateur, this.motDePasse, this.email);
}
```

**Classe Message pour représenter un message échangé**

```dart
class Message {
  // Expéditeur du message
  Utilisateur expediteur;

  // Destinataire du message
  Utilisateur destinataire;

  // Contenu du message
  String contenu;

  // Constructeur avec paramètres
  Message(this.expediteur, this.destinataire, this.contenu);
}
```

**Fonction principale pour gérer les interactions de l'utilisateur**

```dart
void main() {
  // Créer une instance de la classe ApplicationData pour gérer les données
  ApplicationData data = ApplicationData();

  // Créer des utilisateurs de test
  data.ajouterUtilisateur(Utilisateur("antoine", "azerty", "antoine@exemple.com"));
  data.ajouterUtilisateur(Utilisateur("marie", "qwerty", "marie@exemple.com"));

  // Créer des messages de test
  data.ajouterMessage(Message(data.utilisateurs[0], data.utilisateurs[1], "Salut Marie !"));
  data.ajouterMessage(Message(data.utilisateurs[1], data.utilisateurs[0], "Bonjour Antoine !"));

  // Afficher les données de l'application
  print("Utilisateurs :");
  for (Utilisateur utilisateur in data.obtenirUtilisateurs()) {
    print("\t- ${utilisateur.nomUtilisateur} (${utilisateur.email})");
  }

  print("Messages :");
  for (Message message in data.obtenirMessages()) {
    print("\t- De : ${message.expediteur.nomUtilisateur} à : ${message.destinataire.nomUtilisateur}");
    print("\t  - Contenu : ${message.contenu}");
  }
}
```

**Explication du code :**

* Le code crée des instances de classes pour représenter les données de l'application.
* La classe `ApplicationData` gère la liste des utilisateurs et des messages.
* Les classes `Utilisateur` et `Message` représentent les informations spécifiques à chaque utilisateur et message.
* La fonction `main` crée et initialise les données de l'application.
* Elle affiche ensuite les données dans la console pour montrer comment le code fonctionne.