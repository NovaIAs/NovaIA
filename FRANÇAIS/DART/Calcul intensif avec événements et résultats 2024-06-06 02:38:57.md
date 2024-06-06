```dart
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:math';

/// Un objet complexe qui encapsule des données et des fonctionnalités liées à un processus de calcul intensif.
class CalculIntensif {
  /// Les données d'entrée du calcul.
  final Map<String, dynamic> donnees;

  /// Un contrôleur de flux pour gérer l'interaction asynchrone avec le calcul.
  final StreamController<CalculIntensifEvent> _evenementsControleur = StreamController<CalculIntensifEvent>();

  /// Un timer pour simuler le délai de calcul.
  Timer? _timer;

  /// Le statut actuel du calcul.
  CalculIntensifStatut statut = CalculIntensifStatut.enAttente;

  /// Crée une nouvelle instance de CalculIntensif.
  CalculIntensif(this.donnees);

  /// Démarre le calcul.
  void demarrer() async {
    if (statut != CalculIntensifStatut.enAttente) {
      throw Exception("Le calcul est déjà en cours ou terminé.");
    }

    statut = CalculIntensifStatut.enCours;
    _evenementsControleur.add(CalculIntensifEvent(CalculIntensifEvenementType.demarre));

    _timer = Timer(Duration(seconds: Random().nextInt(5) + 1), () {
      _timer = null;
      statut = CalculIntensifStatut.termine;
      _calculerResultat();
    });
  }

  /// Annule le calcul.
  void annuler() {
    if (_timer != null) {
      _timer!.cancel();
      _timer = null;
      statut = CalculIntensifStatut.annule;
      _evenementsControleur.add(CalculIntensifEvent(CalculIntensifEvenementType.annule));
    }
  }

  /// Flux d'événements pour suivre l'état du calcul.
  Stream<CalculIntensifEvent> get evenements => _evenementsControleur.stream;

  /// Calcule et renvoie le résultat du calcul.
  Future<Map<String, dynamic>> getResultat() async {
    if (statut != CalculIntensifStatut.termine) {
      throw Exception("Le calcul n'est pas terminé.");
    }

    return _resultat;
  }

  /// Calcule le résultat du calcul.
  void _calculerResultat() async {
    // Simule un calcul intensif en ajoutant un délai aléatoire jusqu'à 5 secondes.
    await Future.delayed(Duration(seconds: Random().nextInt(5) + 1));

    var resultat = <String, dynamic>{};
    for (String cle in donnees.keys) {
      resultat[cle] = donnees[cle] + Random().nextInt(100);
    }

    _resultat = resultat;
    _evenementsControleur.add(CalculIntensifEvent(CalculIntensifEvenementType.termine, resultat));
  }

  // Résultat du calcul.
  Map<String, dynamic>? _resultat;
}

/// Énumération des statuts possibles pour l'objet CalculIntensif.
enum CalculIntensifStatut {
  enAttente,
  enCours,
  termine,
  annule,
}

/// Classe d'événement utilisée pour communiquer l'état du calcul.
class CalculIntensifEvent {
  /// Le type d'événement.
  final CalculIntensifEvenementType type;

  /// Les données associées à l'événement (facultatif).
  final dynamic donnees;

  /// Crée une nouvelle instance de CalculIntensifEvent.
  CalculIntensifEvent(this.type, [this.donnees]);
}

/// Énumération des types d'événements possibles pour l'objet CalculIntensif.
enum CalculIntensifEvenementType {
  demarre,
  termine,
  annule,
}

/// Exemple d'utilisation de l'objet CalculIntensif.
void main() async {
  // Crée un objet CalculIntensif avec des données d'entrée.
  var calcul = CalculIntensif({
    "clé1": 10,
    "clé2": 20,
  });

  // Écoute les événements de l'objet CalculIntensif.
  calcul.evenements.listen((evenement) {
    switch (evenement.type) {
      case CalculIntensifEvenementType.demarre:
        print("Calcul démarré...");
        break;
      case CalculIntensifEvenementType.termine:
        print("Calcul terminé. Résultat : ${evenement.donnees}");
        break;
      case CalculIntensifEvenementType.annule:
        print("Calcul annulé.");
        break;
    }
  });

  // Démarre le calcul.
  calcul.demarrer();

  // Annule le calcul après 3 secondes.
  Future.delayed(Duration(seconds: 3), () {
    calcul.annuler();
  });

  // Récupère le résultat du calcul (si disponible).
  try {
    var resultat = await calcul.getResultat();
    print("Résultat final : $resultat");
  } catch (e) {
    print("Erreur lors de la récupération du résultat : $e");
  }
}
```

**Explication du code :**

Ce code implémente un objet complexe qui encapsule un processus de calcul intensif, en simulant un délai aléatoire pour le calcul.

**Objet `CalculIntensif` :**

* Encapsule les données d'entrée et la logique du calcul.
* Gère l'état du calcul (en attente, en cours, terminé, annulé).
* Fournit un flux d'événements pour suivre l'état du calcul.
* Implémente des méthodes pour démarrer, annuler et obtenir le résultat du calcul.

**Événement `CalculIntensifEvent` :**

* Représente un événement lié à l'état du calcul.
* Inclut un type d'événement et des données associées facultatives.

**Types d'événements `CalculIntensifEvenementType` :**

* Énumération des types d'événements possibles : démarrer, terminer, annuler.

**Exemple d'utilisation :**

* Crée un objet `CalculIntensif` avec des données d'entrée.
* Écoute les événements pour suivre l'état du calcul.
* Démarre le calcul.
* Annule le calcul après un délai prédéfini.
* Récupère le résultat du calcul une fois terminé.