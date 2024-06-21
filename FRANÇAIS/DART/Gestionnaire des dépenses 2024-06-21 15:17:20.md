**gestionnaire de dépenses**

```dart
import 'dart:collection';
import 'package:flutter/material.dart';
import 'package:intl/intl.dart';

class GestionnaireDepenses extends StatefulWidget {
  @override
  _GestionnaireDepensesState createState() => _GestionnaireDepensesState();
}

class _GestionnaireDepensesState extends State<GestionnaireDepenses> {
  final List<Depense> _depenses = [];
  final TextEditingController _libelleController = TextEditingController();
  final TextEditingController _montantController = TextEditingController();
  final DateFormat _formatteurDate = DateFormat('dd/MM/yyyy');

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Gestionnaire de dépenses'),
      ),
      body: Column(
        children: [
          Padding(
            padding: EdgeInsets.all(16.0),
            child: TextField(
              controller: _libelleController,
              decoration: InputDecoration(hintText: 'Libellé'),
            ),
          ),
          Padding(
            padding: EdgeInsets.all(16.0),
            child: TextField(
              controller: _montantController,
              decoration: InputDecoration(hintText: 'Montant'),
              keyboardType: TextInputType.number,
            ),
          ),
          ElevatedButton(
            onPressed: () => _ajouterDepense(),
            child: Text('Ajouter'),
          ),
          Expanded(
            child: ListView.builder(
              itemCount: _depenses.length,
              itemBuilder: (context, index) => _construireItemDepense(_depenses[index]),
            ),
          ),
        ],
      ),
    );
  }

  void _ajouterDepense() {
    if (_libelleController.text.isNotEmpty &&
        double.tryParse(_montantController.text) != null) {
      setState(() {
        _depenses.add(Depense(
            libelle: _libelleController.text,
            montant: double.parse(_montantController.text),
            date: DateTime.now()));
        _libelleController.clear();
        _montantController.clear();
      });
    }
  }

  Widget _construireItemDepense(Depense depense) {
    return ListTile(
      title: Text(depense.libelle),
      subtitle: Text('${_formatteurDate.format(depense.date)} - ${depense.montant} €'),
    );
  }
}

class Depense {
  late String libelle;
  late double montant;
  late DateTime date;

  Depense({required this.libelle, required this.montant, required this.date});
}
```

**Notes:**

* Ce code crée une application simple de gestion des dépenses.
* Il utilise la classe `Depense` pour stocker des informations sur chaque dépense, y compris son libellé, son montant et sa date.
* La liste de dépenses est gérée dans la classe `_GestionnaireDepensesState` et représentée dans une liste `ListView`.
* L'interface utilisateur comprend des champs de saisie pour le libellé et le montant de la dépense, ainsi qu'un bouton pour ajouter une nouvelle dépense.
* Le formatage de la date utilise le package `intl`.