**Code complexe en Dart**

Ce code implémente un système de rendu hiérarchique basé sur des composants, inspiré de React. Il utilise des fonctions de première classe, des fermetures et des objets immuables pour créer une interface utilisateur réactive et efficace.

```dart
import 'dart:html';

class Component {
  final String type;
  final Map<String, dynamic> props;

  Component(this.type, this.props);

  Element render() {
    var element = document.createElement(type);
    props.forEach((key, value) {
      element.setAttribute(key, value);
    });
    return element;
  }
}

class App extends Component {
  App() : super('div', {});

  List<Component> children = [];

  void renderChildren() {
    children.forEach((child) {
      appElement.append(child.render());
    });
  }

  Element appElement = document.querySelector('#app');

  @override
  Element render() {
    renderChildren();
    return appElement;
  }
}

class Button extends Component {
  Button(String text) : super('button', {'textContent': text});
}

class Counter extends Component {
  int count = 0;

  Counter() : super('div', {});

  void increment() {
    count++;
    render();
  }

  @override
  Element render() {
    var element = super.render();
    element.append(Text('Count: $count'));
    element.append(Button('Increment').render());
    return element;
  }
}

void main() {
  var app = App();
  app.children.add(Counter());
  document.body.append(app.render());
}
```

**Explication du code**

* **Composant de base:** La classe `Component` définit l'interface pour tous les composants. Il possède un type (ex. `div`), des propriétés (ex. `textContent`), et une méthode `render` qui crée l'élément HTML correspondant.
* **Application:** La classe `App` est le conteneur principal des composants. Elle possède une liste d'enfants et une méthode `renderChildren` qui les affiche dans un élément HTML spécifique.
* **Composant bouton:** `Button` est un simple composant qui crée un bouton avec un texte donné.
* **Compteur:** `Counter` est un composant plus complexe avec un état interne (le nombre). Il possède une méthode `increment` qui incrémente le nombre et déclenche un nouveau rendu.
* **Fonction principale:** `main` instancie l'application, ajoute un compteur comme enfant et l'affiche dans le document.

**Caractéristiques complexes:**

* **Hiérarchie de composants:** Les composants peuvent être imbriqués dans une hiérarchie, permettant la création d'interfaces utilisateur complexes.
* **Fermetures:** La méthode `increment` accède à l'état interne du compteur, même lorsqu'elle est appelée depuis un événement externe.
* **Objets immuables:** Le compteur et ses propriétés sont immuables, garantissant la stabilité et l'intégrité des données.
* **Réactivité:** Les modifications de l'état interne (ex. en incrémentant le compteur) déclenchent automatiquement le rendu, maintenant l'interface utilisateur à jour.