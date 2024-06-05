```typescript
// Gestion des États d'un composant React
interface ÉtatComposant {
  compteur: nombre;
  texteSaisi: texte;
}

// Composant React principal
class ComposantPrincipal extends React.Component<{}, ÉtatComposant> {
  constructor(props) {
    super(props);

    this.état = {
      compteur: 0,
      texteSaisi: '',
    };
  }

  handleIncrementationCompteur = () => {
    this.setState((étatPrécédent) => ({
      ...étatPrécédent,
      compteur: étatPrécédent.compteur + 1,
    }));
  };

  handleSaisieTexte = (événement) => {
    this.setState({ texteSaisi: événement.cible.value });
  };

  render() {
    return (
      <div>
        {/* Affichage du compteur */}
        <p>Compteur : {this.état.compteur}</p>

        {/* Bouton d'incrémentation du compteur */}
        <button onClick={this.handleIncrementationCompteur}>Incrémenter le compteur</button>

        {/* Champ de saisie de texte */}
        <input type="text" value={this.état.texteSaisi} onChange={this.handleSaisieTexte} />

        {/* Affichage du texte saisi */}
        <p>Texte saisi : {this.état.texteSaisi}</p>
      </div>
    );
  }
}
```

**Explication du code :**

Ce code TypeScript crée un composant React appelé `ComposantPrincipal` qui gère deux états :

* `compteur` : Un entier qui représente un compteur.
* `texteSaisi` : Une chaîne qui représente le texte saisi par l'utilisateur.

Le composant définit deux méthodes de gestion des événements :

* `handleIncrementationCompteur` : Incrémente le compteur d'un lors du clic sur un bouton.
* `handleSaisieTexte` : Met à jour l'état `texteSaisi` avec la valeur saisie par l'utilisateur dans un champ de saisie.

Dans la méthode de rendu, le composant affiche le compteur, un bouton d'incrémentation et un champ de saisie de texte. Les événements `onClick` et `onChange` sont liés aux méthodes de gestion des événements respectives.