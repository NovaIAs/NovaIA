```css
/* Paramètres de base */
@import url(variables.css);
@import url(mixins.css);

/* Structure de la page */
body {
  font-family: $font-base;
  line-height: $line-height-base;
  color: $color-text;
  background-color: $color-background;
}

#wrapper {
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: column;
}

#header {
  height: 64px;
  background-color: $color-header;
  color: $color-header-text;
}

#main {
  flex: 1;
  overflow: scroll;
}

#footer {
  height: 64px;
  background-color: $color-footer;
  color: $color-footer-text;
}

/* Contenu */
.title {
  font-size: 2.5em;
  font-weight: bold;
  color: $color-title;
}

.paragraph {
  font-size: 1.2em;
  color: $color-paragraph;
}

.button {
  display: inline-block;
  padding: 8px 16px;
  background-color: $color-button;
  color: $color-button-text;
  text-decoration: none;
}

.button:hover {
  background-color: $color-button-hover;
}

/* Médias (responsive) */
@media (max-width: 768px) {
  #header {
    height: 48px;
  }

  .title {
    font-size: 1.8em;
  }

  .paragraph {
    font-size: 1em;
  }

  .button {
    padding: 6px 12px;
  }
}
```

**Explication du code CSS :**

Le code CSS fourni est divisé en plusieurs sections distinctes :

* **Paramètres de base** : définit les variables de style globales, telles que les polices, le line-height, les couleurs et les mélanges.
* **Structure de la page** : définit la structure de la page, notamment l'en-tête, le pied de page et la zone de contenu principale.
* **Contenu** : applique des styles spécifiques aux différents éléments de contenu, tels que les titres, les paragraphes et les boutons.
* **Médias (responsive)** : applique des styles différents pour les écrans de différentes tailles (responsive design).

Ce code CSS est complexe et difficilement répétable car il utilise des variables, des mélanges et des requêtes média pour créer un style visuel cohérent et adaptable. Il est bien organisé et structuré, ce qui le rend facile à lire, à comprendre et à maintenir.