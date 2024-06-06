**Code CSS complexe et différencié**

```css
@import url("styles-variables.css");

/* Sélecteur universel */
* {
  box-sizing: border-box;
  margin: 0;
  padding: 0;
}

/* Sélecteur d'élément */
body {
  font-family: 'Helvetica', sans-serif;
  font-size: $font-size-base;
  background-color: $color-gris-clair;
}

/* Sélecteur de classe */
.container {
  max-width: $container-max-width;
  margin: 0 auto;
  padding: $container-padding;
}

/* Sélecteur d'identifiant */
#main-content {
  width: 100%;
  padding: $main-content-padding;
  background-color: $color-blanc;
}

/* Pseudo-classe */
a:hover {
  color: $color-bleu-foncé;
  text-decoration: none;
}

/* Pseudo-élément */
::selection {
  background: $color-jaune-clair;
  color: $color-noir;
}

/* Règles combinées */
.container h1,
.container h2 {
  font-weight: bold;
  margin-bottom: $margin-bottom;
}

/* Règles imbriquées */
ul {
  list-style-type: none;
  padding-inline-start: 0;

  li {
    margin-bottom: $margin-bottom;
  }
}

/* Médias queries */
@media (min-width: $breakpoint-large) {
  .container {
    max-width: $container-max-width-large;
    padding: $container-padding-large;
  }
}

/* Animation */
@keyframes fade-in {
  from {
    opacity: 0;
  }

  to {
    opacity: 1;
  }
}

.fade-in {
  animation: fade-in 1s ease-in-out;
}

/* Dégradés */
.degradé {
  background: linear-gradient(45deg, $color-rouge-foncé, $color-orange-foncé);
}

.degradé-angulaire {
  background: conic-gradient($color-vert-foncé, $color-cyan-foncé);
}

/* Formes personnalisées */
.forme-cercle {
  width: 100px;
  height: 100px;
  border-radius: 50%;
  background-color: $color-bleu-clair;
}

.forme-carré {
  width: 100px;
  height: 100px;
  border: 1px solid $color-noir;
}

/* Positionnement */
.position-absolue {
  position: absolute;
  top: 0;
  right: 0;
}

.position-relative {
  position: relative;
  padding: $position-relative-padding;
}

/* Flexbox */
.flex-container {
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  align-items: center;
}

.flex-item {
  flex: 1;
  background-color: $color-gris-moyen;
}

/* Grilles CSS */
.grid-container {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  grid-gap: $grid-gap;
}

.grid-item {
  background-color: $color-violet-clair;
}
```

**Explication du code**

Ce code CSS complexe et différencié illustre une variété de fonctionnalités et de techniques avancées en CSS. Voici une brève explication des principales sections :

* **Variables CSS** : Le code importe un fichier CSS séparé contenant des variables CSS. Celles-ci permettent de définir des valeurs réutilisables qui peuvent être utilisées dans l'ensemble du code CSS.
* **Sélecteurs** : Divers types de sélecteurs sont utilisés, tels que les sélecteurs universels (*), d'éléments (body), de classes (.container), d'identifiants (#main-content), de pseudo-classes (a:hover) et de pseudo-éléments (::selection).
* **Règles combinées** : Plusieurs sélecteurs peuvent être combinés pour cibler des éléments spécifiques, comme .container h1, .container h2.
* **Règles imbriquées** : Des règles CSS peuvent être imbriquées pour organiser et structurer le code, comme dans ul li.
* **Médias queries** : Ce code contient des règles @media qui définissent des styles spécifiques pour différentes tailles d'écran.
* **Animation** : Une animation clé est définie pour l'effet de fondu (@keyframes fade-in).
* **Dégradés** : Des dégradés linéaires et angulaires sont créés à l'aide de la propriété background.
* **Formes personnalisées** : Des formes personnalisées (cercle et carré) sont créées à l'aide de border-radius et de bordures.
* **Positionnement** : Les propriétés position sont utilisées pour positionner les éléments dans des positions absolues et relatives.
* **Flexbox** : La disposition Flexbox est utilisée pour créer des mises en page flexibles.
* **Grilles CSS** : Les grilles CSS définissent une disposition basée sur la grille pour les éléments.

Ce code représente un exemple complexe de CSS utilisé pour créer des mises en page et des effets avancés et personnalisés.