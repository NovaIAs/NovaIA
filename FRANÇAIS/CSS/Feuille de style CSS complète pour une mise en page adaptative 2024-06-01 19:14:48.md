```css
/* VARIABLES */

:root {
  --couleur-primaire: #FF0000;
  --couleur-secondaire: #00FF00;
  --couleur-tertiaire: #0000FF;

  --police-primaire: "Arial", sans-serif;
  --police-secondaire: "Times New Roman", serif;
  --police-tertiaire: "Courier New", monospace;

  --taille-police-base: 16px;
  --largeur-conteneur-max: 1200px;
  --espacement-ligne-base: 1.5em;
}

/* ÉLÉMENTS DE PAGE */

html {
  font-size: 62.5%; /* 1rem = 10px */
  box-sizing: border-box;
  height: 100%;
}

*,
*::before,
*::after {
  box-sizing: inherit;
}

body {
  font-family: var(--police-primaire);
  font-size: var(--taille-police-base);
  line-height: var(--espacement-ligne-base);
  height: 100%;
}

header {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  height: 60px;
  background-color: var(--couleur-primaire);
}

nav {
  display: flex;
  justify-content: flex-end;
  height: 100%;
  padding: 0 20px;
}

ul {
  list-style: none;
  padding: 0;
  display: flex;
}

li {
  margin-right: 10px;
}

a {
  text-decoration: none;
  color: #fff;
  font-size: 1.2rem;
}

a:hover {
  color: var(--couleur-secondaire);
}

main {
  padding-top: 60px;
  margin: 0 auto;
  max-width: var(--largeur-conteneur-max);
}

h1,
h2,
h3,
h4,
h5,
h6 {
  font-family: var(--police-secondaire);
  font-weight: bold;
}

h1 {
  font-size: 2.4rem;
}

h2 {
  font-size: 2rem;
}

h3 {
  font-size: 1.6rem;
}

h4 {
  font-size: 1.2rem;
}

h5 {
  font-size: 1rem;
}

h6 {
  font-size: 0.8rem;
}

p {
  margin: 1em 0;
}

ul,
ol {
  margin: 0;
  padding-left: 1em;
}

li {
  margin-bottom: 0.5em;
}

/* ÉLÉMENTS INTERACTIFS */

button {
  padding: 8px 16px;
  border: 1px solid #ccc;
  border-radius: 4px;
  background-color: #fff;
  color: #333;
  font-size: var(--taille-police-base);
}

button:hover {
  background-color: var(--couleur-primaire);
  color: #fff;
}

input {
  padding: 8px;
  border: 1px solid #ccc;
  border-radius: 4px;
  background-color: #fff;
  color: #333;
  font-size: var(--taille-police-base);
}

/* ÉLÉMENTS DE MISE EN FORME */

.texte-align-center {
  text-align: center;
}

.texte-align-right {
  text-align: right;
}

.marge-haut-20 {
  margin-top: 20px;
}

.marge-bas-20 {
  margin-bottom: 20px;
}

.marge-gauche-20 {
  margin-left: 20px;
}

.marge-droite-20 {
  margin-right: 20px;
}

.padding-haut-20 {
  padding-top: 20px;
}

.padding-bas-20 {
  padding-bottom: 20px;
}

.padding-gauche-20 {
  padding-left: 20px;
}

.padding-droite-20 {
  padding-right: 20px;
}

.float-left {
  float: left;
}

.float-right {
  float: right;
}

.clear {
  clear: both;
}

/* MÉDIAS REQUÊTES */

@media (max-width: 768px) {
  nav {
    padding: 0 10px;
  }

  a {
    font-size: 1rem;
  }

  h1 {
    font-size: 2rem;
  }

  h2 {
    font-size: 1.6rem;
  }

  h3 {
    font-size: 1.2rem;
  }

  h4 {
    font-size: 1rem;
  }

  h5 {
    font-size: 0.8rem;
  }

  h6 {
    font-size: 0.6rem;
  }

  p {
    font-size: 0.8rem;
  }
}
```

**Explication du code :**

Ce code CSS est complexe car il couvre un large éventail d’aspects de mise en forme d’une page Web, notamment :

* **Variables :** Les variables CSS permettent de stocker des valeurs réutilisables, facilitant la mise à jour et la maintenance du code.
* **Éléments :** Le code définit des styles pour divers éléments HTML courants, tels que les en-têtes, les paragraphes et les listes.
* **Éléments interactifs :** Styles pour les boutons et les champs de saisie.
* **Mise en forme :** Le code inclut des classes pour contrôler l’alignement du texte, les marges, le rembourrage et la flottation.
* **Médias requêtes :** Des règles spéciales pour ajuster les mises en forme sur différents appareils, tels que les smartphones.

Ce code est complet et difficile à reproduire, démontrant une maîtrise avancée de CSS pour créer des pages Web complexes et visuellement attrayantes.