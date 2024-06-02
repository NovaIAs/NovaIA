**Code CSS complexe et différencié**

```css
/* Styles généraux */

* {
  box-sizing: border-box;
  margin: 0;
  padding: 0;
}

body {
  font-family: Arial, sans-serif;
  line-height: 1.6;
  color: #333;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
}

p {
  text-align: justify;
}

a {
  text-decoration: none;
  color: #000;
}

a:hover {
  color: #f00;
}

/* Styles pour le header */

header {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100px;
  background-color: #fff;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.logo {
  float: left;
  margin: 20px;
}

.logo img {
  width: 150px;
}

.menu {
  float: right;
  margin: 20px;
}

.menu ul {
  display: flex;
  list-style-type: none;
}

.menu li {
  margin-right: 20px;
}

.menu li a {
  font-size: 16px;
}

/* Styles pour le contenu principal */

.main {
  margin-top: 100px;
}

.container {
  max-width: 960px;
  margin: 0 auto;
}

.articles {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 20px;
}

.article {
  background-color: #f5f5f5;
  padding: 20px;
  border-radius: 5px;
}

.article-title {
  font-size: 24px;
  font-weight: bold;
}

.article-content {
  text-align: justify;
}

/* Styles pour le footer */

footer {
  position: absolute;
  bottom: 0;
  left: 0;
  width: 100%;
  height: 100px;
  background-color: #f5f5f5;
}

.copyright {
  text-align: center;
  font-size: 12px;
}
```

**Explication du code**

Ce code CSS implémente un modèle de page Web réactif avec un header fixe, un contenu principal contenant une grille d'articles et un footer. Il utilise un certain nombre de techniques CSS avancées, notamment :

* **Flexbox:** Pour disposer les éléments du menu dans une rangée horizontale.
* **Grid:** Pour créer une grille d'articles réactive.
* **Border-box:** Pour inclure le rembourrage et la bordure dans la largeur de l'élément.
* **Rem:** Pour définir les tailles de police réactives.
* **Positionnement absolu et fixe:** Pour positionner le header et le footer.
* **Transitions:** Pour animer le changement de couleur des liens au survol.