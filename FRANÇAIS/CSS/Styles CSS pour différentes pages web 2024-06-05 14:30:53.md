```css
/* Styles de base */

body {
  background-color: #F5F5F5;
  font-family: Arial, sans-serif;
}

h1 {
  color: #333;
  font-size: 2rem;
  text-align: center;
}

p {
  color: #666;
  font-size: 1.2rem;
}

/* Styles spécifiques de la page d'accueil */

.accueil {
  padding: 2rem;
}

.accueil__titre {
  font-size: 3rem;
  margin-bottom: 1rem;
}

.accueil__paragraphe {
  margin-bottom: 1.5rem;
}

/* Styles spécifiques de la page produits */

.produits {
  display: grid;
  grid-template-columns: repeat(3, auto);
  gap: 1rem;
}

.produit {
  background-color: #FFF;
  padding: 1rem;
  text-align: center;
}

.produit__image {
  width: 100px;
  height: 100px;
}

.produit__nom {
  font-size: 1.5rem;
  margin-bottom: 0.5rem;
}

.produit__prix {
  color: #666;
}

/* Styles spécifiques de la page contact */

.contact {
  padding: 2rem;
}

.contact__formulaire {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.contact__label {
  font-size: 1.2rem;
}

.contact__input {
  width: 100%;
  padding: 0.5rem;
}

.contact__bouton {
  background-color: #333;
  color: #FFF;
  padding: 0.5rem 1rem;
  cursor: pointer;
}

/* Styles spécifiques de la page panier */

.panier {
  padding: 2rem;
}

.panier__tableau {
  width: 100%;
  border-collapse: collapse;
}

.panier__th {
  background-color: #333;
  color: #FFF;
  padding: 0.5rem;
}

.panier__td {
  padding: 0.5rem;
}

/* Styles spécifiques de la page de paiement */

.paiement {
  padding: 2rem;
}

.paiement__formulaire {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.paiement__label {
  font-size: 1.2rem;
}

.paiement__input {
  width: 100%;
  padding: 0.5rem;
}

.paiement__bouton {
  background-color: #333;
  color: #FFF;
  padding: 0.5rem 1rem;
  cursor: pointer;
}
```

**Explication du code**

Ce code CSS est un exemple complexe de feuille de style qui couvre différents types de pages d'un site web, notamment la page d'accueil, la page produits, la page contact, la page panier et la page de paiement.

Le code utilise les principes de base du CSS, tels que les sélecteurs, les propriétés et les valeurs, pour définir l'apparence des différents éléments HTML sur chaque page.

**Sélecteurs**

* `body`: sélectionne l'élément `<body>` qui contient tout le contenu de la page.
* `.accueil`: sélectionne l'élément qui a la classe "accueil".
* `.produits`: sélectionne l'élément qui a la classe "produits".
* `.produit`: sélectionne tous les éléments qui ont la classe "produit".
* `.contact`: sélectionne l'élément qui a la classe "contact".
* `.panier`: sélectionne l'élément qui a la classe "panier".
* `.paiement`: sélectionne l'élément qui a la classe "paiement".

**Propriétés**

* `background-color`: définit la couleur d'arrière-plan d'un élément.
* `font-family`: définit la police de caractères utilisée pour le texte d'un élément.
* `font-size`: définit la taille de la police utilisée pour le texte d'un élément.
* `text-align`: définit l'alignement du texte dans un élément.
* `display`: définit la façon dont un élément est affiché.
* `grid-template-columns`: définit le nombre de colonnes et leur largeur dans une grille CSS.
* `gap`: définit l'espacement entre les éléments dans une grille ou un flexbox.
* `padding`: définit l'espacement entre le contenu d'un élément et ses bordures.

**Valeurs**

* `#F5F5F5`: un code couleur hexadécimal pour le gris clair.
* `#333`: un code couleur hexadécimal pour le gris foncé.
* `1.2rem`: une unité de longueur relative à la taille de police racine.
* `repeat(3, auto)`: crée trois colonnes de largeur automatique dans une grille CSS.
* `1rem`: une unité de longueur égale à la taille de la police racine.
* `100px`: une unité de longueur absolue égale à 100 pixels.