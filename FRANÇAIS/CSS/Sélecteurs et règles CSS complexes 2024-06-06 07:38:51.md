```css
/* Sélecteurs combinés complexes */

/* Sélectionner tous les éléments avec la classe "conteneur" qui sont suivis d'un élément avec la classe "section" */
.conteneur + .section {
  background-color: #f0f8ff;
}

/* Sélectionner tous les éléments avec la classe "paragraphe" qui sont précédés d'un élément avec la classe "titre" */
.titre ~ .paragraphe {
  color: #000;
}

/* Sélectionner tous les éléments avec la classe "élément" qui sont descendants d'un élément avec la classe "parent" et qui ont un frère précédé d'un élément avec la classe "frère" */
.parent .élément + .frère ~ .élément {
  border: 1px solid #ccc;
}

/* Pseudo-classes complexes */

/* Sélectionner tous les éléments avec la classe "bouton" qui sont en focus */
.bouton:focus {
  outline: none;
  box-shadow: 0 0 5px #000;
}

/* Sélectionner tous les éléments avec la classe "image" qui ne sont pas chargés */
.image:not([loaded]) {
  display: none;
}

/* Sélectionner tous les éléments avec la classe "conteneur" qui contiennent un élément avec la classe "actif" */
.conteneur:has(.actif) {
  background-color: #00ff00;
}

/* Sélecteurs d'attributs complexes */

/* Sélectionner tous les éléments avec l'attribut "data-role" égal à "menu" */
[data-role="menu"] {
  list-style-type: none;
}

/* Sélectionner tous les éléments avec l'attribut "id" commençant par "section-" */
[id^="section-"] {
  margin-bottom: 20px;
}

/* Sélectionner tous les éléments avec l'attribut "class" contenant "exemple" */
[class~="exemple"] {
  color: #f00;
}

/* Sélecteurs de pseudo-éléments complexes */

/* Sélectionner le premier caractère dans tous les éléments avec la classe "titre" */
.titre::first-letter {
  font-size: 2em;
}

/* Sélectionner le dernier élément enfant dans tous les éléments avec la classe "conteneur" */
.conteneur::last-child {
  border-bottom: 1px solid #000;
}

/* Sélectionner le deuxième élément enfant dans tous les éléments avec la classe "liste" */
.liste::nth-child(2) {
  background-color: #eee;
}

/* Sélecteurs de pseudo-classes complexes */

/* Sélectionner tous les éléments avec la classe "bouton" qui sont actifs ou survolés */
.bouton:active,
.bouton:hover {
  cursor: pointer;
}

/* Sélectionner tous les éléments avec la classe "conteneur" qui ont une hauteur supérieure à 200px ou une largeur inférieure à 300px */
.conteneur:is(:is(:lang(fr)), :is(:lang(en)), :is(:not(lang))) {
  background-color: #ff0000;
}

/* Règles imbriquées complexes */

.conteneur {
  background-color: #fff;
  padding: 20px;

  .titre {
    font-size: 2em;
    margin-bottom: 20px;
  }

  .paragraphe {
    margin-bottom: 20px;
  }

  .bouton {
    display: inline-block;
    padding: 5px 10px;
    background-color: #000;
    color: #fff;
  }
}

/* Explication */

Ce code CSS est un exemple de code complexe et différencié qui utilise diverses techniques avancées. Voici quelques explications sur les techniques utilisées :

* **Sélecteurs combinés complexes :** Ces sélecteurs combinent plusieurs sélecteurs simples pour sélectionner des éléments qui répondent à des critères spécifiques. Cela permet de créer des règles CSS très précises et ciblées.
* **Pseudo-classes complexes :** Les pseudo-classes sélectionnent des éléments en fonction de leur état ou de leurs propriétés. Les pseudo-classes complexes peuvent être utilisées pour créer des règles CSS très spécifiques et dynamiques.
* **Sélecteurs d'attributs complexes :** Ces sélecteurs sélectionnent des éléments en fonction des valeurs de leurs attributs. Ils sont utiles pour sélectionner des éléments avec des attributs spécifiques ou des valeurs d'attribut.
* **Sélecteurs de pseudo-éléments complexes :** Ces sélecteurs sélectionnent des parties spécifiques d'éléments, telles que le premier caractère ou le dernier élément enfant. Ils peuvent être utilisés pour styliser des éléments de manière très précise.
* **Sélecteurs de pseudo-classes complexes :** Ces sélecteurs sélectionnent des éléments en fonction de plusieurs conditions. Ils peuvent être utilisés pour créer des règles CSS très spécifiques et conditionnelles.
* **Règles imbriquées complexes :** Les règles imbriquées permettent de créer des règles CSS imbriquées pour styliser des éléments descendants. Cela permet d'organiser le code CSS et de créer des règles CSS plus spécifiques et ciblées.