**Styles complexes en CSS**

```
/* Mixin pour appliquer des styles à plusieurs sélecteurs */
@mixin background-gradient($start-color, $end-color) {
  background: linear-gradient($start-color, $end-color);
  background-clip: text;
  -webkit-background-clip: text;
}

/* Sélecteur pour les titres de niveau 1 */
h1 {
  @include background-gradient(#000, #fff);
  font-family: "Montserrat", Arial, sans-serif;
  font-size: 4rem;
  text-transform: uppercase;
  text-align: center;
}

/* Sélecteur pour les titres de niveau 2 */
h2 {
  @include background-gradient(#fff, #000);
  font-family: "Helvetica", Arial, sans-serif;
  font-size: 2rem;
  text-transform: capitalize;
  text-align: left;
}

/* Sélecteur pour les paragraphes */
p {
  font-family: "Times New Roman", Georgia, serif;
  font-size: 1.2rem;
  line-height: 1.5em;
  text-indent: 1em;
  text-align: justify;
}

/* Sélecteur pour les liens */
a {
  color: #00f;
  text-decoration: none;
  @include background-gradient(transparent, #00f);
}

/* Sélecteur pour les images */
img {
  display: block;
  margin: 0 auto;
  width: 100%;
  height: auto;
}

/* Sélecteur pour les listes non ordonnées */
ul {
  list-style-type: none;
  padding: 0;
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
}

/* Sélecteur pour les éléments de liste */
li {
  width: 25%;
  margin: 0 1%;
}

/* Sélecteur pour les formulaires */
form {
  width: 100%;
  margin: 0 auto;
  padding: 2rem;
  background-color: #f0f0f0;
}

/* Sélecteur pour les champs de saisie */
input,
textarea {
  width: 100%;
  padding: 0.5rem;
  margin-bottom: 1rem;
  border: 1px solid #ccc;
}

/* Sélecteur pour les boutons */
button {
  padding: 0.5rem 1rem;
  background-color: #000;
  color: #fff;
  border: none;
  cursor: pointer;
}

/* Sélecteur pour le pied de page */
footer {
  background-color: #000;
  color: #fff;
  padding: 1rem;
  text-align: center;
}
```

**Explication**

Ce code CSS fournit des styles complexes et différenciés pour une variété d'éléments HTML. Il utilise des mixins, des sélecteurs, des propriétés de boîte, des dégradés de fond, des polices de caractères et d'autres techniques pour créer des mises en page et des effets visuels sophistiqués.

* **Mixins** : Les mixins permettent de réutiliser des blocs de code CSS, ce qui facilite la maintenance et la cohérence. Dans ce code, le mixin `background-gradient` est utilisé pour appliquer des dégradés de fond à plusieurs éléments.
* **Sélecteurs** : Les sélecteurs ciblent des éléments HTML spécifiques et leur appliquent des styles. Ce code utilise des sélecteurs pour cibler des titres, des paragraphes, des liens, des images, des listes, des formulaires et des boutons.
* **Propriétés de boîte** : Les propriétés de boîte contrôlent la disposition et l'apparence des éléments. Ce code utilise des propriétés telles que `margin`, `padding` et `border` pour positionner et encadrer les éléments.
* **Dégradés de fond** : Les dégradés de fond créent des transitions fluides entre les couleurs. Dans ce code, les dégradés de fond sont utilisés sur les titres, les liens et les boutons pour ajouter de la profondeur et du dynamisme.
* **Polices de caractères** : Les polices de caractères contrôlent l'apparence du texte. Ce code utilise différentes polices de caractères pour les titres, les paragraphes et les listes afin de créer une hiérarchie visuelle.