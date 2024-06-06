**Code CSS complexe**

```css
/* Paramètres de grille complexe avec plusieurs colonnes et lignes */
.grille {
  display: grid;
  grid-template-columns: repeat(3, 1fr) 180px;
  grid-template-rows: repeat(4, auto) 100px;
  grid-gap: 20px;
  grid-column-gap: 10px;
  grid-row-gap: 5px;
}

/* Style d'élément basé sur l'index de ligne et de colonne dans la grille */
.grille > * {
  background-color: hsl(${(grid-column - 1) * 360 / 3}, 100%, 50%);
  height: 100%;
}

/* Styles de section du haut */
.grille > :nth-child(n + 5) {
  grid-column: 1 / span 3;
  grid-row: 1 / span 2;
  background-color: hsla(0, 100%, 50%, 0.75);
}

/* Style de section du bas à gauche */
.grille > :nth-child(11) {
  background-color: hsla(240, 100%, 50%, 0.75);
}

/* Style de section du bas à droite */
.grille > :nth-child(12) {
  grid-column: 3 / span 2;
  grid-row: 5;
  background-color: hsla(120, 100%, 50%, 0.8);
}

/* Style de section de droite */
.grille > :nth-child(n + 8) {
  grid-column: 4;
  background-color: hsla(0, 100%, 50%, 0.6);
  text-align: center;
  font-size: 1.5rem;
  line-height: 1.5;
}

/* Sélecteur d'état pour l'élément survolé */
.grille > :hover {
  background-color: hsla(0, 100%, 25%, 0.5);
}

/* Sélecteur d'état pour l'élément masqué */
.grille > :nth-child(6) {
  display: none;
}

/* Sélecteur d'état pour l'élément actif */
.grille > .actif {
  border: 5px solid hsl(180, 100%, 30%);
}
```

**Explication du code**

* **Grille complexe :** Ce code crée une grille complexe à l'aide de la propriété `grid` de CSS. Il définit 3 colonnes et 4 lignes principales, ainsi qu'une colonne supplémentaire pour les éléments latéraux.
* **Style basé sur l'index :** Les éléments enfants de la grille sont stylisés en fonction de leur index de ligne et de colonne à l'aide des sélecteurs `:nth-child(...)`. Cela permet de créer des effets visuels complexes.
* **Sections spécifiques :** Le code cible des sections spécifiques de la grille (haut, bas à gauche, bas à droite, droite) pour appliquer des styles uniques.
* **État hover :** Un sélecteur d'état `:hover` met en évidence l'élément survolé avec une couleur de fond translucide.
* **Masquage d'élément :** Le sélecteur `:nth-child(6)` masque le sixième élément enfant de la grille.
* **État actif :** Un sélecteur d'état `.actif` ajoute une bordure autour de l'élément actif.