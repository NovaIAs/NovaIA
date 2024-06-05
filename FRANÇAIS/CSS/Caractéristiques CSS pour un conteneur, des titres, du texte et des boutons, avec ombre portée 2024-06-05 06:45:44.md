**Code CSS :**

```css
/* Mixin pour les ombres portées */
@mixin ombre-portee($couleur, $decalle-x, $decalle-y, $flou, $etalement) {
    box-shadow: $decalle-x $decalle-y $flou $etalement $couleur;
}

/* Sélecteur pour les éléments avec la classe "conteneur" */
.conteneur {
    /* Couleur d'arrière-plan */
    background-color: #f0f8ff;

    /* Rembourrage */
    padding: 20px;

    /* Marge */
    margin: 10px auto;

    /* Largeur maximale */
    max-width: 800px;

    /* Bordure */
    border: 1px solid #ccc;

    /* Appel du mixin pour l'ombre portée */
    @include ombre-portee(#000, 0, 5px, 10px, 2px);
}

/* Sélecteur pour les éléments avec la classe "titre" */
.titre {
    /* Couleur de police */
    color: #333;

    /* Taille de police */
    font-size: 24px;

    /* Marge supérieure */
    margin-top: 0;

    /* Marge inférieure */
    margin-bottom: 20px;
}

/* Sélecteur pour les éléments avec la classe "texte" */
.texte {
    /* Couleur de police */
    color: #666;

    /* Taille de police */
    font-size: 16px;

    /* Marge inférieure */
    margin-bottom: 20px;
}

/* Sélecteur pour les éléments avec la classe "bouton" */
.bouton {
    /* Couleur d'arrière-plan */
    background-color: #007bff;

    /* Couleur de police */
    color: #fff;

    /* Rembourrage */
    padding: 10px 20px;

    /* Marge supérieure */
    margin-top: 10px;

    /* Bordure */
    border: none;

    /* Rayon des bordures */
    border-radius: 5px;

    /* Curseur */
    cursor: pointer;

    /* Transition */
    transition: all 0.3s ease-in-out;
}

/* Pseudo-classe pour le bouton lorsqu'il est survolé */
.bouton:hover {
    /* Couleur d'arrière-plan */
    background-color: #0069d9;
}
```

**Explication du code :**

* Le mixin `ombre-portee` permet de définir facilement des ombres portées en spécifiant les valeurs de décalage, de flou et d'étalement.
* Le sélecteur `.conteneur` définit les propriétés de style pour les éléments avec la classe "conteneur", y compris la couleur d'arrière-plan, les marges, les bordures et l'appel du mixin d'ombre portée.
* Les sélecteurs `.titre`, `.texte` et `.bouton` définissent les propriétés de style pour les éléments avec les classes respectives, telles que la couleur de police, la taille de police et les marges.
* Les pseudo-classes comme `:hover` permettent de modifier les propriétés de style pour les éléments lorsqu'ils sont survolés avec la souris.