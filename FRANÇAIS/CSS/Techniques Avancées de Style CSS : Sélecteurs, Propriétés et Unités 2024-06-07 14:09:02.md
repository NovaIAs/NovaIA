```css
/* Sélecteurs complexes - cibler des éléments spécifiques en fonction de leur position, de leurs relations ou de leurs états */

/* Sélecteurs d'attribut avancés - cibler des éléments en fonction des valeurs de leurs attributs */
[attribut="valeur"] {}
[attribut~="valeur"] {}
[attribut^="valeur"] {}
[attribut$="valeur"] {}
[attribut*="valeur"] {}

/* Sélecteurs pseudo-classes - cibler des éléments en fonction de leur état */
:first-child {}
:last-child {}
:nth-child(n) {}
:nth-of-type(n) {}
:empty {}
:target {}
:hover {}

/* Sélecteurs pseudo-éléments - cibler des parties spécifiques d'éléments */
::before {}
::after {}

/* Sélecteurs combinés - combiner des sélecteurs pour cibler des éléments appartenant à plusieurs catégories */
parent > enfant {}
parent ~ frère {}
parent + frère {}

/* Spécificité - déterminer quel sélecteur s'applique en cas de conflits */
/* Les sélecteurs plus spécifiques ont une priorité plus élevée */

/* Spécifique (ID) */
#mon_id {}

/* Spécifique (classe) */
.ma_classe {}

/* Moins spécifique (élément) */
p {}

/* Sélecteurs globaux - cibler tous les éléments d'un document */
* {}

/* Déclarations de propriété imbriquées - imbriquer des sélecteurs pour créer des règles spécifiques */
.ma_classe {
  color: rouge;
  font-size: 12px;
}

/* Unités de mesure - spécifier les valeurs des propriétés en utilisant différentes unités */
em {} % {} px {} cm {}

/* Couleurs - spécifier les couleurs en utilisant des noms, des valeurs RVB, des valeurs RGBA ou des valeurs hexadécimales */
color: rouge;
color: rgb(255, 0, 0);
color: rgba(255, 0, 0, 0.5);
color: #FF0000;

/* Polices - spécifier les polices en utilisant des noms de police, des tailles de police et des styles de police */
font-family: Arial, sans-serif;
font-size: 12px;
font-style: italic;

/* Arrière-plans - spécifier les couleurs d'arrière-plan, les images d'arrière-plan et les positions d'arrière-plan */
background-color: noir;
background-image: url(image.jpg);
background-position: center;

/* Marges et paddings - spécifier les espaces autour des éléments */
margin: 10px;
padding: 10px;

/* Bordures - spécifier la largeur, le style et la couleur des bordures */
border: 1px solide noir;

/* Animation - animer les éléments en utilisant des propriétés de transition */
transition: all 1s ease-in-out;

/* Médias - appliquer des styles spécifiques en fonction de la taille de l'écran ou du type de périphérique */
@media (min-width: 768px) {
  body {
    color: bleu;
  }
}
```

**Explication du code :**

Ce code CSS complexe utilise des sélecteurs avancés, des propriétés et des unités de mesure pour créer des styles très spécifiques et différenciés pour les éléments d'un document. Il comprend :

* **Sélecteurs avancés** qui ciblent les éléments en fonction de leur position, de leurs relations ou de leurs états, tels que : `:first-child` pour cibler le premier enfant d'un parent, `[attribut="valeur"]` pour cibler les éléments avec un attribut spécifique ayant une valeur spécifique.
* **Unités de mesure** différentes, telles que les pixels (`px`), les pourcentages (`%`) et les centimètres (`cm`), pour spécifier les valeurs des propriétés.
* **Couleurs** spécifiées à l'aide de noms de couleurs, de valeurs RVB ou RGBA, ou de valeurs hexadécimales, pour définir la couleur du texte, de l'arrière-plan ou des bordures.
* **Polices** spécifiées à l'aide de noms de police, de tailles de police et de styles de police, pour contrôler l'apparence du texte.
* **Arrière-plans** spécifiés à l'aide de couleurs d'arrière-plan, d'images d'arrière-plan et de positions d'arrière-plan, pour définir l'apparence de l'arrière-plan des éléments.
* **Marges et paddings** spécifiés pour créer des espaces autour des éléments, ce qui affecte leur positionnement et leur espacement.
* **Bordures** spécifiées à l'aide de la largeur, du style et de la couleur des bordures, pour contrôler l'apparence des bordures autour des éléments.
* **Animation** spécifiée à l'aide de propriétés de transition, pour animer les éléments lorsqu'ils sont modifiés.
* **Médias** qui appliquent des styles spécifiques en fonction de la taille de l'écran ou du type de périphérique, pour créer des mises en page responsives.