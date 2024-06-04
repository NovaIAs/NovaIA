```css
/* **Mélangeur de couleur** */

/* Fonctions mélangeuses de couleurs */
@function mixer-couleur($couleur1, $couleur2, $poids) {
  $r1: red($couleur1);
  $g1: green($couleur1);
  $b1: blue($couleur1);

  $r2: red($couleur2);
  $g2: green($couleur2);
  $b2: blue($couleur2);

  $r: round(mix($r1, $r2, $poids));
  $g: round(mix($g1, $g2, $poids));
  $b: round(mix($b1, $b2, $poids));

  rgb($r, $g, $b);
}

/* Éclaircissement et assombrissement d'une couleur */
@function éclaircir($couleur, $pourcentage) {
  mixer-couleur($couleur, white, $pourcentage);
}

@function assombrir($couleur, $pourcentage) {
  mixer-couleur($couleur, black, $pourcentage);
}

/* **Typographie** */

/* Styles de police pour les titres */
@class police-titre {
  font-family: "Roboto", sans-serif;
  font-weight: bold;
  font-size: 2rem;
}

/* Styles de police pour les paragraphes */
@class police-paragraphe {
  font-family: "Lato", sans-serif;
  font-size: 1.2rem;
  line-height: 1.5em;
}

/* **Mise en page** */

/* Conteneur principal */
@class conteneur-principal {
  margin: 0 auto;
  max-width: 800px;
  padding: 20px;

  /* Arrière-plan dégradé */
  background: linear-gradient(45deg, #333333 0%, #666666 100%);
}

/* Barre latérale */
@class barre-latérale {
  float: left;
  width: 20%;
  background: #ffffff;

  /* Effet d'ombre */
  -webkit-box-shadow: 0px 2px 4px 0px rgba(0, 0, 0, 0.2);
  -moz-box-shadow: 0px 2px 4px 0px rgba(0, 0, 0, 0.2);
  box-shadow: 0px 2px 4px 0px rgba(0, 0, 0, 0.2);
}

/* Contenu principal */
@class contenu-principal {
  margin-left: 20%;
}

/* **Animations** */

/* Animation de fondu */
@keyframes fondu {
  from { opacity: 0; }
  to { opacity: 1; }
}

/* Animation d'apparition par glissement */
@keyframes glissement {
  from { transform: translateX(-100%); }
  to { transform: translateX(0); }
}
```

**Explication :**

Ce code CSS comporte :

* **Mélangeur de couleurs :** Fonctions pour mélanger, éclaircir et assombrir les couleurs.
* **Typographie :** Classes pour différents styles de police et de paragraphe.
* **Mise en page :** Classes pour le conteneur principal, la barre latérale et le contenu principal.
* **Animations :** Keyframes pour une animation de fondu et une animation d'apparition par glissement.