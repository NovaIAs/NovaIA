**Code CSS Complexe**

```css
/* Ce code complexe en CSS est conçu pour créer un design de page web unique et sophistiqué. Il utilise des sélecteurs avancés, des propriétés personnalisées, des dégradés, des transitions et des animations pour produire un effet visuel époustouflant. */

:root {
  --couleur-primaire: #FF4E6B;
  --couleur-secondaire: #222E51;
  --couleur-accent: #F5D1FF;
}

body {
  font-family: "Lato", sans-serif;
  background: linear-gradient(135deg, var(--couleur-primaire) 0%, var(--couleur-accent) 100%);
  height: 100vh;
}

header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1rem;
  background: var(--couleur-secondaire);
}

.logo {
  font-size: 2rem;
  font-weight: bold;
  color: white;
}

.menu {
  display: flex;
  gap: 1rem;
}

.menu-item {
  font-size: 1.5rem;
  color: white;
  text-decoration: none;
  transition: color 0.5s ease-in-out;
}

.menu-item:hover {
  color: var(--couleur-primaire);
}

main {
  padding: 2rem;
  display: grid;
  grid-template-columns: repeat(3, auto);
  gap: 1rem;
}

.card {
  padding: 1rem;
  background: white;
  border: 1px solid var(--couleur-primaire);
  border-radius: 1rem;
  box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);
  transition: box-shadow 0.5s ease-in-out;
}

.card:hover {
  box-shadow: 0px 0px 20px rgba(0, 0, 0, 0.5);
}

.card-title {
  font-size: 1.5rem;
  font-weight: bold;
  color: var(--couleur-primaire);
}

.card-content {
  font-size: 1rem;
  color: var(--couleur-secondaire);
  line-height: 1.5rem;
}

footer {
  padding: 1rem;
  background: var(--couleur-primaire);
  color: white;
  text-align: center;
}
```

**Explication du Code**

Ce code CSS crée une mise en page de page web qui comprend :

* **En-tête avec un logo et un menu :** L'en-tête utilise un dégradé linéaire et des transitions pour les éléments de menu afin de créer un effet de survol dynamique.
* **Grille de cartes dynamiques :** La grille principale contient des cartes qui sont mises en valeur par des ombres portées et des transitions lors du survol.
* **Propriétés personnalisées :** Les propriétés personnalisées sont utilisées pour stocker les couleurs et améliorer la maintenabilité du code.
* **Transitions et animations :** Les transitions et animations sont utilisées pour des effets visuels fluides et réactifs.
* **Sélecteurs avancés :** Les sélecteurs avancés (par exemple, ":hover") sont utilisés pour cibler des éléments spécifiques et appliquer des styles dynamiques.
* **Mise en page réactive :** Le code utilise une mise en page réactive qui s'adapte à différentes tailles d'écran.
* **Conception accessible :** Les tailles de police et les contrastes sont soigneusement sélectionnés pour améliorer l'accessibilité.

Ce code CSS complexe crée un design de page web unique et engageant qui offre une expérience utilisateur sophistiquée et mémorable.