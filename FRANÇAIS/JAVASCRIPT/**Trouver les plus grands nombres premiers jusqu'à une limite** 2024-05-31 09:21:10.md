```javascript
function rechercherLesPlusGrandsNombresPremiers(limite) {
  // Initialise une liste pour stocker les nombres premiers
  const nombresPremiers = [];

  for (let i = 2; i <= limite; i++) {
    // Vérifie si le nombre actuel est premier
    let estPremier = true;
    for (let j = 2; j <= Math.sqrt(i); j++) {
      if (i % j === 0) {
        estPremier = false;
        break;
      }
    }

    // Si le nombre est premier, ajoute-le à la liste
    if (estPremier) {
      nombresPremiers.push(i);
    }
  }

  // Inverse la liste pour obtenir les plus grands nombres premiers en premier
  return nombresPremiers.reverse();
}

// Teste la fonction avec une limite de 100
const plusGrandsNombresPremiers = rechercherLesPlusGrandsNombresPremiers(100);

// Affiche les 10 premiers nombres premiers obtenus
console.log(plusGrandsNombresPremiers.slice(0, 10));
```

**Explication du code :**

* La fonction `rechercherLesPlusGrandsNombresPremiers()` prend une limite en paramètre et retourne une liste des plus grands nombres premiers inférieurs ou égaux à cette limite.

* La fonction commence par initialiser une liste vide pour stocker les nombres premiers.

* Il parcourt ensuite les nombres de 2 à la limite (incluse).

* Pour chaque nombre, il vérifie s'il est premier en utilisant une boucle imbriquée qui divise le nombre par tous les entiers de 2 à sa racine carrée. Si le nombre est divisible par l'un de ces entiers, il n'est pas premier.

* Si le nombre est premier, il est ajouté à la liste des nombres premiers.

* Enfin, la fonction inverse la liste pour obtenir les plus grands nombres premiers en premier.

* Le code teste ensuite la fonction avec une limite de 100 et affiche les 10 premiers nombres premiers obtenus.