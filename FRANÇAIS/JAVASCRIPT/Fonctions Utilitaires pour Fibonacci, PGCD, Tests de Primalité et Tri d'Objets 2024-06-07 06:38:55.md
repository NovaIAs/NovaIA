```javascript
// Déclaration d'une fonction récursive pour générer la séquence de Fibonacci jusqu'à un indice donné
const fibonacci = (n) => {
  // Cas de base : si n est 0 ou 1, renvoie la valeur correspondante de la séquence
  if (n === 0) {
    return 0;
  } else if (n === 1) {
    return 1;
  }

  // Appel récursif : somme les deux éléments précédents de la séquence
  return fibonacci(n - 1) + fibonacci(n - 2);
};

// Déclaration d'une fonction pour trouver le plus grand commun diviseur (PGCD) de deux nombres
const pgcd = (a, b) => {
  // Si b est 0, renvoie a
  if (b === 0) {
    return a;
  }

  // Sinon, renvoie le PGCD de b et du reste de la division euclidienne de a par b
  return pgcd(b, a % b);
};

// Déclaration d'une fonction pour déterminer si un nombre est premier
const estPremier = (nombre) => {
  // Si le nombre est inférieur ou égal à 1, renvoie faux
  if (nombre <= 1) {
    return false;
  }

  // Pour chaque nombre de 2 à la racine carrée du nombre donné, vérifie s'il divise le nombre sans reste
  for (let i = 2; i <= Math.sqrt(nombre); i++) {
    if (nombre % i === 0) {
      return false;
    }
  }

  // Si aucun diviseur n'a été trouvé, renvoie vrai
  return true;
};

// Déclaration d'une fonction pour trier un tableau d'objets en fonction d'une propriété
const trierParPropriete = (tableau, propriete) => {
  // Trie le tableau à l'aide de la méthode sort et de la fonction de comparaison fournie
  tableau.sort((a, b) => {
    // Compare les valeurs des propriétés spécifiées des deux objets
    if (a[propriete] < b[propriete]) {
      return -1;
    } else if (a[propriete] > b[propriete]) {
      return 1;
    }

    // Si les valeurs des propriétés sont égales, renvoie 0
    return 0;
  });
};

// Utilisation des fonctions déclarées

// Génération des 10 premiers nombres de Fibonacci
console.log("Séquence de Fibonacci : ", fibonacci(10));

// Calcul du PGCD de 12 et 18
console.log("PGCD de 12 et 18 : ", pgcd(12, 18));

// Vérification si 17 est premier
console.log("17 est-il premier ?", estPremier(17));

// Tri d'un tableau d'objets par la propriété "nom"
const contacts = [
  { nom: "Alice", age: 25 },
  { nom: "Bob", age: 30 },
  { nom: "Charlie", age: 28 },
];
trierParPropriete(contacts, "nom");
console.log("Contacts triés par nom : ", contacts);
```

**Explications du code :**

* La fonction `fibonacci` utilise la récursivité pour générer la séquence de Fibonacci jusqu'à l'indice `n`.
* La fonction `pgcd` utilise l'algorithme d'Euclide pour trouver le PGCD de deux nombres.
* La fonction `estPremier` vérifie si un nombre est premier en testant tous les nombres de 2 à sa racine carrée pour voir s'ils sont des diviseurs.
* La fonction `trierParPropriete` utilise la méthode de tri JavaScript pour trier un tableau d'objets en fonction d'une propriété donnée.