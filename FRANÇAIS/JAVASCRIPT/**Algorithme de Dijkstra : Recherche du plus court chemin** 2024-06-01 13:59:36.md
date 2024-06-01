**Un algorithme de recherche de chemin le plus court (Dijkstra)**

```javascript
const dijkstra = (graphe, noeudDepart) => {
  // Initialisation des distances pour tous les nœuds
  const distances = graphe.nœuds.reduce((acc, nœud) => {
    acc[nœud] = nœud === noeudDepart ? 0 : Infinity;
    return acc;
  }, {});

  // Initialisation de la file de priorité
  const filePrioritaire = new PriorityQueue((a, b) => distances[a] < distances[b]);
  filePrioritaire.push(noeudDepart);

  // Tant que la file de priorité n'est pas vide
  while (!filePrioritaire.isEmpty()) {

    // Récupération du nœud avec la plus petite distance
    const nœudCourant = filePrioritaire.pop();

    // Si le nœud courant a une distance infinie, alors il n'est pas accessible
    if (distances[nœudCourant] === Infinity) {
      continue;
    }

    // Pour tous les voisins du nœud courant
    for (let voisin of graphe.voisins(nœudCourant)) {
      const distanceVoisin = distances[voisin];
      const poidsArête = graphe.poidsArête(nœudCourant, voisin);

      // Si la distance du voisin est supérieure à la distance du nœud courant + le poids de l'arête
      if (distanceVoisin > distances[nœudCourant] + poidsArête) {

        // Mise à jour de la distance du voisin
        distances[voisin] = distances[nœudCourant] + poidsArête;

        // Ajout du voisin à la file de priorité
        if (!filePrioritaire.has(voisin)) {
          filePrioritaire.push(voisin);
        }
      }
    }
  }

  return distances;
};
```

**Explication du code :**

Cet algorithme de Dijkstra calcule les distances les plus courtes depuis un nœud de départ donné vers tous les autres nœuds d'un graphe pondéré.

1. **Initialisation des distances :** On initialise un dictionnaire `distances` qui contient la distance de chaque nœud au nœud de départ.

2. **Initialisation de la file de priorité :** On crée une file de priorité qui trie les nœuds en fonction de leur distance au nœud de départ.

3. **Boucle principale :** On boucle tant que la file de priorité n'est pas vide.

4. **Récupération du nœud avec la plus petite distance :** On récupère le nœud avec la plus petite distance de la file de priorité.

5. **Mise à jour des distances :** On met à jour les distances des voisins du nœud courant en considérant le poids des arêtes entre les nœuds.

6. **Ajout des voisins à la file de priorité :** Si un voisin a une distance inférieure à celle actuellement stockée, on le met à jour et on l'ajoute à la file de priorité.

7. **Fin de l'algorithme :** Une fois que tous les nœuds ont été traités, le dictionnaire `distances` contient les distances les plus courtes depuis le nœud de départ.