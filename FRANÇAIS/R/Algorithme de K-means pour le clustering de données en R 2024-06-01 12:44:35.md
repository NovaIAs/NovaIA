```r

# Cette fonction implémente l'algorithme de K-means pour le clustering de données.
kmeans <- function(data, k, max_iterations = 100, tolerance = 1e-6) {

  # Vérifier les arguments d'entrée
  if (k < 1 || k > nrow(data)) {
    stop("Le nombre de clusters doit être compris entre 1 et le nombre de lignes de données.")
  }
  if (max_iterations < 1) {
    stop("Le nombre d'itérations doit être supérieur ou égal à 1.")
  }
  if (tolerance < 0) {
    stop("La tolérance doit être positive ou nulle.")
  }

  # Initialiser les centres des clusters
  centers <- data[sample(nrow(data), k), ]

  # Itérer jusqu'à ce que les centres ne changent plus ou que le nombre maximal d'itérations soit atteint
  for (i in 1:max_iterations) {

    # Attribuer les points de données aux clusters les plus proches
    clusters <- sapply(data, function(x) which.min(dist(x, centers)))

    # Mettre à jour les centres des clusters
    centers <- lapply(1:k, function(j) {
      mean(data[clusters == j, , drop = FALSE])
    })

    # Vérifier si les centres ont changé
    if (all(abs(rowMeans(centers) - rowMeans(centers_old)) < tolerance)) {
      break
    }

    # Mettre à jour les centres antérieurs
    centers_old <- centers
  }

  # Renvoyer les clusters et les centres
  return(list(clusters, centers))
}

# Exemple d'utilisation
data <- data.frame(
  x = rnorm(100, mean = 0, sd = 1),
  y = rnorm(100, mean = 0, sd = 1)
)
clusters <- kmeans(data, k = 3)$clusters
plot(data, col = clusters)

```

**Explication du code :**

Cette fonction :

1. **Vérifie les arguments d’entrée :** Valide les valeurs de `k` (nombre de clusters), `max_iterations` (nombre maximal d’itérations) et `tolerance` (tolérance pour les changements de centres).
2. **Initialise les centres des clusters :** Choisit aléatoirement `k` points de données comme centres initiaux.
3. **Itère jusqu’à la convergence :**
   - **Attribue les points de données aux clusters :** Calcule la distance entre chaque point de données et chaque centre et attribue le point au cluster du centre le plus proche.
   - **Met à jour les centres des clusters :** Calcule les moyennes des points de données dans chaque cluster pour mettre à jour les centres.
   - **Vérifie la convergence :** Compare les nouveaux centres aux anciens et s’arrête si la différence est inférieure à la tolérance ou que le nombre maximal d’itérations est atteint.
4. **Renvoie les clusters et les centres :** Renvoie une liste contenant les affectations de clusters et les centres finaux.

Ce code implémente l’algorithme de K-means pour partitionner un ensemble de données en un nombre spécifié de clusters. Il est couramment utilisé pour l’analyse de groupement et l’exploration de données.