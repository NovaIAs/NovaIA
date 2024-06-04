**Fonctions auxiliaires:**

```r
# Fonction pour calculer la moyenne d'une liste de nombres
moyenne <- function(x) {
  return(sum(x) / length(x))
}

# Fonction pour calculer l'écart-type d'une liste de nombres
ecart_type <- function(x) {
  m <- moyenne(x)
  return(sqrt(sum((x - m)^2) / (length(x) - 1)))
}
```

**Fonction principale:**

```r
# Fonction pour effectuer une analyse statistique sur un ensemble de données
analyse_statistique <- function(data) {
  # Vérifier si les données sont valides
  if (!is.data.frame(data)) {
    stop("Les données doivent être un dataframe.")
  }

  # Calculer la moyenne et l'écart-type pour chaque variable numérique
  moyennes <- apply(data[, sapply(data, is.numeric)], 2, moyenne)
  ecart_types <- apply(data[, sapply(data, is.numeric)], 2, ecart_type)

  # Calculer la matrice de corrélation
  correlations <- cor(data[, sapply(data, is.numeric)])

  # Calculer la table de contingence pour les variables catégorielles
  contingence <- table(data[, sapply(data, is.factor)])

  # Générer un rapport contenant les résultats
  rapport <- paste0(
    "**Moyennes:**\n",
    paste0(" - ", names(moyennes), ": ", signif(moyennes, 3), "\n"),
    "\n**Écarts-types:**\n",
    paste0(" - ", names(ecart_types), ": ", signif(ecart_types, 3), "\n"),
    "\n**Matrice de corrélation:**\n",
    round(correlations, 3),
    "\n**Table de contingence:**\n",
    as.matrix(contingence)
  )

  # Renvoyer le rapport
  return(rapport)
}
```

**Utilisation du code:**

```r
# Charger les données
data <- read.csv("data.csv")

# Effectuer l'analyse statistique
rapport <- analyse_statistique(data)

# Afficher le rapport
print(rapport)
```

**Explication:**

La fonction `analyse_statistique()` est une fonction complexe qui effectue une analyse statistique complète sur un dataframe. Elle calcule la moyenne, l'écart-type, la matrice de corrélation et la table de contingence pour les variables numériques et catégorielles, respectivement. Les fonctions auxiliaires `moyenne()` et `ecart_type()` sont utilisées pour calculer la moyenne et l'écart-type. La fonction `cor()` est utilisée pour calculer la matrice de corrélation et la fonction `table()` est utilisée pour calculer la table de contingence. Le rapport généré par la fonction contient les résultats de l'analyse, organisés en sections pour la moyenne, l'écart-type, la matrice de corrélation et la table de contingence.