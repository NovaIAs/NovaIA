**Fonction de clustering hiérarchique multivarié à liens complets avec optimisation de l'indice de silhouette et visualisation interactive**

```r
# Importation des bibliothèques nécessaires
library(tidyverse)
library(stats)
library(dendextend)
library(shiny)

# Définition de la fonction de clustering
cluster_agglo_multivarié <- function(data, dist="euclidean", method="complete") {
  
  # Calcul de la matrice de distance
  dist_matrix <- dist(data, method = dist)
  
  # Création de l'objet de clustering
  clustering <- hclust(dist_matrix, method = method)
  
  # Optimisation de l'indice de silhouette
  optimal_k <- 1
  max_silhouette <- 0
  
  for (k in 2:ncol(data)) {
    
    # Création de k clusters
    clusters <- cutree(clustering, k)
    
    # Calcul de l'indice de silhouette
    silhouette <- silhouette(data, clusters)$silhouette
    
    # Mise à jour de k optimal et de l'indice de silhouette maximal
    if (mean(silhouette) > max_silhouette) {
      optimal_k <- k
      max_silhouette <- mean(silhouette)
    }
  }
  
  # Coupe du dendrogramme à k optimal
  clustering_optimal <- cutree(clustering, optimal_k)
  
  # Création de l'interface Shiny pour la visualisation interactive
  ui <- fluidPage(
    titlePanel("Visualisation du dendrogramme"),
    plotOutput("dendrogram")
  )
  
  server <- function(input, output) {
    output$dendrogram <- renderPlot({
      plot(clustering, main="Dendrogramme")
      rect.hclust(clustering, k=optimal_k, border="red")
    })
  }
  
  shinyApp(ui, server)
  
  # Renvoie l'objet de clustering optimal et l'indice de silhouette maximal
  return(list(clustering_optimal, max_silhouette))
}
```

**Exemple d'utilisation**

```r
# Données de l'iris
iris_data <- iris

# Clustering hiérarchique multivarié
clustering_iris <- cluster_agglo_multivarié(iris_data)

# Affichage du dendrogramme optimisé
shinyApp(ui, server)
```