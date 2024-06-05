**Problème:** Modéliser la croissance d'une population d'insectes en utilisant l'équation logistique.

**Code R:**

```r
# Chargement des bibliothèques
library(tidyverse)
library(shiny)
library(ggplot2)
library(tidymodels)

# Chargement des données
insect_data <- read.csv("insect_data.csv")

# Préparation des données
insect_data <- insect_data %>%
  rename(num_insectes = individus)

# Modélisation de la croissance de la population
model <- logistic_reg() %>%
  fit(num_insectes ~ jours, data = insect_data)

# Prédiction de la croissance future
futur_jours <- seq(max(insect_data$jours) + 1, max(insect_data$jours) + 30, by = 1)

predictions <- model %>%
  predict(new_data = tibble(jours = futur_jours))

# Création d'une application Shiny
ui <- fluidPage(
  titlePanel("Croissance de la population d'insectes"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "jours",
        "Nombre de jours",
        min = min(insect_data$jours),
        max = max(insect_data$jours),
        value = max(insect_data$jours)
      )
    ),
    mainPanel(
      plotOutput("growth_plot")
    )
  )
)

server <- function(input, output) {
  output$growth_plot <- renderPlot({
    ggplot(insect_data, aes(x = jours, y = num_insectes)) +
      geom_line(color = "blue") +
      geom_line(data = predictions, aes(x = jours, y = .pred), color = "red") +
      labs(title = "Croissance de la population d'insectes")
  })
}

shinyApp(ui, server)
```

**Explication du code:**

* **Chargement des bibliothèques:** Les bibliothèques `tidyverse`, `shiny`, `ggplot2` et `tidymodels` sont chargées pour manipuler et visualiser les données.
* **Chargement des données:** Les données sur la population d'insectes sont chargées à partir d'un fichier CSV.
* **Préparation des données:** Les données sont renommées pour une clarté accrue.
* **Modélisation de la croissance de la population:** Un modèle logistique est créé pour modéliser la relation entre le nombre d'insectes et le nombre de jours.
* **Prédiction de la croissance future:** Des prédictions sont effectuées pour estimer la croissance de la population sur une période de 30 jours supplémentaires.
* **Création d'une application Shiny:** Une application Shiny est créée pour visualiser de manière interactive la croissance de la population d'insectes. L'interface utilisateur (UI) comprend un panneau latéral avec un curseur pour ajuster le nombre de jours. Les données observées et prédites sont tracées dans le panneau principal.
* **Serveur Shiny:** Le serveur Shiny traite les interactions de l'utilisateur et met à jour l'affichage en conséquence.