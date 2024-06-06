```r
# Bibliothèques à charger
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(shiny)

# Données
data <- data.frame(
  date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
  valeur = rnorm(365, mean = 50, sd = 10)
)

# Nettoyage des données
data <- data %>%
  mutate(
    jour = wday(date, label = TRUE),
    mois = month(date, label = TRUE, abbreviate = TRUE)
  ) %>%
  select(date, jour, mois, valeur)

# Modèle de régression linéaire
modele <- lm(valeur ~ jour + mois, data = data)

# Résumé du modèle
summary(modele)

# Visualisation des données
ggplot(data, aes(x = date, y = valeur, color = jour)) +
  geom_line() +
  labs(title = "Évolution de la valeur au cours de l'année",
       x = "Date",
       y = "Valeur")

# Interface Shiny
ui <- fluidPage(
  titlePanel("Visualisation interactive des données"),
  sidebarPanel(
    sliderInput(
      "mois",
      "Mois",
      min = 1,
      max = 12,
      value = 1
    )
  ),
  mainPanel(
    plotlyOutput("plot")
  )
)

server <- function(input, output) {
  output$plot <- renderPlotly({
    data %>%
      filter(month(date) == input$mois) %>%
      ggplot(aes(x = date, y = valeur, color = jour)) +
      geom_line() +
      labs(title = "Évolution de la valeur pour le mois sélectionné",
           x = "Date",
           y = "Valeur")
  })
}

shinyApp(ui, server)
```

**Explications du code :**

* **Bibliothèques :** Le code charge plusieurs bibliothèques R pour manipuler et visualiser les données.
* **Données :** Le jeu de données représente l'évolution d'une valeur pour chaque jour de l'année.
* **Nettoyage des données :** Les données sont nettoyées pour extraire des informations telles que le jour de la semaine et le mois.
* **Modèle de régression :** Un modèle de régression linéaire est utilisé pour prédire la valeur en fonction du jour et du mois.
* **Résumé du modèle :** Le résumé présente des informations statistiques sur le modèle, telles que les coefficients et la signification.
* **Visualisation des données :** Un graphique linéaire est généré pour visualiser l'évolution de la valeur au cours de l'année, avec des couleurs différentes pour chaque jour de la semaine.
* **Interface Shiny :** Une interface interactive est créée à l'aide de Shiny, permettant aux utilisateurs de sélectionner un mois spécifique et de visualiser les données correspondantes.