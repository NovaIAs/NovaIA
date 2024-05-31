```r
# Régression linéaire généralisée bayésienne hiérarchique à plusieurs niveaux pour des données à temps discret

# Charger les bibliothèques
library(brms)
library(dplyr)
library(tidyverse)
library(bayesplot)

# Charger les données
data <- read.csv("data.csv")

# Modèle
mod <- brm(count ~ age + gender + (1 | group), 
           family = poisson, 
           data = data, 
           prior = c(prior(normal(0, 1), class = b),
                     prior(normal(0, 1), class = b)), 
           chains = 4, 
           iter = 2000, 
           warmup = 1000)

# Vérification du modèle
summary(mod)
plot(mod)

# Graphiques de convergence
mcmc_trace(mod)

# Prévisions
newdata <- data.frame(age = 30, gender = "femme", group = "A")
pred <- predict(mod, newdata = newdata)

# Graphique des prévisions
plot(pred)

# Sensibilité des hyperparamètres
prior_predictive(mod, newdraws = 100)

# Simulation à partir du modèle postérieur
sim <- simulate(mod, nsims = 100)

# Diagnostics
bayesrules::check_model(mod)

# Extraire les résultats
results <- mod %>% 
  tidy(effects = "fixed", conf.int = TRUE) %>% 
  filter(term != "(Intercept)")

# Affichage des résultats
print(results)
```