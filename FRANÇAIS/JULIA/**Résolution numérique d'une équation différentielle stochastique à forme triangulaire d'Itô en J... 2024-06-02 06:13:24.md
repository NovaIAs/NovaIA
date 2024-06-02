**Calculer la solution numérique d'une équation différentielle stochastique sous la forme triangulaire d'Itô**

```julia
using DifferentialEquations, Distributions, LinearAlgebra

# Fonctions de dérive et de diffusion pour le processus d'Itô
drift(u, t) = u
diffusion(u, t) = [1.0, 0.0; 0.0, 1.0]

# Définition des paramètres de l'équation différentielle stochastique
initial_state = randn(2, 1)  # État initial
time_span = (0.0, 1.0)      # Intervalle de temps
num_steps = 100            # Nombre de pas de temps

# Méthode de résolution
solver = SDEdiffeq(drift, diffusion, initial_state)

# Résolution de l'équation différentielle stochastique
solution = solve(solver, time_span, num_steps)

# Affichage de la solution
for i in 1:num_steps
    println("État à t = $(time_span[2] * i / num_steps): $(solution[i, :])")
end
```

**Ce code implémente le schéma d'Euler-Maruyama pour résoudre une équation différentielle stochastique sous la forme triangulaire d'Itô.**

**Voici un aperçu du code :**

* **Fonctions de dérive et de diffusion** : Ces fonctions définissent l'évolution déterministe et stochastique du processus d'Itô, respectivement.
* **Définition des paramètres** : Cela inclut l'état initial, l'intervalle de temps et le nombre de pas de temps.
* **Méthode de résolution** : La méthode `SDEdiffeq` est utilisée pour résoudre l'équation différentielle stochastique.
* **Résolution** : La fonction `solve` est utilisée pour résoudre l'équation différentielle stochastique et renvoyer la solution.
* **Affichage de la solution** : La solution est affichée à chaque pas de temps spécifié.