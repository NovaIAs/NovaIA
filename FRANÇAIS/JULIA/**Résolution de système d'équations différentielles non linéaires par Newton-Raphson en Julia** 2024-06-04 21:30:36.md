**Résolution d'un système d'équations différentielles non linéaires par la méthode de Newton-Raphson**

**Objectif :** Résoudre un système d'équations différentielles non linéaires de la forme :

```
y'(t) = f(t, y(t))
```

avec des conditions initiales données.

**Code :**

```julia
using DifferentialEquations, Optim

# Fonction définissant le système d'équations
function f(t, y)
    return [y[1] - y[2] + 1; y[2] - y[1] - 1]
end

# Définition des conditions initiales
y0 = [0.0, 1.0]

# Paramétrage de la méthode de Newton-Raphson
maxiter = 100
tol = 1e-6

# Résolution du système
prob = DifferentialEquations.ODEProblem(f, y0, 0.0, 1.0)
solver = Newton()
sol = solve(prob, solver, maxiter, tol)

# Affichage de la solution
println("Solution :")
for i in 1:length(sol.t)
    println("  t = $(sol.t[i]), y = $(sol.y[i,:])")
end
```

**Explication du code :**

* La fonction `f` définit le système d'équations différentielles.
* Les conditions initiales sont stockées dans `y0`.
* La méthode de Newton-Raphson est paramétrée avec un nombre maximal d'itérations (`maxiter`) et une tolérance (`tol`).
* Le problème d'équations différentielles est défini à l'aide de `DifferentialEquations.ODEProblem`.
* Le solveur `Newton()` est utilisé pour résoudre le problème.
* La solution est stockée dans `sol` et contient les valeurs de `t` et `y` pour chaque point de temps.
* Enfin, la solution est affichée.