**Fonction de Résolution d'Équations Différentielles Ordinaires**

```julia
function ode_solver(f, y0, dt, tstart, tend)
    # Initialisation
    t = [tstart : dt : tend]
    y = [y0]

    # Boucle de résolution
    for i in 2:length(t)
        y_next = y[i-1] + dt * f(y[i-1], t[i-1])
        y = push!(y, y_next)
    end

    # Retourne le temps et les solutions
    return (t, y)
end
```

**Exemple d'Utilisation**

```julia
# Résoudre l'équation y' = y avec une condition initiale y(0) = 1
f(y, t) = y

# Paramètres de résolution
y0 = 1
dt = 0.01
tstart = 0
tend = 10

# Résolution de l'équation
(t, y) = ode_solver(f, y0, dt, tstart, tend)

# Affichage des résultats
for i in 1:length(t)
    println("t = ", t[i], "  y = ", y[i])
end
```

**Code d'Intégration Numérique**

```julia
function integrate(f, a, b, n)
    h = (b - a) / n
    x = [a : h : b]
    s = 0.0

    for i in 1:n
        s += h * (f(x[i-1]) + f(x[i])) / 2
    end

    return s
end
```

**Exemple d'Utilisation**

```julia
# Intégrer la fonction sin(x) sur [0, π]
f(x) = sin(x)

# Paramètres d'intégration
a = 0
b = pi
n = 100

# Intégration de la fonction
result = integrate(f, a, b, n)

# Affichage du résultat
println("L'intégrale de sin(x) sur [0, π] est :", result)
```

**Code d'Ajustement de Courbe**

```julia
function curve_fit(x, y, model, params)
    # Prédiction du modèle
    predictions = model(params, x)

    # Calcul de l'erreur quadratique moyenne
    error = sum((y - predictions)^2) / length(y)

    # Retourne l'erreur et les paramètres
    return (error, params)
end
```

**Exemple d'Utilisation**

```julia
# Données de test
x = [1, 2, 3, 4, 5]
y = [2, 4, 6, 8, 10]

# Modèle de régression linéaire
model(params, x) = params[1] * x + params[2]

# Paramètres initiaux
params_init = [1.0, 1.0]

# Optimisation à l'aide de NLopt
optimizer = NLopt.LN_COBYLA()
optimized_params = NLopt.optimize(optimizer, curve_fit, params_init, x, y, model)

# Affichage des paramètres optimisés
println("Paramètre a :", optimized_params[1])
println("Paramètre b :", optimized_params[2])
```