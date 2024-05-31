```julia
# On importe la librairie `DifferentialEquations` pour résoudre les équations différentielles
using DifferentialEquations

# On définit la fonction différentielle qui décrit le système dynamique
f(u, p, t) = [p[1]*u[1] - p[2]*u[2]; p[2]*u[1] + p[1]*u[2]]

# On définit les paramètres du système
p = [0.5, 1.0]

# On définit les conditions initiales
u0 = [1.0, 0.0]

# On résout le système d'équations différentielles
sol = solve(f, u0, tspan=(0.0, 10.0), parameters=p, abstol=1e-6, reltol=1e-6)

# On trace la solution
plot(sol.t, sol.u[1], label="u1")
plot!(sol.t, sol.u[2], label="u2")
xlabel("Temps")
ylabel("Valeur")
legend()
```

**Explication du code :**

1. On importe la librairie `DifferentialEquations` qui fournit des fonctions pour résoudre des équations différentielles.

2. On définit la fonction différentielle qui décrit le système dynamique. Cette fonction prend en entrée l'état du système `u`, les paramètres du système `p` et le temps `t`, et renvoie les dérivées des états du système.

3. On définit les paramètres du système `p` qui sont constants.

4. On définit les conditions initiales du système `u0` qui sont les valeurs des états du système au temps initial.

5. On résout le système d'équations différentielles en utilisant la fonction `solve`. Cette fonction prend en entrée la fonction différentielle, les conditions initiales, l'intervalle de temps sur lequel résoudre le système et les paramètres du système. Elle renvoie une solution qui contient les valeurs des états du système pour chaque temps dans l'intervalle de temps donné.

6. On trace la solution en utilisant la fonction `plot`. Cette fonction prend en entrée les temps et les valeurs des états du système, et trace les courbes de ces valeurs en fonction du temps.