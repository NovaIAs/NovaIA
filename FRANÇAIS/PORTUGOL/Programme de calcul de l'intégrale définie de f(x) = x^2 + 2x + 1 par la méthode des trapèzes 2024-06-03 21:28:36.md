**Programme de calcul de l'intégrale définie de f(x) = x^2 + 2x + 1 sur l'intervalle [a, b] à l'aide de la méthode des trapèzes**

```portugol

programa integral_dos_trapezios

// FUNÇÕES DE APOIO

funcao trap(f, a, b, n)
{// Calcula a integral definida de f(x) sur l'intervalle [a, b] usando a méthode des trapèzes
    var delta: real
    var i: inteiro
    var s: real

    delta := (b - a) / n
    s := (f(a) + f(b)) / 2.0
    pour i := 1 jusqu'à n - 1 faire
        s := s + f(a + i * delta)
    fsi
    renvoyer delta * s
}

funcao f(x)
{// Fonction dont on veut calculer l'intégrale
    renvoyer x * x + 2.0 * x + 1.0
}

// PROGRAMME PRINCIPAL

var a, b, n: entier
var resultado: real

ecrire("Entrez la limite inférieure de l'intervalle (a) : ")
lire(a)
ecrire("Entrez la limite supérieure de l'intervalle (b) : ")
lire(b)
ecrire("Entrez le nombre de trapèzes (n) : ")
lire(n)

resultado := trap(f, a, b, n)

ecrire("L'intégrale définie de f(x) sur l'intervalle [", a, ", ", b, "] est : ", resultado)

fin

```

**Explication du code :**

* Les fonctions `trap` et `f` sont définies pour faciliter le calcul de l'intégrale.
* La fonction `trap` prend en arguments la fonction à intégrer (`f`), les limites de l'intervalle (`a` et `b`), et le nombre de trapèzes (`n`).
* La fonction `f` définit la fonction dont on veut calculer l'intégrale (dans ce cas, `x^2 + 2x + 1`).
* Dans le programme principal, l'utilisateur entre les limites de l'intervalle et le nombre de trapèzes.
* La fonction `trap` est appelée pour calculer l'intégrale, et le résultat est affiché à l'écran.