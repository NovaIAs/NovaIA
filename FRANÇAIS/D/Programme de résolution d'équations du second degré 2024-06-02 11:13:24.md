**Programme de résolution d'équations quadratiques**

```d
import std.fmt, std.math, std.random;

void main(string[] args) {
    // Générer les coefficients aléatoires de l'équation quadratique
    real a = uniform(1.0, 10.0);
    real b = uniform(-10.0, 10.0);
    real c = uniform(-10.0, 10.0);

    // Calculer le discriminant
    real d = b * b - 4 * a * c;

    // Vérifier si l'équation a des solutions réelles
    if (d < 0) {
        writeln("L'équation n'a pas de solutions réelles.");
        return;
    }

    // Calculer les solutions
    real sqrtD = sqrt(d);
    real x1 = (-b + sqrtD) / (2 * a);
    real x2 = (-b - sqrtD) / (2 * a);

    // Afficher les solutions
    writeln("Solutions :");
    writeln("x1 = ", x1);
    writeln("x2 = ", x2);
}
```

**Explication du code :**

* Les coefficients `a`, `b` et `c` de l'équation quadratique `ax^2 + bx + c = 0` sont générés aléatoirement.
* Le discriminant `d` est calculé. S'il est négatif, l'équation n'a pas de solutions réelles.
* Les solutions `x1` et `x2` sont calculées à l'aide de la formule quadratique.
* Les solutions sont affichées à la console.