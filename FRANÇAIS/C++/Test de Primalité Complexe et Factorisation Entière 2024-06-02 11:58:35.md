**Test de primalité complexe et algorithme de factorisation**

```c++
#include <iostream>
#include <vector>
#include <cmath>
#include <cstring>

// Test de primalité de Miller-Rabin
bool millerRabin(const unsigned long long& n, const int& k) {
    if (n < 2) return false;
    if (n % 2 == 0) return (n == 2);

    unsigned long long s = 0;
    unsigned long long d = n - 1;
    while (d % 2 == 0) {
        ++s;
        d /= 2;
    }

    for (int i = 0; i < k; ++i) {
        unsigned long long a = rand() % (n - 3) + 2;
        unsigned long long x = powmod(a, d, n);
        if (x != 1 && x != n - 1) {
            for (int r = 1; r <= s - 1; ++r) {
                x = powmod(x, 2, n);
                if (x == n - 1) break;
            }
            if (x != n - 1) return false;
        }
    }
    return true;
}

// Calcul de puissance modulaire
unsigned long long powmod(const unsigned long long& base, const unsigned long long& exp, const unsigned long long& mod) {
    unsigned long long result = 1;
    unsigned long long pow = base % mod;
    while (exp) {
        if (exp & 1) result = (result * pow) % mod;
        pow = (pow * pow) % mod;
        exp >>= 1;
    }
    return result;
}

// Algorithme de factorisation de Pollard-Rho
unsigned long long pollardRho(const unsigned long long& n) {
    if (n % 2 == 0) return 2;
    if (n % 3 == 0) return 3;

    unsigned long long x = rand() % (n - 3) + 2;
    unsigned long long y = x;
    unsigned long long c = rand() % n;

    while (true) {
        x = (x * x + c) % n;
        y = (y * y + c) % n;
        y = (y * y + c) % n;

        if (x != y) {
            unsigned long long gcd = __gcd(abs(x - y), n);
            if (gcd > 1 && gcd < n) return gcd;
        }
    }
}

// Algorithme de factorisation entière
vector<unsigned long long> factoriser(const unsigned long long& n) {
    vector<unsigned long long> facteurs;

    if (n == 1) return facteurs;
    if (millerRabin(n, 10)) {
        facteurs.push_back(n);
        return facteurs;
    }

    unsigned long long p;
    while (true) {
        p = pollardRho(n);
        if (p == n) break;
        facteurs.push_back(p);
        n /= p;
    }
    facteurs.push_back(n);

    return facteurs;
}

int main() {
    unsigned long long n;
    cout << "Entrez un entier à factoriser : ";
    cin >> n;

    vector<unsigned long long> facteurs = factoriser(n);
    cout << "Facteurs de " << n << " : ";
    for (const auto& facteur : facteurs) cout << facteur << " ";
    cout << endl;

    return 0;
}
```

**Explication du code :**

Ce code effectue une factorisation entière d'un nombre donné `n` en utilisant une combinaison du test de primalité de Miller-Rabin et de l'algorithme de factorisation de Pollard-Rho.

**Fonction `millerRabin` :**

* Teste si `n` est premier en utilisant l'algorithme de Miller-Rabin.
* Renvoie `true` si `n` est probablement premier, `false` sinon.

**Fonction `powmod` :**

* Calcule la puissance modulaire `base^exp % mod` de manière efficace.

**Fonction `pollardRho` :**

* Utilise l'algorithme de Pollard-Rho pour factoriser `n`.
* Renvoie un facteur non trivial de `n` s'il en trouve un, sinon renvoie `n`.

**Fonction `factoriser` :**

* Factorise `n` en utilisant une combinaison de Miller-Rabin et Pollard-Rho.
* Renvoie un vecteur contenant les facteurs premiers de `n`.

**Fonction `main` :**

* Demande à l'utilisateur un entier `n`.
* Factorise `n` et affiche les facteurs trouvés.

**Complexité du code :**

La complexité du code est déterminée principalement par l'algorithme de factorisation de Pollard-Rho. Pollard-Rho a une complexité temporelle attendue O(n^(1/4)) et une complexité spatiale O(1).