**Programme de calcul de la transformée de Fourier discrète en C++**

**Explications :**

Ce programme calcule la transformée de Fourier discrète (TFD) d'un signal temporel discret à l'aide de l'algorithme de Cooley-Tukey (FFT). La TFD est un outil puissant utilisé pour analyser les signaux dans le domaine fréquentiel.

**Code :**

```cpp
#include <complex>
#include <vector>
#include <cmath>

using std::vector;
using std::complex;

vector<complex<double>> fft(const vector<double>& signal) {
    // Obtenir la longueur du signal
    int n = signal.size();

    // Cas de base : signal de longueur 1
    if (n == 1) {
        return { signal[0] };
    }

    // Décomposer le signal en deux sous-signaux de longueur n/2
    vector<double> p1(n / 2), p2(n / 2);
    for (int i = 0; i < n / 2; i++) {
        p1[i] = signal[2 * i];
        p2[i] = signal[2 * i + 1];
    }

    // Calculer la TFD des sous-signaux
    vector<complex<double>> y1 = fft(p1);
    vector<complex<double>> y2 = fft(p2);

    // Calculer les twiddle factors (facteurs de rotation)
    vector<complex<double>> W(n / 2);
    for (int i = 0; i < n / 2; i++) {
        W[i] = polar(1.0, -2.0 * M_PI * i / n);
    }

    // Combiner les TFD des sous-signaux
    vector<complex<double>> Y(n);
    for (int i = 0; i < n / 2; i++) {
        Y[i] = y1[i] + W[i] * y2[i];
        Y[i + n / 2] = y1[i] - W[i] * y2[i];
    }

    return Y;
}

int main() {
    // Signal temporel discret
    vector<double> signal = { 1, 2, 3, 4, 5, 6, 7, 8 };

    // Calculer la TFD
    vector<complex<double>> Y = fft(signal);

    // Afficher les amplitudes des composantes fréquentielles
    for (const auto& y : Y) {
        cout << abs(y) << endl;
    }

    return 0;
}
```