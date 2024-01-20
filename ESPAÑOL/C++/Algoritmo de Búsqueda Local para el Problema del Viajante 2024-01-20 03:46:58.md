```c++
// Este programa resuelve el problema del viajante, que consiste en encontrar el camino más corto que pasa por todas las ciudades una sola vez.

#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

// Clase que representa una ciudad.
class Ciudad {
public:
    Ciudad(double x, double y) : x(x), y(y) {}

    double x;
    double y;
};

// Clase que representa una solución del problema del viajante.
class Solucion {
public:
    Solucion(vector<Ciudad> ciudades) : ciudades(ciudades) {}

    vector<Ciudad> ciudades;
    double distancia_total = 0;

    // Calcula la distancia total de la solución.
    void calcular_distancia_total() {
        // Se debe recorrer el vector ciudades, pero de 2 en 2.
        for (unsigned int i = 0; i < ciudades.size() - 1; i++) {
            Ciudad ciudad1 = ciudades[i];
            Ciudad ciudad2 = ciudades[i + 1];

            // Se calcula la distancia entre 2 ciudades utilizando la fórmula de la distancia euclidiana.
            distancia_total += sqrt(pow(ciudad1.x - ciudad2.x, 2) + pow(ciudad1.y - ciudad2.y, 2));
        }
    }

    // Imprime la solución.
    void imprimir() {
        for (unsigned int i = 0; i < ciudades.size(); i++) {
            cout << "(" << ciudades[i].x << ", " << ciudades[i].y << ") ";
        }

        cout << "\nDistancia total: " << distancia_total << endl;
    }
};

// Función que genera una solución aleatoria del problema del viajante.
Solucion generar_solucion_aleatoria(vector<Ciudad> ciudades) {
    // Se crea un vector de ciudades aleatorias.
    random_shuffle(ciudades.begin(), ciudades.end());

    // Se crea una solución con las ciudades aleatorias.
    Solucion solucion(ciudades);

    // Se calcula la distancia total de la solución.
    solucion.calcular_distancia_total();

    return solucion;
}

// Función que aplica el algoritmo de búsqueda local al problema del viajante.
Solucion busqueda_local(Solucion solucion_inicial) {
    // Se crea una solución inicial.
    Solucion solucion_actual = solucion_inicial;

    // Se itera hasta que no haya mejora en la solución.
    while (true) {
        // Se crean dos listas de ciudades para almacenar las ciudades que se pueden intercambiar.
        vector<Ciudad> ciudades_intercambiables_1;
        vector<Ciudad> ciudades_intercambiables_2;

        // Se recorren las ciudades de la solución actual.
        for (unsigned int i = 0; i < solucion_actual.ciudades.size(); i++) {
            // Se crea una lista de las ciudades que se pueden intercambiar con la ciudad actual.
            for (unsigned int j = i + 1; j < solucion_actual.ciudades.size(); j++) {
                ciudades_intercambiables_1.push_back(solucion_actual.ciudades[i]);
                ciudades_intercambiables_2.push_back(solucion_actual.ciudades[j]);
            }
        }

        // Se intercambian las ciudades que producen la mayor mejora en la solución.
        double mejor_mejora = 0;
        int mejor_indice_1 = -1;
        int mejor_indice_2 = -1;

        // Se recorren las listas de ciudades que se pueden intercambiar.
        for (unsigned int i = 0; i < ciudades_intercambiables_1.size(); i++) {
            for (unsigned int j = 0; j < ciudades_intercambiables_2.size(); j++) {
                // Se crea una nueva solución con las ciudades intercambiadas.
                Solucion nueva_solucion = solucion_actual;
                nueva_solucion.ciudades[solucion_actual.ciudades.find(ciudades_intercambiables_1[i])] = ciudades_intercambiables_2[j];
                nueva_solucion.ciudades[solucion_actual.ciudades.find(ciudades_intercambiables_2[j])] = ciudades_intercambiables_1[i];

                // Se calcula la distancia total de la nueva solución.
                nueva_solucion.calcular_distancia_total();

                // Si la nueva solución es mejor que la actual, se guarda la mejora y los índices de las ciudades intercambiadas.
                if (nueva_solucion.distancia_total < solucion_actual.distancia_total) {
                    mejor_mejora = solucion_actual.distancia_total - nueva_solucion.distancia_total;
                    mejor_indice_1 = i;
                    mejor_indice_2 = j;
                }
            }
        }

        // Si no se encontró ninguna mejora, se termina la búsqueda local.
        if (mejor_mejora == 0) {
            break;
        }

        // Se intercambian las ciudades que producen la mayor mejora en la solución.
        solucion_actual.ciudades[solucion_actual.ciudades.find(ciudades_intercambiables_1[mejor_indice_1])] = ciudades_intercambiables_2[mejor_indice_2];
        solucion_actual.ciudades[solucion_actual.ciudades.find(ciudades_intercambiables_2[mejor_indice_2])] = ciudades_intercambiables_1[mejor_indice_1];

        // Se calcula la distancia total de la solución actual.
        solucion_actual.calcular_distancia_total();
    }

    return solucion_actual;
}

// Función principal del programa.
int main() {
    // Se crean las ciudades del problema.
    vector<Ciudad> ciudades = {
        Ciudad(0, 0),
        Ciudad(10, 0),
        Ciudad(20, 0),
        Ciudad(30, 0),
        Ciudad(40, 0)
    };

    // Se genera una solución aleatoria.
    Solucion solucion_aleatoria = generar_solucion_aleatoria(ciudades);

    // Se aplica el algoritmo de búsqueda local a la solución aleatoria.
    Solucion solucion_local = busqueda_local(solucion_aleatoria);

    // Se imprime la solución local.
    cout << "Solución local:" << endl;
    solucion_local.imprimir();

    return 0;