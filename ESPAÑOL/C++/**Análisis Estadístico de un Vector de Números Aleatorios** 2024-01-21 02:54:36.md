```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <random>

using namespace std;

// Función para generar un vector de números aleatorios
vector<int> generar_vector_aleatorio(int tamaño, int mínimo, int máximo) {
    // Creamos un objeto de tipo generador de números aleatorios
    random_device rd;
    mt19937 gen(rd());
    uniform_int_distribution<int> dist(mínimo, máximo);

    // Creamos el vector de números aleatorios
    vector<int> vector_aleatorio(tamaño);
    for (int i = 0; i < tamaño; i++) {
        vector_aleatorio[i] = dist(gen);
    }

    return vector_aleatorio;
}

// Función para calcular la media de un vector de números
float calcular_media(vector<int> vector) {
    // Sumamos todos los elementos del vector
    int suma = accumulate(vector.begin(), vector.end(), 0);

    // Calculamos la media dividiendo la suma por el tamaño del vector
    float media = (float)suma / vector.size();

    return media;
}

// Función para calcular la desviación estándar de un vector de números
float calcular_desviacion_estandar(vector<int> vector) {
    // Calculamos la media del vector
    float media = calcular_media(vector);

    // Calculamos la varianza del vector
    float varianza = 0;
    for (int i = 0; i < vector.size(); i++) {
        varianza += pow(vector[i] - media, 2);
    }
    varianza /= vector.size();

    // Calculamos la desviación estándar como la raíz cuadrada de la varianza
    float desviacion_estandar = sqrt(varianza);

    return desviacion_estandar;
}

// Función para ordenar un vector de números
void ordenar_vector(vector<int> &vector) {
    // Utilizamos la función sort de la biblioteca estándar de C++ para ordenar el vector
    sort(vector.begin(), vector.end());
}

// Función para eliminar los elementos duplicados de un vector
void eliminar_duplicados(vector<int> &vector) {
    // Utilizamos la función unique de la biblioteca estándar de C++ para eliminar los elementos duplicados del vector
    vector.erase(unique(vector.begin(), vector.end()), vector.end());
}

// Función para imprimir un vector de números
void imprimir_vector(vector<int> vector) {
    // Recorremos el vector y imprimimos cada elemento
    for (int i = 0; i < vector.size(); i++) {
        cout << vector[i] << " ";
    }
    cout << endl;
}

int main() {
    // Generamos un vector de 10 números aleatorios entre 1 y 100
    vector<int> vector_aleatorio = generar_vector_aleatorio(10, 1, 100);

    // Imprimimos el vector de números aleatorios
    cout << "Vector de números aleatorios:" << endl;
    imprimir_vector(vector_aleatorio);

    // Calculamos la media del vector
    float media = calcular_media(vector_aleatorio);

    // Imprimimos la media del vector
    cout << "Media del vector: " << media << endl;

    // Calculamos la desviación estándar del vector
    float desviacion_estandar = calcular_desviacion_estandar(vector_aleatorio);

    // Imprimimos la desviación estándar del vector
    cout << "Desviación estándar del vector: " << desviacion_estandar << endl;

    // Ordenamos el vector
    ordenar_vector(vector_aleatorio);

    // Imprimimos el vector ordenado
    cout << "Vector ordenado:" << endl;
    imprimir_vector(vector_aleatorio);

    // Eliminamos los elementos duplicados del vector
    eliminar_duplicados(vector_aleatorio);

    // Imprimimos el vector sin elementos duplicados
    cout << "Vector sin elementos duplicados:" << endl;
    imprimir_vector(vector_aleatorio);

    return 0;
}
```

Explicación del código:

* La función `generar_vector_aleatorio` genera un vector de números aleatorios utilizando el objeto de tipo generador de números aleatorios `random_device` y la distribución uniforme `uniform_int_distribution`.
* La función `calcular_media` calcula la media de un vector de números sumando todos los elementos del vector y dividiendo la suma por el tamaño del vector.
* La función `calcular_desviacion_estandar` calcula la desviación estándar de un vector de números calculando primero la media del vector y luego la varianza del vector. La desviación estándar se calcula como la raíz cuadrada de la varianza.
* La función `ordenar_vector` ordena un vector de números utilizando la función `sort` de la biblioteca estándar de C++.
* La función `eliminar_duplicados` elimina los elementos duplicados de un vector utilizando la función `unique` de la biblioteca estándar de C++.
* La función `imprimir_vector` imprime un vector de números en la consola.
* La función `main` es la función principal del programa. En esta función, se genera un vector de 10 números aleatorios entre 1 y 100, se calculan la media y la desviación estándar del vector, se ordena el vector, se eliminan los elementos duplicados del vector y se imprime el vector en la consola.