```c++
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <string>

using namespace std;

// Función para comparar dos cadenas por longitud
bool compararLongitudCadenas(const string& a, const string& b) {
    return a.length() < b.length();
}

// Función para contar el número de apariciones de cada palabra en un texto
map<string, int> contarPalabras(string texto) {
    map<string, int> conteoPalabras;

    // Convertir el texto a minúsculas
    transform(texto.begin(), texto.end(), texto.begin(), ::tolower);

    // Dividir el texto en palabras
    vector<string> palabras;
    stringstream ss(texto);
    string palabra;
    while (ss >> palabra) {
        palabras.push_back(palabra);
    }

    // Contar el número de apariciones de cada palabra
    for (string palabra : palabras) {
        conteoPalabras[palabra]++;
    }

    return conteoPalabras;
}

// Función para encontrar las palabras más largas en un texto
vector<string> encontrarPalabrasMasLargas(string texto) {
    // Convertir el texto a minúsculas
    transform(texto.begin(), texto.end(), texto.begin(), ::tolower);

    // Dividir el texto en palabras
    vector<string> palabras;
    stringstream ss(texto);
    string palabra;
    while (ss >> palabra) {
        palabras.push_back(palabra);
    }

    // Ordenar las palabras por longitud
    sort(palabras.begin(), palabras.end(), compararLongitudCadenas);

    // Encontrar las palabras más largas
    int longitudMaxima = palabras.back().length();
    vector<string> palabrasMasLargas;
    for (string palabra : palabras) {
        if (palabra.length() == longitudMaxima) {
            palabrasMasLargas.push_back(palabra);
        }
    }

    return palabrasMasLargas;
}

// Función para encontrar las palabras más repetidas en un texto
vector<string> encontrarPalabrasMasRepetidas(string texto) {
    // Contar el número de apariciones de cada palabra
    map<string, int> conteoPalabras = contarPalabras(texto);

    // Encontrar las palabras más repetidas
    int frecuenciaMaxima = 0;
    vector<string> palabrasMasRepetidas;
    for (auto par : conteoPalabras) {
        if (par.second > frecuenciaMaxima) {
            frecuenciaMaxima = par.second;
            palabrasMasRepetidas.clear();
            palabrasMasRepetidas.push_back(par.first);
        } else if (par.second == frecuenciaMaxima) {
            palabrasMasRepetidas.push_back(par.first);
        }
    }

    return palabrasMasRepetidas;
}

// Función principal
int main() {
    // Obtener el texto del usuario
    string texto;
    cout << "Introduce el texto: ";
    getline(cin, texto);

    // Contar el número de apariciones de cada palabra
    map<string, int> conteoPalabras = contarPalabras(texto);

    // Encontrar las palabras más largas
    vector<string> palabrasMasLargas = encontrarPalabrasMasLargas(texto);

    // Encontrar las palabras más repetidas
    vector<string> palabrasMasRepetidas = encontrarPalabrasMasRepetidas(texto);

    // Mostrar los resultados
    cout << "Palabras más largas: ";
    for (string palabra : palabrasMasLargas) {
        cout << palabra << " ";
    }
    cout << endl;

    cout << "Palabras más repetidas: ";
    for (string palabra : palabrasMasRepetidas) {
        cout << palabra << " ";
    }
    cout << endl;

    return 0;
}
```

Explicación del código:

* La función `compararLongitudCadenas` compara dos cadenas por longitud.

* La función `contarPalabras` cuenta el número de apariciones de cada palabra en un texto.

* La función `encontrarPalabrasMasLargas` encuentra las palabras más largas en un texto.

* La función `encontrarPalabrasMasRepetidas` encuentra las palabras más repetidas en un texto.

* La función `main` es la función principal del programa. Obtiene el texto del usuario, cuenta el número de apariciones de cada palabra, encuentra las palabras más largas y las palabras más repetidas, y muestra los resultados.