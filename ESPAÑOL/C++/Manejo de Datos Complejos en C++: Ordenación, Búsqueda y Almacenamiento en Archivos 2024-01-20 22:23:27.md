```c++
// Importar las librerías necesarias
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>
#include <functional>
#include <fstream>

// Definir el tipo de datos para los elementos del vector
struct Elemento {
    int valor;
    std::string nombre;
};

// Función para ordenar el vector de elementos por valor
bool ordenarElementosPorValor(const Elemento& a, const Elemento& b) {
    return a.valor < b.valor;
}

// Función para ordenar el vector de elementos por nombre
bool ordenarElementosPorNombre(const Elemento& a, const Elemento& b) {
    return a.nombre < b.nombre;
}

// Función para buscar un elemento en el vector por valor
Elemento* buscarElementoPorValor(const std::vector<Elemento>& vector, int valor) {
    for (Elemento& elemento : vector) {
        if (elemento.valor == valor) {
            return &elemento;
        }
    }

    return nullptr;
}

// Función para buscar un elemento en el vector por nombre
Elemento* buscarElementoPorNombre(const std::vector<Elemento>& vector, const std::string& nombre) {
    for (Elemento& elemento : vector) {
        if (elemento.nombre == nombre) {
            return &elemento;
        }
    }

    return nullptr;
}

// Función para leer los datos de un archivo de texto y almacenarlos en un vector
std::vector<Elemento> leerDatosDeArchivo(const std::string& nombreArchivo) {
    std::ifstream archivo(nombreArchivo);
    if (!archivo.is_open()) {
        throw std::runtime_error("No se pudo abrir el archivo " + nombreArchivo);
    }

    std::vector<Elemento> vector;
    std::string linea;
    while (std::getline(archivo, linea)) {
        std::stringstream ss(linea);
        int valor;
        std::string nombre;
        ss >> valor >> nombre;
        vector.push_back({valor, nombre});
    }

    archivo.close();
    return vector;
}

// Función para escribir los datos de un vector en un archivo de texto
void escribirDatosEnArchivo(const std::string& nombreArchivo, const std::vector<Elemento>& vector) {
    std::ofstream archivo(nombreArchivo);
    if (!archivo.is_open()) {
        throw std::runtime_error("No se pudo abrir el archivo " + nombreArchivo);
    }

    for (const Elemento& elemento : vector) {
        archivo << elemento.valor << " " << elemento.nombre << std::endl;
    }

    archivo.close();
}

// Función principal
int main() {
    // Leer los datos de un archivo de texto
    std::vector<Elemento> vector = leerDatosDeArchivo("datos.txt");

    // Ordenar el vector por valor
    std::sort(vector.begin(), vector.end(), ordenarElementosPorValor);

    // Buscar un elemento en el vector por valor
    int valorABuscar = 10;
    Elemento* elementoEncontrado = buscarElementoPorValor(vector, valorABuscar);
    if (elementoEncontrado != nullptr) {
        std::cout << "Elemento encontrado: " << elementoEncontrado->valor << " " << elementoEncontrado->nombre << std::endl;
    } else {
        std::cout << "Elemento no encontrado" << std::endl;
    }

    // Ordenar el vector por nombre
    std::sort(vector.begin(), vector.end(), ordenarElementosPorNombre);

    // Buscar un elemento en el vector por nombre
    std::string nombreABuscar = "Juan";
    Elemento* elementoEncontrado2 = buscarElementoPorNombre(vector, nombreABuscar);
    if (elementoEncontrado2 != nullptr) {
        std::cout << "Elemento encontrado: " << elementoEncontrado2->valor << " " << elementoEncontrado2->nombre << std::endl;
    } else {
        std::cout << "Elemento no encontrado" << std::endl;
    }

    // Escribir los datos del vector en un archivo de texto
    escribirDatosEnArchivo("salida.txt", vector);

    return 0;
}
```

Este código es un ejemplo de un programa complejo en C++ que realiza las siguientes tareas:

* Lee los datos de un archivo de texto y los almacena en un vector de elementos.
* Ordena el vector por valor.
* Busca un elemento en el vector por valor.
* Ordena el vector por nombre.
* Busca un elemento en el vector por nombre.
* Escribe los datos del vector en un archivo de texto.

El código utiliza las siguientes técnicas:

* Vectores para almacenar los datos.
* Funciones para ordenar y buscar elementos en el vector.
* Archivos de texto para leer y escribir los datos.
* Excepciones para manejar los errores.

El código está bien documentado y es fácil de entender y seguir.