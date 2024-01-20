```c++
// Biblioteca principal del lenguaje C++
#include <iostream>
// Biblioteca para la entrada y salida estándar
#include <cstdio>
// Biblioteca para el manejo de archivos
#include <fstream>
// Biblioteca para el manejo de cadenas
#include <string>
// Biblioteca para el manejo de contenedores
#include <vector>
// Biblioteca para el manejo de algoritmos
#include <algorithm>
// Biblioteca para el manejo de expresiones regulares
#include <regex>

// Declaración del espacio de nombres estándar
using namespace std;

// Definición de constantes
const int MAX_SIZE = 100;
const string FILE_NAME = "data.txt";

// Definición de estructuras y clases
struct Producto {
    string nombre;
    double precio;
    int cantidad;
};

class Tienda {
private:
    vector<Producto> productos;

public:
    void agregarProducto(Producto producto) {
        productos.push_back(producto);
    }

    void eliminarProducto(string nombre) {
        for (vector<Producto>::iterator it = productos.begin(); it != productos.end(); ++it) {
            if (it->nombre == nombre) {
                productos.erase(it);
                break;
            }
        }
    }

    Producto buscarProducto(string nombre) {
        for (vector<Producto>::iterator it = productos.begin(); it != productos.end(); ++it) {
            if (it->nombre == nombre) {
                return *it;
            }
        }

        return Producto();
    }

    void mostrarProductos() {
        cout << "Productos disponibles:" << endl;
        for (vector<Producto>::iterator it = productos.begin(); it != productos.end(); ++it) {
            cout << "Nombre: " << it->nombre << ", Precio: " << it->precio << ", Cantidad: " << it->cantidad << endl;
        }
    }

    double calcularTotal() {
        double total = 0;
        for (vector<Producto>::iterator it = productos.begin(); it != productos.end(); ++it) {
            total += it->precio * it->cantidad;
        }

        return total;
    }
};

// Definición de funciones
bool leerArchivo(string nombreArchivo, vector<Producto>& productos) {
    ifstream archivo;

    archivo.open(nombreArchivo);

    if (archivo.is_open()) {
        string linea;

        while (getline(archivo, linea)) {
            regex patron("([^,]+),([0-9.]+),([0-9]+)");
            smatch resultados;

            if (regex_match(linea, resultados, patron)) {
                productos.push_back({ resultados[1].str(), stod(resultados[2].str()), stoi(resultados[3].str()) });
            }
        }

        archivo.close();

        return true;
    } else {
        return false;
    }
}

bool escribirArchivo(string nombreArchivo, vector<Producto>& productos) {
    ofstream archivo;

    archivo.open(nombreArchivo);

    if (archivo.is_open()) {
        for (vector<Producto>::iterator it = productos.begin(); it != productos.end(); ++it) {
            archivo << it->nombre << "," << it->precio << "," << it->cantidad << endl;
        }

        archivo.close();

        return true;
    } else {
        return false;
    }
}

// Función principal
int main() {
    // Declaración de variables
    Tienda tienda;
    vector<Producto> productos;

    // Leer los productos del archivo
    if (leerArchivo(FILE_NAME, productos)) {
        for (vector<Producto>::iterator it = productos.begin(); it != productos.end(); ++it) {
            tienda.agregarProducto(*it);
        }
    } else {
        cout << "Error al leer el archivo." << endl;
    }

    // Mostrar los productos disponibles
    cout << "==================================================================" << endl;
    cout << "Productos disponibles:" << endl;
    cout << "==================================================================" << endl;
    tienda.mostrarProductos();
    cout << endl;

    // Agregar un nuevo producto
    Producto producto1 = { "iPhone 14", 1000, 5 };
    tienda.agregarProducto(producto1);

    // Eliminar un producto
    tienda.eliminarProducto("Samsung Galaxy S22");

    // Buscar un producto
    Producto producto2 = tienda.buscarProducto("iPhone 13");

    if (producto2.nombre != "") {
        cout << "==================================================================" << endl;
        cout << "Producto encontrado:" << endl;
        cout << "==================================================================" << endl;
        cout << "Nombre: " << producto2.nombre << ", Precio: " << producto2.precio << ", Cantidad: " << producto2.cantidad << endl;
        cout << endl;
    } else {
        cout << "==================================================================" << endl;
        cout << "Producto no encontrado." << endl;