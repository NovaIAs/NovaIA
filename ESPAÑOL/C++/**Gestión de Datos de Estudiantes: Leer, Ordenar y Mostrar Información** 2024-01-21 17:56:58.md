```c++
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include <algorithm>
#include <iterator>

using namespace std;

// Definición de la estructura de datos para almacenar los datos de los estudiantes
struct Estudiante {
    string nombre;
    string apellido;
    int edad;
    float nota;
};

// Función para leer los datos de los estudiantes desde un archivo de texto
vector<Estudiante> leerEstudiantes(string nombreArchivo) {
    vector<Estudiante> estudiantes;
    ifstream archivo(nombreArchivo);

    if (archivo.is_open()) {
        string linea;
        while (getline(archivo, linea)) {
            stringstream ss(linea);
            Estudiante estudiante;
            ss >> estudiante.nombre >> estudiante.apellido >> estudiante.edad >> estudiante.nota;
            estudiantes.push_back(estudiante);
        }
        archivo.close();
    } else {
        cout << "Error al abrir el archivo " << nombreArchivo << endl;
    }

    return estudiantes;
}

// Función para imprimir los datos de los estudiantes en la consola
void imprimirEstudiantes(vector<Estudiante> estudiantes) {
    cout << "Listado de estudiantes:" << endl;
    for (Estudiante estudiante : estudiantes) {
        cout << estudiante.nombre << " " << estudiante.apellido << ", " << estudiante.edad << " años, nota: " << estudiante.nota << endl;
    }
}

// Función para ordenar los estudiantes por nombre
bool compararPorNombre(Estudiante a, Estudiante b) {
    return a.nombre < b.nombre;
}

// Función para ordenar los estudiantes por apellido
bool compararPorApellido(Estudiante a, Estudiante b) {
    return a.apellido < b.apellido;
}

// Función para ordenar los estudiantes por edad
bool compararPorEdad(Estudiante a, Estudiante b) {
    return a.edad < b.edad;
}

// Función para ordenar los estudiantes por nota
bool compararPorNota(Estudiante a, Estudiante b) {
    return a.nota < b.nota;
}

// Función para obtener el estudiante con la nota más alta
Estudiante obtenerMejorEstudiante(vector<Estudiante> estudiantes) {
    Estudiante mejorEstudiante;
    float mejorNota = 0;
    for (Estudiante estudiante : estudiantes) {
        if (estudiante.nota > mejorNota) {
            mejorNota = estudiante.nota;
            mejorEstudiante = estudiante;
        }
    }
    return mejorEstudiante;
}

// Función para obtener el estudiante con la nota más baja
Estudiante obtenerPeorEstudiante(vector<Estudiante> estudiantes) {
    Estudiante peorEstudiante;
    float peorNota = 10;
    for (Estudiante estudiante : estudiantes) {
        if (estudiante.nota < peorNota) {
            peorNota = estudiante.nota;
            peorEstudiante = estudiante;
        }
    }
    return peorEstudiante;
}

// Función para calcular el promedio de las notas de los estudiantes
float calcularPromedioNotas(vector<Estudiante> estudiantes) {
    float promedio = 0;
    for (Estudiante estudiante : estudiantes) {
        promedio += estudiante.nota;
    }
    promedio /= estudiantes.size();
    return promedio;
}

// Función principal
int main() {
    // Leer los datos de los estudiantes desde el archivo "estudiantes.txt"
    vector<Estudiante> estudiantes = leerEstudiantes("estudiantes.txt");

    // Imprimir los datos de los estudiantes en la consola
    imprimirEstudiantes(estudiantes);

    // Ordenar los estudiantes por nombre
    sort(estudiantes.begin(), estudiantes.end(), compararPorNombre);

    // Imprimir los datos de los estudiantes ordenados por nombre
    cout << endl << "Estudiantes ordenados por nombre:" << endl;
    imprimirEstudiantes(estudiantes);

    // Ordenar los estudiantes por apellido
    sort(estudiantes.begin(), estudiantes.end(), compararPorApellido);

    // Imprimir los datos de los estudiantes ordenados por apellido
    cout << endl << "Estudiantes ordenados por apellido:" << endl;
    imprimirEstudiantes(estudiantes);

    // Ordenar los estudiantes por edad
    sort(estudiantes.begin(), estudiantes.end(), compararPorEdad);

    // Imprimir los datos de los estudiantes ordenados por edad
    cout << endl << "Estudiantes ordenados por edad:" << endl;
    imprimirEstudiantes(estudiantes);

    // Ordenar los estudiantes por nota
    sort(estudiantes.begin(), estudiantes.end(), compararPorNota);

    // Imprimir los datos de los estudiantes ordenados por nota
    cout << endl << "Estudiantes ordenados por nota:" << endl;
    imprimirEstudiantes(estudiantes);

    // Obtener el estudiante con la nota más alta
    Estudiante mejorEstudiante = obtenerMejorEstudiante(estudiantes);

    // Imprimir los datos del estudiante con la nota más alta
    cout << endl << "Mejor estudiante:" << endl;
    cout << mejorEstudiante.nombre << " " << mejorEstudiante.apellido << ", " << mejorEstudiante.edad << " años, nota: " << mejorEstudiante.nota << endl;

    // Obtener el estudiante con la nota más baja
    Estudiante peorEstudiante = obtenerPeorEstudiante(estudiantes);

    // Imprimir los datos del estudiante con la nota más baja
    cout << endl << "Peor estudiante:" << endl;
    cout << peorEstudiante.nombre << " " << peorEstudiante.apellido << ", " << peorEstudiante.edad << " años, nota: " << peorEstudiante.nota << endl;

    // Calcular el promedio de las notas de los estudiantes
    float promedioNotas = calcularPromedioNotas(estudiantes);

    // Imp