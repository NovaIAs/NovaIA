```C++
// Librerías necesarias
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <map>

using namespace std;

// Definir la estructura de un registro de empleado
struct Empleado {
    int id;
    string nombre;
    string apellido;
    string email;
    double salario;
};

// Función para comparar empleados por ID
bool compararEmpleadosPorID(Empleado emp1, Empleado emp2) {
    return emp1.id < emp2.id;
}

// Función para comparar empleados por salario
bool compararEmpleadosPorSalario(Empleado emp1, Empleado emp2) {
    return emp1.salario > emp2.salario;
}

// Función para imprimir la información de un empleado
void imprimirEmpleado(Empleado emp) {
    cout << "ID: " << emp.id << endl;
    cout << "Nombre: " << emp.nombre << endl;
    cout << "Apellido: " << emp.apellido << endl;
    cout << "Email: " << emp.email << endl;
    cout << "Salario: " << emp.salario << endl;
    cout << endl;
}

// Función para leer los datos de los empleados desde un archivo CSV
vector<Empleado> leerEmpleadosDesdeCSV(string nombreArchivo) {
    ifstream archivo(nombreArchivo);
    vector<Empleado> empleados;

    string linea;
    while (getline(archivo, linea)) {
        stringstream ss(linea);
        string campo;

        Empleado emp;

        getline(ss, campo, ',');
        emp.id = stoi(campo);

        getline(ss, campo, ',');
        emp.nombre = campo;

        getline(ss, campo, ',');
        emp.apellido = campo;

        getline(ss, campo, ',');
        emp.email = campo;

        getline(ss, campo, ',');
        emp.salario = stod(campo);

        empleados.push_back(emp);
    }

    archivo.close();

    return empleados;
}

// Función para guardar los datos de los empleados en un archivo CSV
void guardarEmpleadosEnCSV(string nombreArchivo, vector<Empleado> empleados) {
    ofstream archivo(nombreArchivo);

    for (Empleado emp : empleados) {
        archivo << emp.id << "," << emp.nombre << "," << emp.apellido << "," << emp.email << "," << emp.salario << endl;
    }

    archivo.close();
}

// Función para buscar un empleado por su ID
Empleado buscarEmpleadoPorID(vector<Empleado> empleados, int id) {
    Empleado empleado;

    for (Empleado emp : empleados) {
        if (emp.id == id) {
            empleado = emp;
            break;
        }
    }

    return empleado;
}

// Función para eliminar un empleado por su ID
void eliminarEmpleadoPorID(vector<Empleado>& empleados, int id) {
    for (int i = 0; i < empleados.size(); i++) {
        if (empleados[i].id == id) {
            empleados.erase(empleados.begin() + i);
            break;
        }
    }
}

// Función para actualizar el salario de un empleado por su ID
void actualizarSalarioEmpleadoPorID(vector<Empleado>& empleados, int id, double nuevoSalario) {
    for (int i = 0; i < empleados.size(); i++) {
        if (empleados[i].id == id) {
            empleados[i].salario = nuevoSalario;
            break;
        }
    }
}

// Función para calcular el salario total de los empleados
double calcularSalarioTotal(vector<Empleado> empleados) {
    double salarioTotal = 0;

    for (Empleado emp : empleados) {
        salarioTotal += emp.salario;
    }

    return salarioTotal;
}

// Función para calcular el salario promedio de los empleados
double calcularSalarioPromedio(vector<Empleado> empleados) {
    double salarioTotal = calcularSalarioTotal(empleados);
    double salarioPromedio = salarioTotal / empleados.size();

    return salarioPromedio;
}

// Función para encontrar al empleado con el salario más alto
Empleado encontrarEmpleadoConSalarioMasAlto(vector<Empleado> empleados) {
    Empleado empleadoConSalarioMasAlto;

    for (Empleado emp : empleados) {
        if (emp.salario > empleadoConSalarioMasAlto.salario) {
            empleadoConSalarioMasAlto = emp;
        }
    }

    return empleadoConSalarioMasAlto;
}

// Función para encontrar al empleado con el salario más bajo
Empleado encontrarEmpleadoConSalarioMasBajo(vector<Empleado> empleados) {
    Empleado empleadoConSalarioMasBajo;

    for (Empleado emp : empleados) {
        if (emp.salario < empleadoConSalarioMasBajo.salario) {
            empleadoConSalarioMasBajo = emp;
        }
    }

    return empleadoConSalarioMasBajo;
}

// Función para agrupar a los empleados por departamento