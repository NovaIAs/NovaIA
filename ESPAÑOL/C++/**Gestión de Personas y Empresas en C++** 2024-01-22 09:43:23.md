```c++
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>

using namespace std;

class Persona {
public:
    string nombre;
    int edad;
    string ciudad;

    Persona(string nombre, int edad, string ciudad) {
        this->nombre = nombre;
        this->edad = edad;
        this->ciudad = ciudad;
    }

    void imprimir() {
        cout << "Nombre: " << nombre << endl;
        cout << "Edad: " << edad << endl;
        cout << "Ciudad: " << ciudad << endl;
    }
};

class Empresa {
public:
    string nombre;
    string direccion;
    vector<Persona> empleados;

    Empresa(string nombre, string direccion) {
        this->nombre = nombre;
        this->direccion = direccion;
    }

    void agregarEmpleado(Persona empleado) {
        empleados.push_back(empleado);
    }

    void imprimir() {
        cout << "Nombre: " << nombre << endl;
        cout << "Direccion: " << direccion << endl;
        cout << "Empleados:" << endl;
        for (Persona empleado : empleados) {
            empleado.imprimir();
        }
    }
};

int main() {
    // Crear personas
    Persona juan("Juan Perez", 25, "Madrid");
    Persona maria("Maria Gonzalez", 30, "Barcelona");
    Persona pedro("Pedro Sanchez", 40, "Valencia");

    // Crear empresa
    Empresa empresa("ACME", "Calle Mayor 123");

    // Agregar empleados a la empresa
    empresa.agregarEmpleado(juan);
    empresa.agregarEmpleado(maria);
    empresa.agregarEmpleado(pedro);

    // Imprimir la empresa
    empresa.imprimir();

    // Crear un mapa de personas por ciudad
    map<string, vector<Persona>> personasPorCiudad;

    // Agregar las personas al mapa por ciudad
    personasPorCiudad["Madrid"].push_back(juan);
    personasPorCiudad["Barcelona"].push_back(maria);
    personasPorCiudad["Valencia"].push_back(pedro);

    // Imprimir el mapa de personas por ciudad
    for (auto it = personasPorCiudad.begin(); it != personasPorCiudad.end(); ++it) {
        cout << "Ciudad: " << it->first << endl;
        for (Persona persona : it->second) {
            persona.imprimir();
        }
    }

    // Ordenar el mapa de personas por ciudad por nombre
    map<string, vector<Persona>> personasPorCiudadOrdenadas;
    for (auto it = personasPorCiudad.begin(); it != personasPorCiudad.end(); ++it) {
        sort(it->second.begin(), it->second.end(), [](Persona a, Persona b) { return a.nombre < b.nombre; });
        personasPorCiudadOrdenadas[it->first] = it->second;
    }

    // Imprimir el mapa de personas por ciudad ordenado por nombre
    for (auto it = personasPorCiudadOrdenadas.begin(); it != personasPorCiudadOrdenadas.end(); ++it) {
        cout << "Ciudad: " << it->first << endl;
        for (Persona persona : it->second) {
            persona.imprimir();
        }
    }

    return 0;
}
```

Explicación del código:

1. Primero, creamos tres objetos de la clase `Persona` que representan a tres personas diferentes: Juan, María y Pedro.
2. A continuación, creamos un objeto de la clase `Empresa` que representa a la empresa ACME.
3. Añadimos los objetos `Persona` a la empresa utilizando el método `agregarEmpleado`.
4. Imprimimos la empresa utilizando el método `imprimir`.
5. Creamos un mapa de personas por ciudad utilizando la clase `map`.
6. Añadimos las personas al mapa por ciudad utilizando la clave de la ciudad y el valor de una lista de personas.
7. Imprimimos el mapa de personas por ciudad.
8. Ordenamos el mapa de personas por ciudad por nombre utilizando la función `sort` y la función lambda `[](Persona a, Persona b) { return a.nombre < b.nombre; }`.
9. Imprimimos el mapa de personas por ciudad ordenado por nombre.