```c++
// Programa principal

// Librerías necesarias
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <algorithm>

// Espacio de nombres estándar
using namespace std;

// Clase Persona
class Persona {
public:
    string nombre;
    int edad;

    Persona(string nombre, int edad) {
        this->nombre = nombre;
        this->edad = edad;
    }

    string getNombre() {
        return nombre;
    }

    int getEdad() {
        return edad;
    }
};

// Clase Empleado
class Empleado : public Persona {
public:
    string puesto;
    double salario;

    Empleado(string nombre, int edad, string puesto, double salario) : Persona(nombre, edad) {
        this->puesto = puesto;
        this->salario = salario;
    }

    string getPuesto() {
        return puesto;
    }

    double getSalario() {
        return salario;
    }
};

// Función principal
int main() {
    // Crear una lista de personas
    vector<Persona> personas = {
        Persona("Juan", 25),
        Persona("María", 30),
        Persona("Pedro", 35),
        Persona("Ana", 40)
    };

    // Crear un mapa de personas por nombre
    map<string, Persona> personasPorNombre;
    for (Persona persona : personas) {
        personasPorNombre[persona.getNombre()] = persona;
    }

    // Crear un conjunto de nombres de personas
    set<string> nombresPersonas;
    for (Persona persona : personas) {
        nombresPersonas.insert(persona.getNombre());
    }

    // Buscar una persona por nombre
    string nombreBuscado = "Juan";
    Persona personaBuscada = personasPorNombre[nombreBuscado];
    cout << "Persona buscada: " << personaBuscada.getNombre() << ", " << personaBuscada.getEdad() << endl;

    // Comprobar si una persona existe por nombre
    string nombreAComprobar = "Pedro";
    bool existePersona = nombresPersonas.find(nombreAComprobar) != nombresPersonas.end();
    cout << "¿Existe la persona con nombre '" << nombreAComprobar << "'? " << (existePersona ? "Sí" : "No") << endl;

    // Crear una lista de empleados
    vector<Empleado> empleados = {
        Empleado("Juan", 25, "Ingeniero", 2000.0),
        Empleado("María", 30, "Contadora", 2500.0),
        Empleado("Pedro", 35, "Gerente", 3000.0),
        Empleado("Ana", 40, "Directora", 3500.0)
    };

    // Crear un mapa de empleados por nombre
    map<string, Empleado> empleadosPorNombre;
    for (Empleado empleado : empleados) {
        empleadosPorNombre[empleado.getNombre()] = empleado;
    }

    // Crear un conjunto de nombres de empleados
    set<string> nombresEmpleados;
    for (Empleado empleado : empleados) {
        nombresEmpleados.insert(empleado.getNombre());
    }

    // Buscar un empleado por nombre
    string nombreEmpleadoBuscado = "Juan";
    Empleado empleadoBuscado = empleadosPorNombre[nombreEmpleadoBuscado];
    cout << "Empleado buscado: " << empleadoBuscado.getNombre() << ", " << empleadoBuscado.getEdad() << ", " << empleadoBuscado.getPuesto() << ", " << empleadoBuscado.getSalario() << endl;

    // Comprobar si un empleado existe por nombre
    string nombreEmpleadoAComprobar = "Pedro";
    bool existeEmpleado = nombresEmpleados.find(nombreEmpleadoAComprobar) != nombresEmpleados.end();
    cout << "¿Existe el empleado con nombre '" << nombreEmpleadoAComprobar << "'? " << (existeEmpleado ? "Sí" : "No") << endl;

    // Ordenar la lista de empleados por salario
    sort(empleados.begin(), empleados.end(), [](Empleado a, Empleado b) { return a.getSalario() < b.getSalario(); });

    // Imprimir la lista de empleados ordenada por salario
    cout << "Lista de empleados ordenada por salario:" << endl;
    for (Empleado empleado : empleados) {
        cout << empleado.getNombre() << ", " << empleado.getEdad() << ", " << empleado.getPuesto() << ", " << empleado.getSalario() << endl;
    }

    return 0;
}
```

**Explicación:**

* **Clase Persona:** Esta clase define la estructura básica de una persona, con atributos como el nombre y la edad.
* **Clase Empleado:** Esta clase hereda de la clase Persona y agrega atributos específicos de un empleado, como el puesto y el salario.
* **Función principal:** Esta función crea una lista de personas, un mapa de personas por nombre, un conjunto de nombres de personas, una lista de empleados, un mapa de empleados por nombre y un conjunto de nombres de empleados. Luego, busca una persona por nombre, comprueba si una persona existe por nombre, ordena la lista de empleados por salario e imprime la lista de empleados ordenada.