```c++
// Importación de librerías
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

// Definición de una clase Persona con atributos (nombre y edad) y métodos (constructor y método para obtener la edad)
class Persona {
public:
    std::string nombre;
    int edad;

    Persona(std::string nombre, int edad) : nombre(nombre), edad(edad) {}

    int obtenerEdad() {
        return edad;
    }
};

// Definición de una clase Estudiante que hereda de la clase Persona y añade el atributo "calificación"
class Estudiante : public Persona {
public:
    float calificacion;

    Estudiante(std::string nombre, int edad, float calificacion) : Persona(nombre, edad), calificacion(calificacion) {}
};

// Función para imprimir los datos de una persona
void imprimirPersona(Persona persona) {
    std::cout << "Nombre: " << persona.nombre << std::endl;
    std::cout << "Edad: " << persona.obtenerEdad() << std::endl;
}

// Función para imprimir los datos de un estudiante
void imprimirEstudiante(Estudiante estudiante) {
    imprimirPersona(estudiante);
    std::cout << "Calificación: " << estudiante.calificacion << std::endl;
}

// Función principal del programa
int main() {
    // Creación de objetos Persona y Estudiante
    Persona persona1("Juan", 20);
    Estudiante estudiante1("María", 22, 8.5);

    // Impresión de los datos de las personas
    std::cout << "Datos de la persona:" << std::endl;
    imprimirPersona(persona1);

    std::cout << std::endl;

    std::cout << "Datos del estudiante:" << std::endl;
    imprimirEstudiante(estudiante1);

    std::cout << std::endl;

    // Creación de un vector de objetos Persona
    std::vector<Persona> personas;
    personas.push_back(persona1);
    personas.push_back(estudiante1);

    // Impresión de los datos de las personas en el vector
    std::cout << "Datos de las personas en el vector:" << std::endl;
    for (Persona persona : personas) {
        imprimirPersona(persona);
        std::cout << std::endl;
    }

    // Ordenación del vector de personas por edad
    std::sort(personas.begin(), personas.end(), [](Persona a, Persona b) { return a.obtenerEdad() < b.obtenerEdad(); });

    // Impresión de los datos de las personas en el vector ordenado
    std::cout << "Datos de las personas en el vector ordenado por edad:" << std::endl;
    for (Persona persona : personas) {
        imprimirPersona(persona);
        std::cout << std::endl;
    }

    return 0;
}
```

Explicación del código:

* Se importan las librerías necesarias para el programa.
* Se definen las clases `Persona` y `Estudiante`. La clase `Persona` tiene atributos (nombre y edad) y métodos (constructor y método para obtener la edad). La clase `Estudiante` hereda de la clase `Persona` y añade el atributo "calificación".
* Se definen las funciones `imprimirPersona` e `imprimirEstudiante` para imprimir los datos de una persona y un estudiante, respectivamente.
* En la función principal del programa se crean objetos `Persona` y `Estudiante`, se imprimen sus datos y se crean un vector de objetos `Persona` y se imprimen sus datos.
* Se ordena el vector de personas por edad y se imprime de nuevo.