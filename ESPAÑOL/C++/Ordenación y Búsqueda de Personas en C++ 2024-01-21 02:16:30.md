```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <sstream>

using namespace std;

class Persona {
public:
    string nombre;
    string apellidos;
    int edad;

    Persona(string nombre, string apellidos, int edad) {
        this->nombre = nombre;
        this->apellidos = apellidos;
        this->edad = edad;
    }

    string getNombreCompleto() {
        return nombre + " " + apellidos;
    }

    int getEdad() {
        return edad;
    }
};

class ComparadorPersonasPorNombre : public binary_function<Persona, Persona, bool> {
public:
    bool operator()(Persona persona1, Persona persona2) {
        return persona1.getNombreCompleto() < persona2.getNombreCompleto();
    }
};

class ComparadorPersonasPorEdad : public binary_function<Persona, Persona, bool> {
public:
    bool operator()(Persona persona1, Persona persona2) {
        return persona1.getEdad() < persona2.getEdad();
    }
};

int main() {
    // Crear un vector de personas
    vector<Persona> personas;
    personas.push_back(Persona("Juan", "García", 20));
    personas.push_back(Persona("María", "López", 25));
    personas.push_back(Persona("Pedro", "Sánchez", 30));
    personas.push_back(Persona("Ana", "Fernández", 35));

    // Ordenar el vector de personas por nombre
    sort(personas.begin(), personas.end(), ComparadorPersonasPorNombre());

    // Mostrar el vector de personas ordenado por nombre
    cout << "Personas ordenadas por nombre:" << endl;
    for (Persona persona : personas) {
        cout << persona.getNombreCompleto() << endl;
    }

    // Ordenar el vector de personas por edad
    sort(personas.begin(), personas.end(), ComparadorPersonasPorEdad());

    // Mostrar el vector de personas ordenado por edad
    cout << "Personas ordenadas por edad:" << endl;
    for (Persona persona : personas) {
        cout << persona.getNombreCompleto() << " (" << persona.getEdad() << " años)" << endl;
    }

    // Crear un mapa de personas, donde la clave es el nombre completo y el valor es la persona
    map<string, Persona> mapaPersonas;
    for (Persona persona : personas) {
        mapaPersonas[persona.getNombreCompleto()] = persona;
    }

    // Buscar una persona en el mapa por su nombre completo
    string nombreCompleto = "Pedro Sánchez";
    if (mapaPersonas.find(nombreCompleto) != mapaPersonas.end()) {
        Persona persona = mapaPersonas[nombreCompleto];
        cout << "Persona encontrada: " << persona.getNombreCompleto() << " (" << persona.getEdad() << " años)" << endl;
    } else {
        cout << "Persona no encontrada" << endl;
    }

    // Crear un conjunto de strings, donde cada string es el nombre completo de una persona
    set<string> conjuntoNombres;
    for (Persona persona : personas) {
        conjuntoNombres.insert(persona.getNombreCompleto());
    }

    // Comprobar si un nombre completo está en el conjunto
    string nombreCompleto2 = "Ana Fernández";
    if (conjuntoNombres.find(nombreCompleto2) != conjuntoNombres.end()) {
        cout << "Nombre encontrado: " << nombreCompleto2 << endl;
    } else {
        cout << "Nombre no encontrado" << endl;
    }

    return 0;
}
```

Este código crea una clase `Persona` con los atributos `nombre`, `apellidos` y `edad`. Luego, crea un vector de personas y lo ordena por nombre y por edad utilizando comparadores personalizados.

También crea un mapa de personas, donde la clave es el nombre completo y el valor es la persona, y un conjunto de strings, donde cada string es el nombre completo de una persona. Finalmente, busca una persona en el mapa por su nombre completo y comprueba si un nombre completo está en el conjunto.