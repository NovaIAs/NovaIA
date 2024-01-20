```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>

using namespace std;

// Objeto de tipo persona
class Persona {
public:
    string nombre;
    int edad;
    string ciudad;

    // Constructor
    Persona(string nombre, int edad, string ciudad) {
        this->nombre = nombre;
        this->edad = edad;
        this->ciudad = ciudad;
    }

    // Método para imprimir la información de la persona
    void imprimir() {
        cout << "Nombre: " << nombre << endl;
        cout << "Edad: " << edad << endl;
        cout << "Ciudad: " << ciudad << endl;
    }
};

// Función para comparar personas por edad
bool compararPorEdad(Persona p1, Persona p2) {
    return p1.edad < p2.edad;
}

// Función para comparar personas por ciudad
bool compararPorCiudad(Persona p1, Persona p2) {
    return p1.ciudad < p2.ciudad;
}

// Función principal
int main() {
    // Creamos un vector de personas
    vector<Persona> personas;
    personas.push_back(Persona("Juan", 25, "Madrid"));
    personas.push_back(Persona("Ana", 30, "Barcelona"));
    personas.push_back(Persona("Luis", 40, "Sevilla"));
    personas.push_back(Persona("María", 20, "Valencia"));
    personas.push_back(Persona("Pedro", 35, "Zaragoza"));

    // Imprimimos las personas sin ordenar
    cout << "Personas sin ordenar:" << endl;
    for (Persona p : personas) {
        p.imprimir();
        cout << endl;
    }

    // Ordenamos las personas por edad
    sort(personas.begin(), personas.end(), compararPorEdad);

    // Imprimimos las personas ordenadas por edad
    cout << "Personas ordenadas por edad:" << endl;
    for (Persona p : personas) {
        p.imprimir();
        cout << endl;
    }

    // Ordenamos las personas por ciudad
    sort(personas.begin(), personas.end(), compararPorCiudad);

    // Imprimimos las personas ordenadas por ciudad
    cout << "Personas ordenadas por ciudad:" << endl;
    for (Persona p : personas) {
        p.imprimir();
        cout << endl;
    }

    // Creamos un mapa de personas, usando el nombre como clave y la persona como valor
    map<string, Persona> mapaPersonas;
    for (Persona p : personas) {
        mapaPersonas[p.nombre] = p;
    }

    // Buscamos una persona en el mapa por su nombre
    string nombreBuscar = "Luis";
    Persona personaBuscada = mapaPersonas[nombreBuscar];

    // Imprimimos la información de la persona buscada
    cout << "Persona encontrada:" << endl;
    personaBuscada.imprimir();

    // Creamos un conjunto de ciudades, usando el nombre de la ciudad como elemento
    set<string> ciudades;
    for (Persona p : personas) {
        ciudades.insert(p.ciudad);
    }

    // Imprimimos las ciudades en el conjunto
    cout << "Ciudades:" << endl;
    for (string ciudad : ciudades) {
        cout << ciudad << endl;
    }

    return 0;
}
```

Explicación:

* El código define una clase `Persona` que tiene tres atributos: `nombre`, `edad` y `ciudad`.
* Se crean varias instancias de la clase `Persona` y se añaden a un vector.
* Se imprimen las personas sin ordenar.
* Se ordenan las personas por edad y se imprimen.
* Se ordenan las personas por ciudad y se imprimen.
* Se crea un mapa de personas, usando el nombre como clave y la persona como valor.
* Se busca una persona en el mapa por su nombre.
* Se imprime la información de la persona buscada.
* Se crea un conjunto de ciudades, usando el nombre de la ciudad como elemento.
* Se imprimen las ciudades en el conjunto.