```c++
// Librerías necesarias
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>

// Definir un tipo de datos personalizado para representar una persona.
struct Persona {
    std::string nombre;
    std::string apellido;
    int edad;

    // Constructor con parámetros.
    Persona(std::string nombre, std::string apellido, int edad)
        : nombre(nombre), apellido(apellido), edad(edad) {}

    // Método para imprimir los datos de una persona.
    void imprimir() {
        std::cout << "Nombre: " << nombre << std::endl;
        std::cout << "Apellido: " << apellido << std::endl;
        std::cout << "Edad: " << edad << std::endl;
    }
};

// Función para comparar dos personas por su edad.
bool compararPorEdad(const Persona& p1, const Persona& p2) {
    return p1.edad < p2.edad;
}

// Función para comparar dos personas por su nombre.
bool compararPorNombre(const Persona& p1, const Persona& p2) {
    return p1.nombre < p2.nombre;
}

// Función principal del programa.
int main() {
    // Crear un vector de personas.
    std::vector<Persona> personas;

    // Añadir personas al vector.
    personas.push_back(Persona("Juan", "García", 25));
    personas.push_back(Persona("María", "López", 30));
    personas.push_back(Persona("Pedro", "Sánchez", 40));

    // Mostrar el vector de personas original.
    std::cout << "Vector de personas original:" << std::endl;
    for (Persona persona : personas) {
        persona.imprimir();
        std::cout << std::endl;
    }

    // Ordenar el vector de personas por edad.
    std::sort(personas.begin(), personas.end(), compararPorEdad);

    // Mostrar el vector de personas ordenado por edad.
    std::cout << "Vector de personas ordenado por edad:" << std::endl;
    for (Persona persona : personas) {
        persona.imprimir();
        std::cout << std::endl;
    }

    // Crear un mapa que asocie el nombre de una persona con su edad.
    std::map<std::string, int> mapaNombresEdades;

    // Añadir pares clave-valor al mapa.
    mapaNombresEdades["Juan"] = 25;
    mapaNombresEdades["María"] = 30;
    mapaNombresEdades["Pedro"] = 40;

    // Mostrar el mapa de nombres y edades.
    std::cout << "Mapa de nombres y edades:" << std::endl;
    for (auto it = mapaNombresEdades.begin(); it != mapaNombresEdades.end(); ++it) {
        std::cout << "Nombre: " << it->first << ", Edad: " << it->second << std::endl;
    }

    // Crear un conjunto de nombres de personas.
    std::set<std::string> conjuntoNombres;

    // Añadir nombres al conjunto.
    conjuntoNombres.insert("Juan");
    conjuntoNombres.insert("María");
    conjuntoNombres.insert("Pedro");

    // Mostrar el conjunto de nombres.
    std::cout << "Conjunto de nombres:" << std::endl;
    for (std::string nombre : conjuntoNombres) {
        std::cout << nombre << std::endl;
    }

    // Encontrar el nombre de la persona más joven.
    Persona personaMasJoven = *std::min_element(personas.begin(), personas.end(), compararPorEdad);

    // Mostrar el nombre de la persona más joven.
    std::cout << "Nombre de la persona más joven: " << personaMasJoven.nombre << std::endl;

    return 0;
}
```

Explicación del código:

1. **Tipo de datos personalizado `Persona`**: Defino una estructura `Persona` que representa los datos de una persona, incluyendo su nombre, apellido y edad.


2. **Función `compararPorEdad`**: Esta función compara dos personas por su edad y devuelve `true` si la primera persona es más joven que la segunda.


3. **Función `compararPorNombre`**: Esta función compara dos personas por su nombre y devuelve `true` si el nombre de la primera persona es menor que el de la segunda.


4. **Vector de personas**: Creo un vector de personas y añado tres personas al vector.


5. **Mostrar el vector de personas original**: Muestro el vector de personas original en la consola.


6. **Ordenar el vector de personas por edad**: Ordeno el vector de personas por edad utilizando el algoritmo `std::sort` y la función `compararPorEdad` como función de comparación.


7. **Mostrar el vector de personas ordenado por edad**: Muestro el vector de personas ordenado por edad en la consola.


8. **Mapa de nombres y edades**: Creo un mapa que asocia el nombre de una persona con su edad.


9. **Añadir pares clave-valor al mapa**: Añado pares clave-valor al mapa, donde la clave es el nombre de la persona y el valor es su edad.


10. **Mostrar el mapa de nombres y edades**: Muestro el mapa de nombres y edades en la consola.


11. **Conjunto de nombres**: Creo un conjunto de nombres de personas.


12. **Añadir nombres al conjunto**: Añado nombres al conjunto.


13. **Mostrar el conjunto de nombres**: Muestro el conjunto de nombres en la consola.


14. **Encontrar el nombre de la persona más joven**: Encuentro el nombre de la persona más joven utilizando el algoritmo `std::min_element` y la función `compararPorEdad` como función de comparación.


15. **Mostrar el nombre de la persona más joven**: Muestro el nombre de la persona más joven en la consola.