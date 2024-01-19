```c++
// Archivo: "main.cpp"

#include <iostream>
using namespace std;

// Definición de la clase "Persona"
class Persona {
public:
    // Constructor
    Persona(string nombre, int edad) {
        this->nombre = nombre;
        this->edad = edad;
    }

    // Destructor
    ~Persona() {
        cout << "¡Adiós, " << nombre << "!" << endl;
    }

    // Métodos
    void saludar() {
        cout << "Hola, mi nombre es " << nombre << " y tengo " << edad << " años." << endl;
    }

private:
    string nombre;
    int edad;
};

// Definición de la función "main"
int main() {
    // Creación de objetos
    Persona persona1("Juan", 20);
    Persona persona2("María", 25);

    // Llamada a los métodos de los objetos
    persona1.saludar();
    persona2.saludar();

    // Destrucción de objetos
    return 0;
}
```

Explicación del código:

- **Definición de la clase "Persona"**: Se define una clase llamada "Persona" con dos atributos privados: "nombre" y "edad". También se definen un constructor y un destructor, y un método llamado "saludar".

- **Función "main"**: En la función "main" se crean dos objetos de la clase "Persona" y se llama al método "saludar" de cada uno de ellos.

- **Constructor**: El constructor de la clase "Persona" recibe dos parámetros: el nombre y la edad de la persona. Estos parámetros se utilizan para inicializar los atributos privados de la clase.

- **Destructor**: El destructor de la clase "Persona" se llama cuando se destruye un objeto de la clase. En este caso, el destructor simplemente imprime un mensaje de despedida.

- **Método "saludar"**: El método "saludar" de la clase "Persona" simplemente imprime un mensaje con el nombre y la edad de la persona.

- **Creación de objetos**: En la función "main" se crean dos objetos de la clase "Persona" utilizando el constructor de la clase.

- **Llamada a los métodos de los objetos**: En la función "main" se llama al método "saludar" de cada uno de los objetos de la clase "Persona" utilizando el operador punto.

- **Destrucción de objetos**: Cuando la función "main" termina de ejecutarse, los objetos de la clase "Persona" se destruyen automáticamente y el destructor de la clase se llama para cada uno de ellos.