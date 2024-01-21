```d
// Módulo principal
module Main {
    // Función principal
    void main() {
        // Creamos un array de cadenas de caracteres
        string[] nombres = ["Juan", "María", "Pedro", "Ana"];

        // Iteramos sobre el array con un bucle foreach
        foreach (nombre; nombres) {
            // Imprimimos el nombre actual
            writeln("Nombre:", nombre);
        }

        // Creamos un diccionario de cadenas de caracteres a enteros
        Dictionary<string, int> edades = new Dictionary<string, int>();

        // Añadimos pares clave-valor al diccionario
        edades["Juan"] = 20;
        edades["María"] = 18;
        edades["Pedro"] = 22;
        edades["Ana"] = 19;

        // Iteramos sobre el diccionario con un bucle foreach
        foreach (clave, valor; edades) {
            // Imprimimos la clave y el valor actuales
            writeln("Clave:", clave, ", Valor:", valor);
        }

        // Creamos una lista de objetos Persona
        Lista<Persona> personas = new Lista<Persona>();

        // Añadimos objetos Persona a la lista
        personas.addLast(new Persona("Juan", 20));
        personas.addLast(new Persona("María", 18));
        personas.addLast(new Persona("Pedro", 22));
        personas.addLast(new Persona("Ana", 19));

        // Iteramos sobre la lista con un bucle foreach
        foreach (persona; personas) {
            // Imprimimos el nombre y la edad de la persona actual
            writeln("Nombre:", persona.nombre, ", Edad:", persona.edad);
        }

        // Creamos una función que devuelve un número aleatorio
        int numeroAleatorio() {
            return random(1, 100);
        }

        // Llamamos a la función para generar un número aleatorio
        int numero = numeroAleatorio();

        // Imprimimos el número aleatorio
        writeln("Número aleatorio:", numero);

        // Creamos una clase Persona
        class Persona {
            // Propiedades de la clase
            string nombre;
            int edad;

            // Constructor de la clase
            Persona(string nombre, int edad) {
                this.nombre = nombre;
                this.edad = edad;
            }

            // Método de la clase
            string getNombre() {
                return nombre;
            }

            // Método de la clase
            int getEdad() {
                return edad;
            }
        }
    }
}
```

Explicación del código:

* El código crea un array de cadenas de caracteres con los nombres "Juan", "María", "Pedro" y "Ana".
* El código itera sobre el array con un bucle foreach y imprime los nombres.
* El código crea un diccionario de cadenas de caracteres a enteros con las edades de las personas.
* El código itera sobre el diccionario con un bucle foreach y imprime las claves y valores.
* El código crea una lista de objetos Persona con los nombres y edades de las personas.
* El código itera sobre la lista con un bucle foreach e imprime los nombres y edades de las personas.
* El código crea una función que devuelve un número aleatorio.
* El código llama a la función para generar un número aleatorio y lo imprime.
* El código crea una clase Persona con las propiedades nombre y edad.
* El código crea un constructor para la clase Persona que inicializa las propiedades nombre y edad.
* El código crea métodos getNombre y getEdad para la clase Persona.