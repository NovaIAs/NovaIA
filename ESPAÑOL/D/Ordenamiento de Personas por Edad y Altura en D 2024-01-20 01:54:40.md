```d
// Importación de bibliotecas.
import std.stdio, std.algorithm;

// Declaración de una clase Persona.
class Persona {
    public:
        string nombre;
        int edad;
        double altura;
}

// Declaración de una función para comparar personas por su edad.
int compararPorEdad(Persona a, Persona b) {
    return a.edad - b.edad;
}

// Declaración de una función para comparar personas por su altura.
int compararPorAltura(Persona a, Persona b) {
    return a.altura - b.altura;
}

// Declaración de la función principal.
void main() {
    // Creación de un vector de personas.
    Persona[] personas = {
        Persona("Juan", 20, 1.75),
        Persona("María", 25, 1.60),
        Persona("Pedro", 30, 1.80),
        Persona("Ana", 35, 1.65)
    };

    // Impresión del vector de personas original.
    writeln("Vector de personas original:");
    foreach (persona in personas) {
        writeln("- {} ({} años, {} metros)", persona.nombre, persona.edad, persona.altura);
    }

    // Ordenamiento del vector de personas por edad.
    personas.sort!compararPorEdad();

    // Impresión del vector de personas ordenado por edad.
    writeln("\nVector de personas ordenado por edad:");
    foreach (persona in personas) {
        writeln("- {} ({} años, {} metros)", persona.nombre, persona.edad, persona.altura);
    }

    // Ordenamiento del vector de personas por altura.
    personas.sort!compararPorAltura();

    // Impresión del vector de personas ordenado por altura.
    writeln("\nVector de personas ordenado por altura:");
    foreach (persona in personas) {
        writeln("- {} ({} años, {} metros)", persona.nombre, persona.edad, persona.altura);
    }
}
```

Este código crea un vector de personas y lo ordena por edad y altura utilizando la función sort!. La función sort! toma una función de comparación como argumento y ordena el vector de acuerdo a esa función. La función de comparación compararPorEdad compara las personas por su edad, devolviendo un valor negativo si la primera persona es menor que la segunda, un valor cero si las personas son iguales y un valor positivo si la primera persona es mayor que la segunda. La función de comparación compararPorAltura compara las personas por su altura, devolviendo un valor negativo si la primera persona es más baja que la segunda, un valor cero si las personas son iguales y un valor positivo si la primera persona es más alta que la segunda.

El código también imprime el vector de personas original y los vectores ordenados por edad y altura.