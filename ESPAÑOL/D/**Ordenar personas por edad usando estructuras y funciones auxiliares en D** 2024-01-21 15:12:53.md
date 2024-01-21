```d
import std.stdio, std.string, std.algorithm;

// Definir una estructura "Persona" con los campos "nombre", "edad" y "sexo".
struct Persona {
    string nombre;
    int edad;
    char sexo;
}

// Función auxiliar para comparar dos objetos "Persona" por su edad.
int compararPorEdad(const Persona& a, const Persona& b) {
    return a.edad < b.edad;
}

// Función auxiliar para imprimir los datos de una persona.
void imprimirPersona(const Persona& persona) {
    writefln("%s (%d años, %c)", persona.nombre, persona.edad, persona.sexo);
}

// Función principal del programa.
void main() {
    // Crear un array de objetos "Persona".
    Persona[] personas = {
        Persona("Juan", 25, 'M'),
        Persona("María", 30, 'F'),
        Persona("Pedro", 20, 'M'),
        Persona("Ana", 28, 'F'),
        Persona("Luis", 35, 'M'),
    };

    // Ordenar el array de personas por edad usando la función auxiliar "compararPorEdad".
    personas.sort(compararPorEdad);

    // Imprimir los datos de cada persona en el array.
    foreach (persona in personas) {
        imprimirPersona(persona);
    }
}
```

Explicación del código:

1. Definimos la estructura "Persona" con los campos "nombre", "edad" y "sexo". Esta estructura nos permitirá almacenar los datos de cada persona en un único objeto.
2. Definimos una función auxiliar "compararPorEdad" que toma dos objetos "Persona" como parámetros y devuelve un entero. Esta función compara las edades de las dos personas y devuelve un valor negativo si la primera persona es más joven que la segunda, un cero si ambas personas tienen la misma edad, y un valor positivo si la primera persona es mayor que la segunda.
3. Definimos una función auxiliar "imprimirPersona" que toma un objeto "Persona" como parámetro y no devuelve ningún valor. Esta función simplemente imprime los datos de la persona en la consola.
4. En la función principal del programa, creamos un array de objetos "Persona" llamado "personas". Este array contiene los datos de cinco personas: Juan, María, Pedro, Ana y Luis.
5. Ordenamos el array de personas por edad usando la función auxiliar "compararPorEdad". Esto nos permite obtener un array de personas ordenado de menor a mayor edad.
6. Finalmente, recorremos el array de personas ordenado e imprimimos los datos de cada persona en la consola usando la función auxiliar "imprimirPersona".

Este código nos permite crear un array de objetos "Persona", ordenarlo por edad y luego imprimir los datos de cada persona en la consola. Es un ejemplo de cómo utilizar estructuras, funciones auxiliares y bucles en el lenguaje D.