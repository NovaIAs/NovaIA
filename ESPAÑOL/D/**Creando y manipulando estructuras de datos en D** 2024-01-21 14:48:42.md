```d
import std.c.stdio;

int main() {
    // Creación de un tipo de dato personalizado
    struct Persona {
        string nombre;
        int edad;
    };

    // Definición de una función para imprimir datos de una persona
    void imprimirPersona(Persona persona) {
        printf("%s, %d años\n", persona.nombre, persona.edad);
    }

    // Creación de un array de personas
    auto personas = [
        Persona("Juan", 25),
        Persona("María", 30),
        Persona("Pedro", 35)
    ];

    // Iteración sobre el array de personas e impresión de sus datos
    foreach (persona; personas) {
        imprimirPersona(persona);
    }

    return 0;
}
```

Explicación del código:

1. **Creación de un tipo de dato personalizado:** Se utiliza la palabra clave `struct` para crear un nuevo tipo de dato llamado `Persona`. Este tipo de dato tiene dos campos: `nombre` (tipo `string`) y `edad` (tipo `int`).

2. **Definición de una función para imprimir datos de una persona:** Se define una función llamada `imprimirPersona` que toma un argumento de tipo `Persona` y utiliza la función `printf` de la biblioteca estándar de C para imprimir el nombre y la edad de la persona.

3. **Creación de un array de personas:** Se utiliza la sintaxis `[]` para crear un array de personas. El array contiene tres elementos, cada uno de los cuales es una instancia del tipo de dato `Persona`.

4. **Iteración sobre el array de personas e impresión de sus datos:** Se utiliza la instrucción `foreach` para iterar sobre el array de personas y llamar a la función `imprimirPersona` para cada elemento del array. Esto imprime los datos de cada persona en el array.