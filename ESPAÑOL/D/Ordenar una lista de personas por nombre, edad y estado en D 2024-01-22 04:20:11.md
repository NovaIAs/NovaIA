```d
// Importar las librerías necesarias
import std.stdio, std.ascii, std.algorithm, std.integral, std.string, std.traits;

// Definir los tipos de datos utilizados
enum Estado {
    ACTIVO,
    INACTIVO,
    PENDIENTE,
}

struct Persona {
    string nombre;
    int edad;
    Estado estado;
}

// Función para comparar personas por nombre
auto compareNombre(in Persona a, in Persona b) pure nothrow @nogc {
    return a.nombre < b.nombre;
}

// Función para comparar personas por edad
auto compareEdad(in Persona a, in Persona b) pure nothrow @nogc {
    return a.edad < b.edad;
}

// Función para comparar personas por estado
auto compareEstado(in Persona a, in Persona b) pure nothrow @nogc {
    return a.estado < b.estado;
}

// Función principal
void main() {
    // Crear una lista de personas
    auto personas = [
        Persona("Juan", 20, ACTIVO),
        Persona("María", 25, INACTIVO),
        Persona("Pedro", 30, PENDIENTE),
        Persona("Ana", 35, ACTIVO),
        Persona("Luis", 40, INACTIVO),
    ];

    // Ordenar la lista de personas por nombre
    personas.sort!(compareNombre);

    // Imprimir la lista de personas ordenada por nombre
    writeln("Personas ordenadas por nombre:");
    for (persona in personas) {
        writeln("\t{} ({})", persona.nombre, persona.estado);
    }

    // Ordenar la lista de personas por edad
    personas.sort!(compareEdad);

    // Imprimir la lista de personas ordenada por edad
    writeln("Personas ordenadas por edad:");
    for (persona in personas) {
        writeln("\t{} ({})", persona.nombre, persona.estado);
    }

    // Ordenar la lista de personas por estado
    personas.sort!(compareEstado);

    // Imprimir la lista de personas ordenada por estado
    writeln("Personas ordenadas por estado:");
    for (persona in personas) {
        writeln("\t{} ({})", persona.nombre, persona.estado);
    }
}
```

Explicación:

* Se importan las librerías necesarias, incluyendo la librería `std.stdio` para poder imprimir datos en la consola, `std.ascii` para poder comparar cadenas de caracteres, `std.algorithm` para poder ordenar la lista de personas, `std.integral` para poder utilizar el tipo de dato `int`, `std.string` para poder utilizar el tipo de dato `string`, y `std.traits` para poder utilizar la función `traits.isSign` para determinar si un tipo de dato es firmado o no firmado.
* Se definen los tipos de datos utilizados, incluyendo un tipo de enumeración `Estado` para representar el estado de una persona, y una estructura `Persona` para representar los datos de una persona.
* Se define una función `compareNombre` para comparar dos personas por nombre, una función `compareEdad` para comparar dos personas por edad, y una función `compareEstado` para comparar dos personas por estado.
* En la función principal, se crea una lista de personas y se ordena la lista por nombre, edad y estado utilizando las funciones `sort!` y `compareNombre`, `compareEdad`, y `compareEstado`, respectivamente.
* Finalmente, se imprimen los datos de las personas en la consola.