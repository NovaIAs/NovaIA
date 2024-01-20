```d
import std.stdio, std.string, std.algorithm, std.traits, std.range;

enum EstadoCivil {
    SOLTERO, CASADO, DIVORCIADO, VIUDO
}

struct Persona {
    string nombre;
    EstadoCivil estadoCivil;
    int edad;
    float altura;
}

void main() {
    auto personas = [
        Persona("Juan", SOLTERO, 25, 1.80),
        Persona("María", CASADO, 30, 1.65),
        Persona("Pedro", DIVORCIADO, 40, 1.75),
        Persona("Ana", VIUDO, 50, 1.55)
    ];

    // Obtener los nombres de las personas que son solteras
    auto solteros = personas.filter!(p => p.estadoCivil == SOLTERO).map!(p => p.nombre);

    // Obtener la persona más alta
    auto personaMasAlta = personas.maxBy!(p => p.altura);

    // Calcular la edad promedio de las personas casadas
    auto edadPromedioCasados = personas.filter!(p => p.estadoCivil == CASADO).map!(p => p.edad).average();

    // Ordenar las personas por edad de menor a mayor
    auto personasOrdenadasPorEdad = personas.sort!byAscending(p => p.edad);

    // Imprimir los resultados
    writeln("Personas solteras:");
    solteros.each!(s => writeln("\t", s));
    writeln();
    writeln("Persona más alta:");
    writeln("\tNombre:", personaMasAlta.nombre);
    writeln("\tAltura:", personaMasAlta.altura);
    writeln();
    writeln("Edad promedio de las personas casadas:");
    writeln("\t", edadPromedioCasados);
    writeln();
    writeln("Personas ordenadas por edad de menor a mayor:");
    personasOrdenadasPorEdad.each!(p => writeln("\t", p.nombre, p.edad));
}
```

Explicación del código:

1. Se definen los tipos `EstadoCivil` y `Persona`. `EstadoCivil` es una enumeración que representa los posibles estados civiles de una persona, y `Persona` es una estructura que representa a una persona con su nombre, estado civil, edad y altura.
2. Se crea una lista de personas con sus respectivos datos.
3. Se utilizan las funciones `filter!`, `map!`, `maxBy!`, `average!` y `sort!` para obtener los datos deseados.
4. Se imprimen los resultados en la consola.