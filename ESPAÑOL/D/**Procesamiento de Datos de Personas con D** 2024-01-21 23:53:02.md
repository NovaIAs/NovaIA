```d
import std.stdio, std.datetime, std.traits;
import std.algorithm, std.range, std.math;

struct Persona {
    string nombre;
    int edad;
    char genero;
    float salario;
}

void imprimirPersona(Persona persona) {
    writeln("Nombre:", persona.nombre);
    writeln("Edad:", persona.edad);
    writeln("Género:", persona.genero);
    writeln("Salario:", persona.salario);
}

void ordenarPorEdad(Persona[] personas) {
    sort(personas, compareBy(Persona, .edad));
}

void filtrarPorGenero(Persona[] personas, char genero) {
    personas = filter(personas, p => p.genero == genero);
}

void calcularSalarioTotal(Persona[] personas) {
    float total = 0.0;
    foreach (Persona persona; personas) {
        total += persona.salario;
    }
    writeln("Salario total:", total);
}

void main() {
    Persona[] personas = [
        Persona("Juan", 25, 'M', 1500.0),
        Persona("María", 30, 'F', 2000.0),
        Persona("Pedro", 35, 'M', 2500.0),
        Persona("Ana", 40, 'F', 3000.0),
        Persona("Luis", 45, 'M', 3500.0)
    ];

    writeln("Personas:");
    foreach (Persona persona; personas) {
        imprimirPersona(persona);
        writeln;
    }

    writeln("Personas ordenadas por edad:");
    ordenarPorEdad(personas);
    foreach (Persona persona; personas) {
        imprimirPersona(persona);
        writeln;
    }

    writeln("Personas filtradas por género femenino:");
    filtrarPorGenero(personas, 'F');
    foreach (Persona persona; personas) {
        imprimirPersona(persona);
        writeln;
    }

    writeln("Salario total:");
    calcularSalarioTotal(personas);
}
```

Este código define una estructura `Persona` con los campos `nombre`, `edad`, `genero` y `salario`. Luego, define varias funciones para trabajar con objetos de tipo `Persona`: `imprimirPersona` para imprimir los campos de una persona, `ordenarPorEdad` para ordenar un array de personas por edad, `filtrarPorGenero` para filtrar un array de personas por género y `calcularSalarioTotal` para calcular el salario total de un array de personas.

La función `main` crea un array de personas, lo imprime, lo ordena por edad, lo filtra por género femenino y calcula el salario total.

El código utiliza varias características del lenguaje D, como el uso de estructuras, funciones genéricas, tipos de datos opcionales, iteradores y funciones de orden superior. También utiliza la biblioteca estándar de D, que incluye funciones para trabajar con cadenas, fechas, matemáticas y algoritmos.