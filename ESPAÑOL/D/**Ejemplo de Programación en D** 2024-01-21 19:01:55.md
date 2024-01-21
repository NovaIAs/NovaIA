```d

import std.stdio, std.array, std.algorithm, std.typecons, std.string, std.range,
      std.math, std.foreach, std.traits, std.immutable;
import std.algorithm : sort, merge, reverse, find;
import std.stdio : writeln, write, readln;
import std.range : to!string;
import std.array : size, popFront, back, clear, sort, merge;
import std.typecons : typeInfo;
import std.string : to!upper, join, format;
import std.math : sin, cos, tan;
import std.traits : isSortable, isArithmetic;
import std.immutable : ImmutableArray;


int main() {
    //Ejemplo de array
    ImmutableArray!long arr = [1, 2, 3, 4, 5];
    writeln("Array: " ~ arr.to!string);

    //Ejemplo de array dinámico
    Array!long arr_dinamico = new Array(5);
    for (int i = 0; i < arr_dinamico.length; i++) {
        arr_dinamico[i] = i + 1;
    }
    writeln("Array dinámico: " ~ arr_dinamico.to!string);

    //Ejemplo de slice
    Slice!long slice = arr.slice(1, 3);
    writeln("Slice: " ~ slice.to!string);

    //Ejemplo de función lambda
    auto sumar = (int a, int b) { return a + b; };
    writeln("Suma: " ~ format("%d", sumar(3, 4)));

    //Ejemplo de función recursiva
    int factorial(int n) {
        if (n == 0) {
            return 1;
        } else {
            return n * factorial(n - 1);
        }
    }
    writeln("Factorial de 5: " ~ format("%d", factorial(5)));

    //Ejemplo de clase
    class Persona {
        string nombre;
        int edad;

        Persona(string nombre, int edad) {
            this.nombre = nombre;
            this.edad = edad;
        }

        string to!string() {
            return format("%s, %d", nombre, edad);
        }
    }

    Persona persona1 = new Persona("Juan", 20);
    Persona persona2 = new Persona("María", 25);
    writeln("Persona 1: " ~ persona1.to!string);
    writeln("Persona 2: " ~ persona2.to!string);

    //Ejemplo de herencia
    class Empleado : Persona {
        string ocupacion;

        Empleado(string nombre, int edad, string ocupacion) : super(nombre, edad) {
            this.ocupacion = ocupacion;
        }

        string to!string() {
            return super.to!string ~ ", " ~ ocupacion;
        }
    }

    Empleado empleado1 = new Empleado("José", 30, "Ingeniero");
    Empleado empleado2 = new Empleado("Ana", 35, "Doctora");
    writeln("Empleado 1: " ~ empleado1.to!string);
    writeln("Empleado 2: " ~ empleado2.to!string);

    return 0;
}
```

Explicación del código:

* Se importan las bibliotecas necesarias para el código.
* Se declara un array inmutable de tipo long con 5 elementos.
* Se declara un array dinámico de tipo long con 5 elementos y se inicializa con un bucle for.
* Se declara un slice a partir del array inmutable, que es una subparte del array.
* Se declara una función lambda que suma dos números.
* Se declara una función recursiva que calcula el factorial de un número.
* Se declara una clase llamada Persona con dos propiedades, nombre y edad.
* Se declara una clase llamada Empleado que hereda de la clase Persona y tiene una propiedad adicional, ocupacion.
* Se crean dos objetos de la clase Persona y dos objetos de la clase Empleado.
* Se imprime en la consola la información de las personas y los empleados.