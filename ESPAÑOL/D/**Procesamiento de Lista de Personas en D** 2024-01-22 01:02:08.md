```d
// 1. Importar las bibliotecas necesarias.
import std.stdio, std.algorithm, std.math, std.string;

// 2. Definir una estructura de datos para representar a una persona.
struct Persona {
    string nombre;
    int edad;
};

// 3. Definir una función para comparar dos personas por su edad.
int compararPorEdad(Persona a, Persona b) {
    return a.edad - b.edad;
}

// 4. Definir una función para imprimir los datos de una persona.
void imprimirPersona(Persona persona) {
    writeln("Nombre:", persona.nombre);
    writeln("Edad:", persona.edad);
}

// 5. Crear una lista de personas.
immutable Personas personas = [
    Persona("Juan", 20),
    Persona("María", 30),
    Persona("Pedro", 40),
    Persona("Ana", 50)
];

// 6. Ordenar la lista por edad utilizando la función compararPorEdad.
personas.sort!(compararPorEdad);

// 7. Mostrar los datos de las personas ordenadas por edad.
writeln("Personas ordenadas por edad:");
foreach (persona; personas) {
    imprimirPersona(persona);
    writeln();
}

// 8. Calcular el promedio de edad de las personas.
immutable double promedio = personas.reduce!((promedio, persona) => promedio + persona.edad) / personas.length;

// 9. Mostrar el promedio de edad de las personas.
writeln("El promedio de edad es:", promedio);

// 10. Buscar a la persona mayor de edad.
immutable Persona personaMayor = personas.max!(compararPorEdad);

// 11. Mostrar los datos de la persona mayor de edad.
writeln("La persona mayor de edad es:");
imprimirPersona(personaMayor);

// 12. Eliminar a la persona mayor de edad de la lista.
personas.remove!(persona => persona.edad == personaMayor.edad);

// 13. Mostrar los datos de las personas restantes.
writeln("Personas restantes:");
foreach (persona; personas) {
    imprimirPersona(persona);
    writeln();
}
```

Explicación del código:

1. Importamos las bibliotecas necesarias para el programa.
2. Definimos una estructura de datos llamada `Persona` para representar a una persona. La estructura tiene dos campos: `nombre` y `edad`.
3. Definimos una función llamada `compararPorEdad` que compara dos personas por su edad. La función devuelve un valor positivo si la primera persona es mayor que la segunda, un valor negativo si la primera persona es menor que la segunda, y cero si las dos personas tienen la misma edad.
4. Definimos una función llamada `imprimirPersona` que imprime los datos de una persona. La función imprime el nombre y la edad de la persona.
5. Creamos una lista de personas llamada `personas`. La lista contiene cuatro personas con sus respectivos nombres y edades.
6. Ordenamos la lista de personas por edad utilizando la función `sort!` y la función `compararPorEdad`. El método `sort!` ordena la lista en su lugar, por lo que después de llamar a `sort!`, la lista `personas` estará ordenada por edad.
7. Mostramos los datos de las personas ordenadas por edad utilizando un bucle `foreach`. El bucle `foreach` itera sobre cada elemento de la lista y llama a la función `imprimirPersona` para imprimir los datos de la persona.
8. Calculamos el promedio de edad de las personas utilizando una expresión lambda y el método `reduce!`. El método `reduce!` reduce la lista a un solo valor aplicando una función a cada elemento de la lista. En este caso, la función lambda suma la edad de cada persona al valor actual del promedio. El resultado final del reduce es el promedio de edad de las personas.
9. Mostramos el promedio de edad de las personas.
10. Buscamos a la persona mayor de edad utilizando el método `max!`. El método `max!` devuelve el elemento más grande de la lista. En este caso, la persona mayor de edad es la que tiene la edad más grande.
11. Mostramos los datos de la persona mayor de edad.
12. Eliminamos a la persona mayor de edad de la lista utilizando el método `remove!`. El método `remove!` elimina el primer elemento de la lista que cumple con una condición. En este caso, la condición es que la edad de la persona sea igual a la edad de la persona mayor de edad.
13. Mostramos los datos de las personas restantes utilizando un bucle `foreach`.