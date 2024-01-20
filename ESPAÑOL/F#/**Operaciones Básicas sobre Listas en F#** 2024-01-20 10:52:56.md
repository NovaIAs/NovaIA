```f#
// Definimos una función que recibe una lista de números y devuelve la suma de todos ellos.
let suma lista =
    // Utilizamos la función fold para recorrer la lista y sumar sus elementos.
    List.fold (+) 0 lista

// Definimos una función que recibe una lista de números y devuelve el producto de todos ellos.
let producto lista =
    // Utilizamos la función fold para recorrer la lista y multiplicar sus elementos.
    List.fold (*) 1 lista

// Definimos una función que recibe una lista de números y devuelve el promedio de todos ellos.
let promedio lista =
    // Sumamos todos los elementos de la lista.
    let suma = suma lista

    // Dividimos la suma por el número de elementos de la lista.
    suma / float lista.Length

// Definimos una función que recibe una lista de números y devuelve el máximo de todos ellos.
let maximo lista =
    // Utilizamos la función max para encontrar el máximo elemento de la lista.
    List.max lista

// Definimos una función que recibe una lista de números y devuelve el mínimo de todos ellos.
let minimo lista =
    // Utilizamos la función min para encontrar el mínimo elemento de la lista.
    List.min lista

// Definimos una lista de números.
let lista = [1, 2, 3, 4, 5]

// Imprimimos la suma de todos los elementos de la lista.
Console.WriteLine("Suma: {0}", suma lista)

// Imprimimos el producto de todos los elementos de la lista.
Console.WriteLine("Producto: {0}", producto lista)

// Imprimimos el promedio de todos los elementos de la lista.
Console.WriteLine("Promedio: {0}", promedio lista)

// Imprimimos el máximo de todos los elementos de la lista.
Console.WriteLine("Máximo: {0}", maximo lista)

// Imprimimos el mínimo de todos los elementos de la lista.
Console.WriteLine("Mínimo: {0}", minimo lista)
```

Este código es complejo porque:

* Utiliza funciones recursivas, como la función `fold` que se utiliza para recorrer la lista y sumar sus elementos.
* Utiliza funciones de orden superior, como la función `max` que se utiliza para encontrar el máximo elemento de la lista.
* Utiliza la programación funcional, que es un paradigma de programación que se caracteriza por el uso de funciones puras y la ausencia de efectos secundarios.

Este código es difícilmente repetible porque:

* Utiliza una combinación de funciones recursivas, funciones de orden superior y programación funcional.
* Es un código complejo que no es fácil de entender.
* Es un código que no es fácil de modificar.