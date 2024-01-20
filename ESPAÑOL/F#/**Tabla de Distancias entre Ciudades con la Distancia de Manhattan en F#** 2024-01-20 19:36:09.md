```f#
// Función para la distacia de Manhattan
let distanciaManhattan (x1, y1) (x2, y2) =
    abs(x1 - x2) + abs(y1 - y2)

// Crear una tabla de distancias entre ciudades
let tablaDistancias =
    let ciudades = ["Madrid", "Barcelona", "Valencia", "Sevilla", "Málaga"]
    let distancias = Array.map2 distanciaManhattan ciudades ciudades
    Array.map (fun (ciudad, distancias) ->
        printfn $"{ciudad}:"
        Array.iter (fun distancia -> printfn $" {distancia}") distancias)
        (ciudades, distancias)

// Primero, creamos una función llamada distanciaManhattan que calcula la distancia de Manhattan entre dos puntos. La distancia de Manhattan es la suma de las diferencias absolutas entre las dos coordenadas x e y de los puntos.

// A continuación, creamos una tabla de distancias entre las ciudades de la lista ciudades. Para ello, utilizamos la función Array.map2, que nos permite aplicar una función (en este caso, distanciaManhattan) a dos arrays a la vez. En este caso, los dos arrays son el array de ciudades y una copia del mismo array de ciudades.

// El resultado de Array.map2 es un array de arrays, donde cada array contiene las distancias entre una ciudad y todas las demás ciudades. A continuación, utilizamos la función Array.map para iterar sobre el array de arrays y mostrar la distancia entre cada ciudad y todas las demás ciudades.

// Por último, utilizamos la función prntf para imprimir la tabla de distancias. La función prntf es similar a la función printf de C, pero está diseñada específicamente para F#.
```
Este código es complejo porque utiliza varias características avanzadas de F#, como funciones de orden superior, arrays y la función prntf. También es difícil de repetir porque utiliza una serie de técnicas avanzadas, como la función Array.map2 y la función Array.map.