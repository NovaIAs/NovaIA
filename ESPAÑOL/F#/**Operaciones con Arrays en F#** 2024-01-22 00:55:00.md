```f#
// Función principal
let array = [| 1; 2; 3; 4; 5 |]

// Imprimir el array usando el bucle for
printfn "Array original: %A" array

// Función para calcular el promedio de los elementos del array
let promedio = array |> Array.sum |> div array.Length

// Imprimir el promedio
printfn "Promedio: %f" promedio

// Función para filtrar los elementos del array que son mayores que el promedio
let filtrado = array |> Array.filter (fun x -> x > promedio)

// Imprimir el array filtrado
printfn "Array filtrado: %A" filtrado

// Función para ordenar el array filtrado en orden descendente
let ordenado = filtrado |> Array.sortWith (fun a b -> b - a)

// Imprimir el array ordenado
printfn "Array ordenado: %A" ordenado
```

**Explicación del código:**

1. **Función principal:** La función principal es el punto de entrada del programa. Crea un array con los valores {1, 2, 3, 4, 5}.

2. **Impresión del array original:** Se utiliza la función `printfn` para imprimir el array original en la consola.

3. **Función para calcular el promedio:** La función `promedio` calcula el promedio de los elementos del array. Utiliza la función `Array.sum` para sumar los elementos del array y la función `div` para dividir la suma por el número de elementos del array.

4. **Impresión del promedio:** Se utiliza la función `printfn` para imprimir el promedio en la consola.

5. **Función para filtrar los elementos del array:** La función `filtrado` filtra los elementos del array que son mayores que el promedio. Utiliza la función `Array.filter` para filtrar los elementos del array según una condición.

6. **Impresión del array filtrado:** Se utiliza la función `printfn` para imprimir el array filtrado en la consola.

7. **Función para ordenar el array filtrado:** La función `ordenado` ordena el array filtrado en orden descendente. Utiliza la función `Array.sortWith` para ordenar el array según una función de comparación.

8. **Impresión del array ordenado:** Se utiliza la función `printfn` para imprimir el array ordenado en la consola.