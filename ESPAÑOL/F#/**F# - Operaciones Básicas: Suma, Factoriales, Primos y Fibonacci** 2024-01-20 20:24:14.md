```f#
// Módulo principal
module Main =
    // Función principal
    let main() =
        // Definición de una lista de números
        let numeros = [1; 2; 3; 4; 5]
        // Definición de una función para sumar dos números
        let suma (x, y) = x + y

        // Uso de la función "reduce" para sumar todos los números de la lista
        printfn "La suma de los números es: %i" (numeros |> Seq.reduce suma 0)

        // Definición de una función para calcular el factorial de un número
        let factorial n = Seq.init n (fun i -> i+1) |> Seq.reduce (*) 1

        // Uso de la función "iter" para imprimir el factorial de cada número de la lista
        printfn "Los factoriales de los números son:"
        numeros |> Seq.iter (fun n -> printfn "%i: %i" n (factorial n))

        // Definición de una función para comprobar si un número es primo
        let Primo n =
            if n <= 1 then false
            else
                let limite = int (sqrt float n)
                Seq.init limite (fun i -> i+1) |> Seq.takeWhile (fun i -> n % i <> 0) |> Seq.isEmpty

        // Uso de la función "filter" para filtrar los números primos de la lista
        printfn "Los números primos de la lista son:"
        numeros |> Seq.filter Primo |> Seq.iter (fun n -> printfn "%i" n)

        // Definición de una función para generar números de Fibonacci
        let fib () =
            let rec iter (a, b) =
                let c = a + b
                (b, c)
            Seq.unfold iter (0, 1)

        // Uso de la función "take" para obtener los primeros 10 números de Fibonacci
        printfn "Los primeros 10 números de Fibonacci son:"
        fib () |> Seq.take 10 |> Seq.iter (fun n -> printfn "%i" n)
```

Explicación del código:

* El módulo `Main`, que contiene la función `main` como punto de entrada.
* La función `suma` se define para sumar dos números.
* La función `factorial` se define para calcular el factorial de un número.
* La función `Primo` se define para comprobar si un número es primo.
* La función `fib` se define para generar números de Fibonacci.
* El código utiliza la biblioteca estándar F# `Seq` para trabajar con secuencias.
* El código utiliza `printfn` para imprimir resultados en la consola.
* El código utiliza `|>` para encadenar llamadas a funciones.