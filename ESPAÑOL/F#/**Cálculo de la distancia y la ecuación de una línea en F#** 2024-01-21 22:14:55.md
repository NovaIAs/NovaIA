```f#
// Definición de tipos y funciones
type Punto = { X: float; Y: float }

type Linea = { Punto1: Punto; Punto2: Punto }

let distancia (p1: Punto) (p2: Punto) =
    sqrt((p2.X - p1.X) ** 2.0 + (p2.Y - p1.Y) ** 2.0)

let crearLinea (p1: Punto) (p2: Punto) =
    { Punto1 = p1; Punto2 = p2 }

// Función principal
let main argv =
    let punto1 = { X = 1.0; Y = 2.0 }
    let punto2 = { X = 4.0; Y = 6.0 }

    let linea = crearLinea punto1 punto2

    printfn "Distancia entre los puntos: %A" (distancia punto1 punto2)
    printfn "Ecuación de la línea: y = %A * x + %A" (linea.Punto2.Y - linea.Punto1.Y) / (linea.Punto2.X - linea.Punto1.X) linea.Punto1.Y

    0 // Código de salida

// Punto de entrada del programa
[<EntryPoint>]
let main' args =
    main args
```

Explicación:

* Se definen los tipos `Punto` y `Linea`.
* Se define una función `distancia` que calcula la distancia entre dos puntos.
* Se define una función `crearLinea` que crea una línea a partir de dos puntos.
* En la función `main`, se crean dos puntos, `punto1` y `punto2`, y se crea una línea, `linea`, a partir de estos puntos.
* Se utilizan las funciones `distancia` y `crearLinea` para calcular la distancia entre los dos puntos y la ecuación de la línea, respectivamente.
* Se imprimen los resultados en la consola.