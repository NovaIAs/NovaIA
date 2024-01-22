El siguiente código es un ejemplo de un código complejo en F#:

```f#
type Point = { X: float; Y: float }

// Función que calcula la distancia entre dos puntos.
let distance (p1: Point) (p2: Point) =
    let dx = p1.X - p2.X
    let dy = p1.Y - p2.Y
    Math.Sqrt(dx*dx + dy*dy)

// Función que calcula el perímetro de un polígono.
let perimeter (points: Point[]) =
    let rec loop (index: int) (acc: float) =
        if index = 0 then acc
        else loop (index - 1) (acc + distance (points.[index]) (points.[(index - 1)]))
    loop (Array.length points - 1) 0.0

// Función principal que lee los puntos de un polígono y calcula su perímetro.
let main args =

    // Leer los puntos del polígono desde la consola.
    let points =
        let mutable points = []
        while true do
            let line = Console.ReadLine()
            if line = null then
                break
            let parts = line.Split(',')
            points <- points @ [{ X = float parts.[0]; Y = float parts.[1] }]
        points

    // Calcular el perímetro del polígono.
    let perimeter = perimeter points

    // Mostrar el perímetro del polígono en la consola.
    Console.WriteLine("El perímetro del polígono es {0}", perimeter)

// Iniciar el programa principal.
[<EntryPoint>]
let main args = main args
```

Este código define un tipo de datos llamado `Point` que representa un punto en el espacio bidimensional. Luego define una función llamada `distance` que calcula la distancia entre dos puntos. A continuación, define una función llamada `perimeter` que calcula el perímetro de un polígono dado un array de puntos. Finalmente, define una función principal llamada `main` que lee los puntos de un polígono desde la consola, calcula su perímetro y lo muestra en la consola.