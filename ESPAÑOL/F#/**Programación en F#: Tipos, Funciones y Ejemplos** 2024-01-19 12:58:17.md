```f#
// Definición de tipos
type Punto = { X: int; Y: int }
type Rango = { Inicio: int; Fin: int }
type Rectangulo = { EsquinaSuperiorIzquierda: Punto; Ancho: int; Alto: int }

// Funciones de puntos
let sumarPuntos (p1: Punto) (p2: Punto) = { X = p1.X + p2.X; Y = p1.Y + p2.Y }
let imprimirPunto (p: Punto) = printfn "(%d, %d)" p.X p.Y

// Funciones de rangos
let unionRangos (r1: Rango) (r2: Rango) =
    { Inicio = min r1.Inicio r2.Inicio; Fin = max r1.Fin r2.Fin }
let imprimirRango (r: Rango) = printfn "[%d, %d]" r.Inicio r.Fin

// Funciones de rectángulos
let crearRectangulo (esqSupIzq: Punto) (ancho: int) (alto: int) =
    { EsquinaSuperiorIzquierda = esqSupIzq; Ancho = ancho; Alto = alto }
let obtenerArea (r: Rectángulo) = r.Ancho * r.Alto
let imprimirRectángulo (r: Rectángulo) =
    printfn "Rectángulo: (%d, %d) (%d, %d)"
        r.EsquinaSuperiorIzquierda.X r.EsquinaSuperiorIzquierda.Y
        (r.EsquinaSuperiorIzquierda.X + r.Ancho)
        (r.EsquinaSuperiorIzquierda.Y + r.Alto)

// Uso de las funciones definidas
let punto1 = { X = 1; Y = 2 }
let punto2 = { X = 3; Y = 4 }
let rango1 = { Inicio = 5; Fin = 10 }
let rango2 = { Inicio = 2; Fin = 7 }
let rectángulo1 = crearRectangulo { X = 10; Y = 10 } 20 30

sumarPuntos punto1 punto2 |> imprimirPunto
unionRangos rango1 rango2 |> imprimirRango
imprimirRectángulo rectángulo1
```

Explicación del código:

1. Definimos tres tipos: `Punto`, `Rango` y `Rectángulo`.
2. Definimos funciones para trabajar con puntos, rangos y rectángulos.
3. Creamos variables de cada tipo y llamamos a las funciones para demostrar su uso.

Este código muestra cómo definir tipos, funciones y usarlas en F#. Es un ejemplo simple, pero demuestra cómo se pueden usar los tipos y funciones para crear código complejo y reutilizable.