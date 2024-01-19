```f#
// Definición de tipos de datos

type Punto = { X: float; Y: float }

type Línea = { Punto1: Punto; Punto2: Punto }

type Triángulo = { Vértice1: Punto; Vértice2: Punto; Vértice3: Punto }

// Definición de funciones

// Distancia entre dos puntos
let distancia (p1: Punto) (p2: Punto) =
    sqrt((p2.X - p1.X)**2 + (p2.Y - p1.Y)**2)

// Área de un triángulo
let área (t: Triángulo) =
    let p1 = t.Vértice1
    let p2 = t.Vértice2
    let p3 = t.Vértice3
    let a = distancia p1 p2
    let b = distancia p2 p3
    let c = distancia p3 p1
    let s = (a + b + c) / 2.0
    sqrt(s * (s - a) * (s - b) * (s - c))

// Perímetro de un triángulo
let perímetro (t: Triángulo) =
    let p1 = t.Vértice1
    let p2 = t.Vértice2
    let p3 = t.Vértice3
    distancia p1 p2 + distancia p2 p3 + distancia p3 p1

// Comprueba si un punto está dentro de un triángulo
let dentro (t: Triángulo) (p: Punto) =
    let p1 = t.Vértice1
    let p2 = t.Vértice2
    let p3 = t.Vértice3
    let a = distancia p1 p2
    let b = distancia p2 p3
    let c = distancia p3 p1
    let d = distancia p1 p
    let e = distancia p2 p
    let f = distancia p3 p
    let s = a + b + c
    s > d + e + f

// Prueba de las funciones

let triángulo1 = { Vértice1 = { X = 0.0; Y = 0.0 }; Vértice2 = { X = 1.0; Y = 0.0 }; Vértice3 = { X = 0.0; Y = 1.0 } }

printfn "Área del triángulo1: %f" (área triángulo1)
printfn "Perímetro del triángulo1: %f" (perímetro triángulo1)

let punto1 = { X = 0.5; Y = 0.5 }

if dentro triángulo1 punto1 then
    printfn "El punto1 está dentro del triángulo1"
else
    printfn "El punto1 está fuera del triángulo1"
```

Explicación del código:

* Se definen los tipos de datos `Punto`, `Línea` y `Triángulo`.
* Se definen las funciones `distancia`, `área`, `perímetro` y `dentro`.
* Se crea un triángulo `triángulo1`.
* Se imprimen el área y el perímetro de `triángulo1`.
* Se crea un punto `punto1`.
* Se comprueba si `punto1` está dentro de `triángulo1`.