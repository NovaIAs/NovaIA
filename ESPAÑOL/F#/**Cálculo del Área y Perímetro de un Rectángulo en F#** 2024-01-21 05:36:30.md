```f#
// Definición de tipos

type Coordenada2D = { x: float; y: float }

type Rectangulo = {
    origen: Coordenada2D;
    ancho: float;
    alto: float;
}

// Funciones

let crearRectangulo (origen: Coordenada2D) (ancho: float) (alto: float) =
    { origen = origen; ancho = ancho; alto = alto }

let areaRectangulo (rectangulo: Rectangulo) =
    rectangulo.ancho * rectangulo.alto

let perimetroRectangulo (rectangulo: Rectangulo) =
    2.0 * (rectangulo.ancho + rectangulo.alto)

// Uso del código

let rectangulo1 = crearRectangulo { x = 0.0; y = 0.0 } 10.0 5.0
let area1 = areaRectangulo rectangulo1
let perimetro1 = perimetroRectangulo rectangulo1

printfn "El área del rectángulo es: %f" area1
printfn "El perímetro del rectángulo es: %f" perimetro1

// Salida

// El área del rectángulo es: 50.000000
// El perímetro del rectángulo es: 30.000000
```

Explicación del código:

- Se definen dos tipos personalizados: `Coordenada2D` y `Rectangulo`.

- Se definen tres funciones: `crearRectangulo`, `areaRectangulo` y `perimetroRectangulo`.

- Se crea una variable llamada `rectangulo1` de tipo `Rectangulo` usando la función `crearRectangulo`.

- Se calculan el área y el perímetro del rectángulo `rectangulo1` usando las funciones `areaRectangulo` y `perimetroRectangulo`.

- Se imprime el área y el perímetro del rectángulo usando la función `printfn`.

Este es un ejemplo de código complejo en F# que utiliza tipos personalizados y funciones para realizar operaciones con rectángulos.