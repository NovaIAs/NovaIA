```f#

// Definición de tipos y estructuras

// Definición de un tipo personalizado para representar un punto en el espacio 2D
type Punto = { X: float; Y: float }

// Definición de un tipo personalizado para representar un círculo en el espacio 2D
type Circulo = { Centro: Punto; Radio: float }

// Definición de un tipo personalizado para representar un triángulo en el espacio 2D
type Triangulo = { Vertice1: Punto; Vertice2: Punto; Vertice3: Punto }

// Definición de un tipo personalizado para representar una figura geométrica en el espacio 2D
type FiguraGeometrica =
    | Punto of Punto
    | Circulo of Circulo
    | Triangulo of Triangulo

// Funciones para calcular el área de las figuras geométricas

// Función para calcular el área de un punto. (Siempre devuelve cero).
let areaPunto _ = 0.0

// Función para calcular el área de un círculo.
let areaCirculo { Radio } = PI * Radio ** 2.0

// Función para calcular el área de un triángulo.
let areaTriangulo { Vertice1; Vertice2; Vertice3 } =
    let lado1 = distancia Vertice1 Vertice2
    let lado2 = distancia Vertice2 Vertice3
    let lado3 = distancia Vertice3 Vertice1
    let semiperimetro = (lado1 + lado2 + lado3) / 2.0
    sqrt (semiperimetro * (semiperimetro - lado1) * (semiperimetro - lado2) * (semiperimetro - lado3))

// Función para calcular la distancia entre dos puntos.
let distancia { X: x1; Y: y1 } { X: x2; Y: y2 } =
    sqrt ((x2 - x1) ** 2.0 + (y2 - y1) ** 2.0)

// Función principal

// Crear una lista de figuras geométricas
let figuras = [
    Punto { X = 1.0; Y = 2.0 };
    Circulo { Centro = { X = 3.0; Y = 4.0 }; Radio = 5.0 };
    Triangulo { Vertice1 = { X = 6.0; Y = 7.0 }; Vertice2 = { X = 8.0; Y = 9.0 }; Vertice3 = { X = 10.0; Y = 11.0 } };
]

// Iterar sobre la lista de figuras geométricas y calcular el área de cada una
let areas = figuras |> List.map (fun figura ->
    match figura with
    | Punto _ -> areaPunto figura
    | Circulo circulo -> areaCirculo circulo
    | Triangulo triangulo -> areaTriangulo triangulo)

// Mostrar los resultados
printfn "Áreas de las figuras geométricas:"
for i in 0 .. (figuras.Length - 1) do
    printfn "%A: %f" (figuras.[i]) areas.[i]
done

```

Explicación:

1. Definición de tipos y estructuras:

   - Se definen los tipos personalizados `Punto`, `Circulo` y `Triangulo` para representar diferentes formas geométricas en el espacio 2D.
   - Se define el tipo personalizado `FiguraGeometrica` como una unión discriminada que puede representar cualquiera de los tres tipos anteriores.

2. Funciones para calcular el área de las figuras geométricas:

   - Se definen tres funciones para calcular el área de un punto, un círculo y un triángulo, respectivamente.
   - También se define una función auxiliar `distancia` para calcular la distancia entre dos puntos.

3. Función principal:

   - Se crea una lista de figuras geométricas que contiene un punto, un círculo y un triángulo.
   - Se itera sobre la lista de figuras geométricas y se calcula el área de cada una utilizando las funciones definidas anteriormente.
   - Se muestran los resultados de las áreas de las figuras geométricas en la consola.

Este código es complejo porque implica la definición de múltiples tipos personalizados y funciones para trabajar con ellos. También requiere un conocimiento de la programación funcional y de los patrones de coincidencia.