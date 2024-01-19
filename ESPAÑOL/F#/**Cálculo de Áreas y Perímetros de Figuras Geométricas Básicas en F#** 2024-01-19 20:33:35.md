```f#
// Definición de tipos
type Punto = { X: float; Y: float }
type Linea = { Punto1: Punto; Punto2: Punto }
type Rectangulo = { Punto1: Punto; Punto2: Punto }
type Circulo = { Centro: Punto; Radio: float }
type Figura =
    | Punto of Punto
    | Linea of Linea
    | Rectangulo of Rectangulo
    | Circulo of Circulo

// Cálculo de áreas
let areaPunto _ = 0.0
let areaLinea _ = 0.0
let areaRectangulo { Punto1; Punto2 } = (Punto2.X - Punto1.X) * (Punto2.Y - Punto1.Y)
let areaCirculo { Centro; Radio } = PI * Radio ** 2

// Cálculo de perímetros
let perimetroPunto _ = 0.0
let perimetroLinea { Punto1; Punto2 } = sqrt((Punto2.X - Punto1.X) ** 2 + (Punto2.Y - Punto1.Y) ** 2)
let perimetroRectangulo { Punto1; Punto2 } =
    2 * ((Punto2.X - Punto1.X) + (Punto2.Y - Punto1.Y))
let perimetroCirculo { Centro; Radio } = 2 * PI * Radio

// Dibujado de figuras
let dibujarFigura figura =
    match figura with
    | Punto _ -> printfn "Punto"
    | Linea { Punto1; Punto2 } -> printfn "Linea: (%f, %f) -> (%f, %f)" Punto1.X Punto1.Y Punto2.X Punto2.Y
    | Rectangulo { Punto1; Punto2 } -> printfn "Rectangulo: (%f, %f) -> (%f, %f)" Punto1.X Punto1.Y Punto2.X Punto2.Y
    | Circulo { Centro; Radio } -> printfn "Circulo: (%f, %f), Radio: %f" Centro.X Centro.Y Radio

// Ejemplo de uso
let figuras = [
    Punto { X = 1.0; Y = 2.0 };
    Linea { Punto1 = { X = 3.0; Y = 4.0 }; Punto2 = { X = 5.0; Y = 6.0 } };
    Rectangulo { Punto1 = { X = 7.0; Y = 8.0 }; Punto2 = { X = 9.0; Y = 10.0 } };
    Circulo { Centro = { X = 11.0; Y = 12.0 }; Radio = 13.0 }
]

figuras |> List.iter dibujarFigura
```

Este código define una serie de tipos de figuras geométricas básicas, como puntos, líneas, rectángulos y círculos. También define funciones para calcular el área y el perímetro de cada figura, así como una función para dibujar la figura en una consola.

El código crea una lista de figuras y luego la recorre llamando a la función de dibujo para cada figura. Esto imprime una descripción de cada figura en la consola.

Algunos de los conceptos de programación funcional que se utilizan en este código incluyen:

* Definición de tipos: El código define una serie de tipos de datos personalizados para representar diferentes tipos de figuras geométricas.
* Funciones de orden superior: El código utiliza funciones de orden superior, como `List.iter`, para recorrer la lista de figuras y llamar a la función de dibujo para cada figura.
* Patrones coincidentes: El código utiliza patrones coincidentes para determinar qué tipo de figura se está pasando a la función de dibujo y luego llama a la función de dibujo correspondiente.

Este código es un ejemplo de cómo se pueden utilizar los conceptos de programación funcional para crear programas claros y concisos.