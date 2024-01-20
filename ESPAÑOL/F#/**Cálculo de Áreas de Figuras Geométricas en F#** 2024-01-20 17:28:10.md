```f#
// Definimos una función para calcular el área de un triángulo.
let areaTriangulo base altura = (base * altura) / 2

// Definimos una función para calcular el área de un círculo.
let areaCirculo radio = Math.PI * Math.pow(radio, 2)

// Definimos una función para calcular el área de un cuadrado.
let areaCuadrado lado = lado * lado

// Definimos una función para calcular el área de un rectángulo.
let areaRectangulo ancho largo = ancho * largo

// Definimos una función para calcular el área de un trapecio.
let areaTrapecio baseMayor baseMenor altura = ((baseMayor + baseMenor) / 2) * altura

// Definimos una función para calcular el área de un paralelogramo.
let areaParalelogramo base altura = base * altura

// Definimos una función para calcular el área de un rombo.
let areaRombo diagonalMayor diagonalMenor = (diagonalMayor * diagonalMenor) / 2

// Definimos una función para calcular el área de un hexágono regular.
let areaHexagonoRegular lado = (3 * Math.sqrt(3) / 2) * Math.pow(lado, 2)

// Definimos una función para calcular el área de un octógono regular.
let areaOctogonoRegular lado = 2 * (1 + Math.sqrt(2)) * Math.pow(lado, 2)

// Definimos una función para calcular el área de un dodecágono regular.
let areaDodecagonoRegular lado = 3 * Math.sqrt(3) * Math.pow(lado, 2)

// Imprimimos las áreas de las diferentes figuras geométricas.
printfn "Área de un triángulo con base 10 y altura 5: %f" (areaTriangulo 10.0 5.0)
printfn "Área de un círculo con radio 5: %f" (areaCirculo 5.0)
printfn "Área de un cuadrado con lado 10: %f" (areaCuadrado 10.0)
printfn "Área de un rectángulo con ancho 10 y largo 5: %f" (areaRectangulo 10.0 5.0)
printfn "Área de un trapecio con base mayor 10, base menor 5 y altura 5: %f" (areaTrapecio 10.0 5.0 5.0)
printfn "Área de un paralelogramo con base 10 y altura 5: %f" (areaParalelogramo 10.0 5.0)
printfn "Área de un rombo con diagonal mayor 10 y diagonal menor 5: %f" (areaRombo 10.0 5.0)
printfn "Área de un hexágono regular con lado 10: %f" (areaHexagonoRegular 10.0)
printfn "Área de un octógono regular con lado 10: %f" (areaOctogonoRegular 10.0)
printfn "Área de un dodecágono regular con lado 10: %f" (areaDodecagonoRegular 10.0)
```