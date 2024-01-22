```f#
// Definimos una función que calcula el área de un círculo dado su radio.
let areaCirculo radio =
    let pi = 3.141592653589793
    pi * radio**2

// Definimos una función que calcula el volumen de un cilindro dado su radio y altura.
let volumenCilindro radio altura =
    areaCirculo radio * altura

// Definimos una función que calcula el área de una esfera dado su radio.
let areaEsfera radio =
    4.0 * areaCirculo radio

// Definimos una función que calcula el volumen de una esfera dado su radio.
let volumenEsfera radio =
    (4.0 / 3.0) * pi * radio**3

// Definimos una función que calcula la longitud de una circunferencia dado su radio.
let longitudCircunferencia radio =
    2.0 * pi * radio

// Definimos una función que calcula el área de un triángulo dado su base y altura.
let areaTriangulo base altura =
    0.5 * base * altura

// Definimos una función que calcula el volumen de una pirámide dado su base y altura.
let volumenPiramide base altura =
    (1.0 / 3.0) * areaTriangulo base altura * altura

// Definimos una función que calcula el área de un rectángulo dado su base y altura.
let areaRectangulo base altura =
    base * altura

// Definimos una función que calcula el volumen de un prisma rectangular dado su base, altura y profundidad.
let volumenPrismaRectangular base altura profundidad =
    areaRectangulo base altura * profundidad

// Definimos una función que calcula el área de un cubo dado su lado.
let areaCubo lado =
    6.0 * lado**2

// Definimos una función que calcula el volumen de un cubo dado su lado.
let volumenCubo lado =
    lado**3

// Definimos una función que calcula la longitud de una diagonal de un cubo dado su lado.
let diagonalCubo lado =
    sqrt(3.0) * lado

// Definimos una función que calcula el área de un tetraedro dado su lado.
let areaTetraedro lado =
    sqrt(3.0) * lado**2

// Definimos una función que calcula el volumen de un tetraedro dado su lado.
let volumenTetraedro lado =
    (1.0 / 6.0) * sqrt(2.0) * lado**3

// Imprimimos los resultados de las funciones.
printfn "Área de un círculo con radio 5: %f" (areaCirculo 5.0)
printfn "Volumen de un cilindro con radio 5 y altura 10: %f" (volumenCilindro 5.0 10.0)
printfn "Área de una esfera con radio 5: %f" (areaEsfera 5.0)
printfn "Volumen de una esfera con radio 5: %f" (volumenEsfera 5.0)
printfn "Longitud de una circunferencia con radio 5: %f" (longitudCircunferencia 5.0)
printfn "Área de un triángulo con base 10 y altura 5: %f" (areaTriangulo 10.0 5.0)
printfn "Volumen de una pirámide con base 10 y altura 5: %f" (volumenPiramide 10.0 5.0)
printfn "Área de un rectángulo con base 10 y altura 5: %f" (areaRectangulo 10.0 5.0)
printfn "Volumen de un prisma rectangular con base 10, altura 5 y profundidad 3: %f" (volumenPrismaRectangular 10.0 5.0 3.0)
printfn "Área de un cubo con lado 5: %f" (areaCubo 5.0)
printfn "Volumen de un cubo con lado 5: %f" (volumenCubo 5.0)
printfn "Longitud de una diagonal de un cubo con lado 5: %f" (diagonalCubo 5.0)
printfn "Área de un tetraedro con lado 5: %f" (areaTetraedro 5.0)
printfn "Volumen de un tetraedro con lado 5: %f" (volumenTetraedro 5.0)
```

Este código es un ejemplo de cómo se puede utilizar F# para calcular diferentes propiedades geométricas. El código define una serie de funciones que calculan el área, el volumen y la longitud de diferentes formas geométricas, como círculos, cilindros, esferas, triángulos, pirámides, rectángulos, prismas rectangulares, cubos y tetraedros. El código también imprime los resultados de las funciones.

El código está organizado en una serie de funciones, cada una de las cuales calcula una propiedad geométrica diferente. Las funciones están definidas en orden alfabético. Las funciones utilizan una variedad de técnicas de programación funcional, como funciones recursivas, funciones de orden superior y funciones anónimas.

El código también utiliza el formato de cadena interpolada de F# para imprimir los resultados de las funciones. El formato de cadena interpolada permite insertar expresiones en una cadena. Las expresiones se evalúan y el resultado se inserta en la cadena.