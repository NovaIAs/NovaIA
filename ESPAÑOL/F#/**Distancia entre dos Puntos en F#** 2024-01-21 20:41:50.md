```f#
// Definir un tipo de dato personalizado para representar un punto en el espacio
type Punto = { X : float; Y : float }

// Crear dos puntos
let punto1 = { X = 1.0; Y = 2.0 }
let punto2 = { X = 3.0; Y = 4.0 }

// Definir una función para calcular la distancia entre dos puntos
let distancia (p1 : Punto) (p2 : Punto) =
    let dx = p2.X - p1.X
    let dy = p2.Y - p1.Y
    sqrt(dx*dx + dy*dy)

// Utilizar la función para calcular la distancia entre los dos puntos
let distanciaPuntos = distancia punto1 punto2

// Mostrar el resultado
printfn "La distancia entre los dos puntos es: %A" distanciaPuntos

```

Explicación:

* La primera línea define un tipo de dato personalizado llamado `Punto` que representa un punto en el espacio. El tipo `Punto` tiene dos campos: `X` e `Y`, ambos de tipo `float`.
* La segunda y tercera línea crean dos puntos, `punto1` y `punto2`, usando el constructor del tipo `Punto`.
* La cuarta línea define una función llamada `distancia` que toma dos puntos como argumentos y devuelve la distancia entre ellos. La función utiliza el teorema de Pitágoras para calcular la distancia.
* La quinta línea utiliza la función `distancia` para calcular la distancia entre los dos puntos `punto1` y `punto2`.
* La sexta línea muestra el resultado en la consola utilizando la función `printfn`.