```go
package main

import (
	"fmt"
	"math"
)

// Definimos la estructura de datos "Punto" con dos campos: "x" e "y".
type Punto struct {
	x float64
	y float64
}

// Definimos la estructura de datos "Línea" con dos campos: "punto1" y "punto2".
type Línea struct {
	punto1 Punto
	punto2 Punto
}

// Definimos una función para calcular la distancia entre dos puntos.
func distanciaPuntos(punto1 Punto, punto2 Punto) float64 {
	return math.Sqrt(math.Pow(punto2.x-punto1.x, 2) + math.Pow(punto2.y-punto1.y, 2))
}

// Definimos una función para calcular la pendiente de una línea.
func pendienteLínea(línea Línea) float64 {
	return (línea.punto2.y - línea.punto1.y) / (línea.punto2.x - línea.punto1.x)
}

// Definimos una función para calcular la ordenada en el origen de una línea.
func ordenadaOrigenLínea(línea Línea) float64 {
	return línea.punto1.y - pendienteLínea(línea)*línea.punto1.x
}

// Definimos una función para determinar si dos líneas son paralelas.
func sonParalelas(línea1 Línea, línea2 Línea) bool {
	return math.Abs(pendienteLínea(línea1)-pendienteLínea(línea2)) < 0.0001
}

// Definimos una función para determinar si dos líneas son perpendiculares.
func sonPerpendiculares(línea1 Línea, línea2 Línea) bool {
	return math.Abs(pendienteLínea(línea1)*pendienteLínea(línea2)+1) < 0.0001
}

// Definimos una función para calcular el punto de intersección de dos líneas.
func puntoIntersecciónLíneas(línea1 Línea, línea2 Línea) Punto {
	x := (ordenadaOrigenLínea(línea2)-ordenadaOrigenLínea(línea1)) /
		(pendienteLínea(línea1)-pendienteLínea(línea2))
	y := pendienteLínea(línea1)*x + ordenadaOrigenLínea(línea1)
	return Punto{x, y}
}

// Definimos una función principal.
func main() {
	// Creamos dos puntos.
	punto1 := Punto{1, 2}
	punto2 := Punto{3, 4}

	fmt.Println("Distancia entre los puntos:", distanciaPuntos(punto1, punto2))

	// Creamos una línea.
	línea1 := Línea{Punto{0, 0}, Punto{1, 1}}

	// Calculamos la pendiente de la línea.
	fmt.Println("Pendiente de la línea:", pendienteLínea(línea1))

	// Calculamos la ordenada en el origen de la línea.
	fmt.Println("Ordenada en el origen de la línea:", ordenadaOrigenLínea(línea1))

	// Creamos otra línea.
	línea2 := Línea{Punto{0, 1}, Punto{1, 2}}

	// Determinamos si las dos líneas son paralelas.
	fmt.Println("¿Son paralelas las dos líneas?", sonParalelas(línea1, línea2))

	// Determinamos si las dos líneas son perpendiculares.
	fmt.Println("¿Son perpendiculares las dos líneas?", sonPerpendiculares(línea1, línea2))

	// Calculamos el punto de intersección de las dos líneas.
	fmt.Println("Punto de intersección de las dos líneas:", puntoIntersecciónLíneas(línea1, línea2))
}
```

Explicación del código:

* Definimos dos estructuras de datos personalizadas: "Punto" y "Línea". La estructura "Punto" tiene dos campos: "x" e "y", que representan las coordenadas x e y del punto. La estructura "Línea" tiene dos campos: "punto1" y "punto2", que representan los dos puntos que definen la línea.
* Definimos varias funciones para realizar cálculos geométricos con puntos y líneas. Estas funciones incluyen la distancia entre dos puntos, la pendiente de una línea, la ordenada en el origen de una línea, la determinación de si dos líneas son paralelas o perpendiculares, y el cálculo del punto de intersección de dos líneas.
* En la función principal, creamos dos puntos, dos líneas y calculamos varios valores geométricos utilizando las funciones definidas anteriormente. Imprimimos los resultados en la consola.