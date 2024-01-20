```go
// Un programa complejo en Go.

// Importamos las librerías necesarias.
import (
	"fmt"
	"math"
)

// Creamos una estructura que representará un punto en el espacio.
type Punto struct {
	x float64
	y float64
}

// Definimos una función para crear un nuevo punto.
func NuevoPunto(x, y float64) *Punto {
	return &Punto{x, y}
}

// Definimos una función para calcular la distancia entre dos puntos.
func Distancia(p1, p2 *Punto) float64 {
	dx := p1.x - p2.x
	dy := p1.y - p2.y
	return math.Sqrt(dx*dx + dy*dy)
}

// Definimos una función para imprimir un punto.
func ImprimirPunto(p *Punto) {
	fmt.Printf("(%f, %f)", p.x, p.y)
}

// Definimos una función principal.
func main() {
	// Creamos dos puntos.
	p1 := NuevoPunto(0, 0)
	p2 := NuevoPunto(3, 4)

	// Calculamos la distancia entre los dos puntos.
	distancia := Distancia(p1, p2)

	// Imprimimos los puntos y la distancia entre ellos.
	ImprimirPunto(p1)
	fmt.Println()
	ImprimirPunto(p2)
	fmt.Println()
	fmt.Printf("Distancia: %f\n", distancia)
}
```

Este programa crea dos puntos en el espacio, calcula la distancia entre ellos y luego imprime los puntos y la distancia entre ellos.

El programa utiliza una estructura para representar un punto en el espacio. La estructura tiene dos campos, `x` e `y`, que representan las coordenadas del punto en el espacio.

El programa también define una función para crear un nuevo punto, una función para calcular la distancia entre dos puntos y una función para imprimir un punto.

La función principal del programa crea dos puntos, calcula la distancia entre ellos y luego imprime los puntos y la distancia entre ellos.

El resultado del programa es:

```
(0.000000, 0.000000)
(3.000000, 4.000000)
Distancia: 5.000000
```