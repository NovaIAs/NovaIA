```go
package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"math/rand"
	"os"
	"strings"
	"time"
)

type Punto struct {
	x, y float64
}

type Linea struct {
	p1, p2 Punto
}

func (p Punto) Distancia(q Punto) float64 {
	dx := q.x - p.x
	dy := q.y - p.y
	return math.Sqrt(dx*dx + dy*dy)
}

func (l Linea) Longitud() float64 {
	return l.p1.Distancia(l.p2)
}

func (l Linea) PuntoMedio() Punto {
	return Punto{(l.p1.x + l.p2.x) / 2, (l.p1.y + l.p2.y) / 2}
}

func GenerarPuntos(n int) []Punto {
	puntos := make([]Punto, n)
	for i := range puntos {
		puntos[i] = Punto{rand.Float64(), rand.Float64()}
	}
	return puntos
}

func GenerarLineas(puntos []Punto) []Linea {
	lineas := make([]Linea, 0)
	for i := 0; i < len(puntos); i++ {
		for j := i + 1; j < len(puntos); j++ {
			lineas = append(lineas, Linea{puntos[i], puntos[j]})
		}
	}
	return lineas
}

func CalcularLongitudTotal(lineas []Linea) float64 {
	var longitudTotal float64
	for _, linea := range lineas {
		longitudTotal += linea.Longitud()
	}
	return longitudTotal
}

func CalcularPuntoMedio(lineas []Linea) Punto {
	var puntoMedio Punto
	for _, linea := range lineas {
		puntoMedio.x += linea.PuntoMedio().x
		puntoMedio.y += linea.PuntoMedio().y
	}
	puntoMedio.x /= float64(len(lineas))
	puntoMedio.y /= float64(len(lineas))
	return puntoMedio
}

func main() {
	// Leer el número de puntos a generar
	fmt.Print("Ingrese el número de puntos a generar: ")
	var n int
	fmt.Scanln(&n)

	// Generar los puntos
	puntos := GenerarPuntos(n)

	// Generar las líneas
	lineas := GenerarLineas(puntos)

	// Calcular la longitud total de las líneas
	longitudTotal := CalcularLongitudTotal(lineas)

	// Calcular el punto medio de las líneas
	puntoMedio := CalcularPuntoMedio(lineas)

	// Imprimir los resultados
	fmt.Println("Número de puntos:", n)
	fmt.Println("Puntos:")
	for _, punto := range puntos {
		fmt.Printf("(%f, %f)\n", punto.x, punto.y)
	}

	fmt.Println("Número de líneas:", len(lineas))
	fmt.Println("Líneas:")
	for _, linea := range lineas {
		fmt.Printf("[(%f, %f), (%f, %f)]\n", linea.p1.x, linea.p1.y, linea.p2.x, linea.p2.y)
	}

	fmt.Println("Longitud total:", longitudTotal)
	fmt.Println("Punto medio:", puntoMedio)
}
```

Explicación:

* El código define un tipo de dato personalizado llamado `Punto` que representa un punto en el espacio bidimensional.
* También define un tipo de dato personalizado llamado `Linea` que representa una línea que conecta dos puntos.
* El código define varias funciones para trabajar con puntos y líneas, como `Distancia()`, `Longitud()` y `PuntoMedio()`.
* El código genera aleatoriamente un conjunto de puntos y líneas.
* El código calcula la longitud total de todas las líneas.
* El código calcula el punto medio de todas las líneas.
* El código imprime los resultados.