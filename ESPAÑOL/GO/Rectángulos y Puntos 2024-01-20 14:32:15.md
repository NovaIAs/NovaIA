```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

// Definimos un tipo de dato personalizado llamado "Punto" que representa un punto en un espacio bidimensional.
type Punto struct {
	x, y int
}

// Definimos un tipo de dato personalizado llamado "Rectangulo" que representa un rectángulo en un espacio bidimensional.
type Rectangulo struct {
	origen Punto
	ancho, alto int
}

// Definimos una función llamada "generarPuntoAleatorio" que genera un punto aleatorio dentro de un rectángulo dado.
func generarPuntoAleatorio(r Rectangulo) Punto {
	// Generamos un número aleatorio para la coordenada x.
	x := rand.Intn(r.ancho) + r.origen.x

	// Generamos un número aleatorio para la coordenada y.
	y := rand.Intn(r.alto) + r.origen.y

	// Devolvemos el punto generado.
	return Punto{x, y}
}

// Definimos una función llamada "contienePunto" que comprueba si un punto dado está contenido dentro de un rectángulo dado.
func contienePunto(r Rectangulo, p Punto) bool {
	// Comprobamos si la coordenada x del punto está dentro del rectángulo.
	if p.x < r.origen.x || p.x > r.origen.x+r.ancho {
		return false
	}

	// Comprobamos si la coordenada y del punto está dentro del rectángulo.
	if p.y < r.origen.y || p.y > r.origen.y+r.alto {
		return false
	}

	// Si ambas coordenadas están dentro del rectángulo, devolvemos true.
	return true
}

// Definimos una función llamada "calcularArea" que calcula el área de un rectángulo dado.
func calcularArea(r Rectangulo) int {
	// Calculamos el área del rectángulo.
	area := r.ancho * r.alto

	// Devolvemos el área calculada.
	return area
}

// Definimos una función llamada "calcularPerimetro" que calcula el perímetro de un rectángulo dado.
func calcularPerimetro(r Rectangulo) int {
	// Calculamos el perímetro del rectángulo.
	perimetro := 2 * (r.ancho + r.alto)

	// Devolvemos el perímetro calculado.
	return perimetro
}

// Definimos una función llamada "imprimirRectangulo" que imprime las propiedades de un rectángulo dado.
func imprimirRectangulo(r Rectangulo) {
	// Imprimimos las propiedades del rectángulo.
	fmt.Printf("Origen: (%d, %d)\n", r.origen.x, r.origen.y)
	fmt.Printf("Ancho: %d\n", r.ancho)
	fmt.Printf("Alto: %d\n", r.alto)
	fmt.Printf("Área: %d\n", calcularArea(r))
	fmt.Printf("Perímetro: %d\n", calcularPerimetro(r))
}

// Función principal.
func main() {
	// Inicializamos el generador de números aleatorios.
	rand.Seed(time.Now().UnixNano())

	// Creamos un rectángulo.
	r := Rectangulo{Punto{10, 20}, 50, 30}

	// Imprimimos el rectángulo.
	imprimirRectangulo(r)

	// Creamos un punto aleatorio dentro del rectángulo.
	p := generarPuntoAleatorio(r)

	// Comprobamos si el punto está contenido dentro del rectángulo.
	if contienePunto(r, p) {
		fmt.Println("El punto está contenido dentro del rectángulo.")
	} else {
		fmt.Println("El punto no está contenido dentro del rectángulo.")
	}
}
```

Explicación:

* En este código definimos dos tipos de datos personalizados: Punto y Rectangulo. El tipo de dato Punto representa un punto en un espacio bidimensional, mientras que el tipo de dato Rectangulo representa un rectángulo en un espacio bidimensional.


* Definimos varias funciones que operan con estos tipos de datos. La función generarPuntoAleatorio genera un punto aleatorio dentro de un rectángulo dado. La función contienePunto comprueba si un punto dado está contenido dentro de un rectángulo dado. La función calcularArea calcula el área de un rectángulo dado. La función calcularPerimetro calcula el perímetro de un rectángulo dado. La función imprimirRectangulo imprime las propiedades de un rectángulo dado.


* En la función principal creamos un rectángulo, generamos un punto aleatorio dentro del rectángulo y comprobamos si el punto está contenido dentro del rectángulo. También imprimimos el rectángulo y sus propiedades.