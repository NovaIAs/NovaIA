```go
// Programa que calcula el área y el perímetro de un círculo usando concurrencia y canales en Go.

package main

import (
	"fmt"
	"math"
	"sync"
)

// Estructura que representa un círculo.
type Circulo struct {
	Radio float64
}

// Función que calcula el área de un círculo.
func (c *Circulo) Area() float64 {
	return math.Pi * c.Radio * c.Radio
}

// Función que calcula el perímetro de un círculo.
func (c *Circulo) Perimetro() float64 {
	return 2 * math.Pi * c.Radio
}

// Función principal
func main() {
	// Crear un canal para comunicar el resultado del cálculo del área.
	areaCanal := make(chan float64)

	// Crear un canal para comunicar el resultado del cálculo del perímetro.
	perimetroCanal := make(chan float64)

	// Crear un grupo de espera para esperar a que terminen las goroutines.
	wg := &sync.WaitGroup{}

	// Crear una goroutine para calcular el área.
	wg.Add(1)
	go func() {
		// Calcular el área del círculo.
		area := Circulo{5}.Area()

		// Enviar el resultado del cálculo al canal.
		areaCanal <- area

		// Indicar al grupo de espera que la goroutine ha terminado.
		wg.Done()
	}()

	// Crear una goroutine para calcular el perímetro.
	wg.Add(1)
	go func() {
		// Calcular el perímetro del círculo.
		perimetro := Circulo{5}.Perimetro()

		// Enviar el resultado del cálculo al canal.
		perimetroCanal <- perimetro

		// Indicar al grupo de espera que la goroutine ha terminado.
		wg.Done()
	}()

	// Esperar a que terminen las goroutines.
	wg.Wait()

	// Recibir el resultado del cálculo del área del canal.
	area := <-areaCanal

	// Recibir el resultado del cálculo del perímetro del canal.
	perimetro := <-perimetroCanal

	// Imprimir el resultado del cálculo.
	fmt.Printf("Área: %f\n", area)
	fmt.Printf("Perímetro: %f\n", perimetro)
}
```

Explicación del código:

1. Definimos una estructura `Circulo` que representa un círculo con un campo `Radio` que almacena el radio del círculo.
2. Definimos dos métodos para la estructura `Circulo`: `Area()` y `Perimetro()`, que calculan el área y el perímetro del círculo, respectivamente.
3. En la función `main()`, creamos dos canales: `areaCanal` y `perimetroCanal`, para comunicar los resultados del cálculo del área y del perímetro, respectivamente.
4. Creamos un grupo de espera `wg` para esperar a que terminen las goroutines.
5. Creamos dos goroutines: una para calcular el área del círculo y otra para calcular el perímetro del círculo.
6. Las goroutines envían los resultados del cálculo a los canales correspondientes.
7. Esperamos a que terminen las goroutines utilizando el grupo de espera `wg`.
8. Recibimos los resultados del cálculo del área y del perímetro de los canales correspondientes.
9. Imprimimos los resultados del cálculo.