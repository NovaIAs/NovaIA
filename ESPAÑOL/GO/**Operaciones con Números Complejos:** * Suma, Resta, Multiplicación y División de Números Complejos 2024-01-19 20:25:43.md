```go
package main

import (
	"fmt"
	"math/rand"
	"os"
	"time"
)

// Define el tipo de dato Complejo
type Complejo struct {
	real float64
	imag float64
}

// Define la función sumaComplejos que recibe dos números complejos y devuelve la suma de ambos.
func sumaComplejos(a, b Complejo) Complejo {
	return Complejo{a.real + b.real, a.imag + b.imag}
}

// Define la función restaComplejos que recibe dos números complejos y devuelve la resta de ambos.
func restaComplejos(a, b Complejo) Complejo {
	return Complejo{a.real - b.real, a.imag - b.imag}
}

// Define la función multiplicaciónComplejos que recibe dos números complejos y devuelve la multiplicación de ambos.
func multiplicaciónComplejos(a, b Complejo) Complejo {
	return Complejo{a.real*b.real - a.imag*b.imag, a.real*b.imag + a.imag*b.real}
}

// Define la función divisiónComplejos que recibe dos números complejos y devuelve la división de ambos.
func divisiónComplejos(a, b Complejo) Complejo {
	denominador := b.real*b.real + b.imag*b.imag
	return Complejo{(a.real*b.real + a.imag*b.imag) / denominador, (a.imag*b.real - a.real*b.imag) / denominador}
}

// Define la función generadorComplejos que genera un número complejo aleatorio con valores entre 0 y 10.
func generadorComplejos() Complejo {
	return Complejo{rand.Float64() * 10, rand.Float64() * 10}
}

// Define la función mostrarComplejo que recibe un número complejo y lo muestra en la consola.
func mostrarComplejo(c Complejo) {
	fmt.Printf("(%f, %f)\n", c.real, c.imag)
}

// Define la función mostrarOperacionesComplejos que recibe dos números complejos y muestra en la consola la suma, resta, multiplicación y división de ambos.
func mostrarOperacionesComplejos(a, b Complejo) {
	suma := sumaComplejos(a, b)
	resta := restaComplejos(a, b)
	multiplicación := multiplicaciónComplejos(a, b)
	división := divisiónComplejos(a, b)

	fmt.Printf("Suma: ")
	mostrarComplejo(suma)

	fmt.Printf("Resta: ")
	mostrarComplejo(resta)

	fmt.Printf("Multiplicación: ")
	mostrarComplejo(multiplicación)

	fmt.Printf("División: ")
	mostrarComplejo(división)
}

func main() {
	// Variable para controlar la semilla del generador de números aleatorios.
	rand.Seed(time.Now().UnixNano())

	// Genera dos números complejos aleatorios.
	a := generadorComplejos()
	b := generadorComplejos()

	// Muestra los números complejos generados.
	fmt.Printf("Número complejo A: ")
	mostrarComplejo(a)

	fmt.Printf("Número complejo B: ")
	mostrarComplejo(b)

	// Muestra las operaciones realizadas con los números complejos.
	mostrarOperacionesComplejos(a, b)

	// Crea un archivo de texto con los resultados de las operaciones.
	f, err := os.Create("operaciones_complejos.txt")
	if err != nil {
		fmt.Println("Error al crear el archivo:", err)
		return
	}

	// Escribe los resultados en el archivo de texto.
	_, err = f.WriteString(fmt.Sprintf("Suma: (%f, %f)\n", sumaComplejos(a, b).real, sumaComplejos(a, b).imag))
	if err != nil {
		fmt.Println("Error al escribir en el archivo:", err)
		return
	}

	_, err = f.WriteString(fmt.Sprintf("Resta: (%f, %f)\n", restaComplejos(a, b).real, restaComplejos(a, b).imag))
	if err != nil {
		fmt.Println("Error al escribir en el archivo:", err)
		return
	}

	_, err = f.WriteString(fmt.Sprintf("Multiplicación: (%f, %f)\n", multiplicaciónComplejos(a, b).real, multiplicaciónComplejos(a, b).imag))
	if err != nil {
		fmt.Println("Error al escribir en el archivo:", err)
		return
	}

	_, err = f.WriteString(fmt.Sprintf("División: (%f, %f)\n", divisiónComplejos(a, b).real, divisiónComplejos(a, b).imag))
	if err != nil {
		fmt.Println("Error al escribir en el archivo:", err)
		return
	}

	// Cierra el archivo de texto.
	if err := f.Close(); err != nil {
		fmt.Println("Error al cerrar el archivo:", err)
		return
	}
}
```

Explicación del código:

1. Se define el tipo de dato `Complejo` que representa un número complejo con dos valores: real e imaginario.

2. Se definen cuatro funciones que realizan las operaciones básicas con números complejos: suma, resta, multiplicación y división.

3. Se define una función `generadorComplejos` que genera un número complejo aleatorio con valores entre 0 y 10.

4. Se define una función `mostrarComplejo` que recibe un número complejo y lo muestra en la consola.

5. Se define una función `mostrarOperacionesComplejos` que recibe dos números complejos y muestra en la consola la suma, resta, multiplicación y división de ambos.

6. En la función `main`, se genera una semilla para el generador de números aleatorios.

7. Se generan dos números complejos aleatorios y se muestran en la consola.

8. Se llaman a las funciones para realizar las operaciones básicas con los números complejos y se muestran los resultados en la consola.

9. Se crea un archivo de texto y se escriben los resultados de las operaciones en el archivo.

10. Se cierra el archivo de texto.