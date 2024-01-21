```go
package main

import (
	"fmt"
	"math/rand"
	"strings"
	"time"
)

// Creación de un tipo personalizado "Persona" con campos "nombre" y "edad".
type Persona struct {
	nombre string
	edad   int
}

// Definición de una función "GenerarNombresAleatorios" para crear un slice de nombres aleatorios.
func GenerarNombresAleatorios(n int) []string {
	nombres := []string{
		"Juan", "María", "Pedro", "Ana", "José",
		"Francisco", "Juana", "Antonio", "Mercedes", "Carlos",
	}

	// Use rand.Shuffle para barajar la lista de nombres de forma aleatoria.
	rand.Shuffle(len(nombres), func(i, j int) {
		nombres[i], nombres[j] = nombres[j], nombres[i]
	})

	// Devuelve un slice con los primeros "n" elementos de la lista barajada.
	return nombres[:n]
}

// Función "ElegirGanador" para seleccionar un ganador aleatorio entre un slice de personas.
func ElegirGanador(personas []Persona) Persona {
	// Genera un índice aleatorio dentro del rango de personas.
	ganadorIdx := rand.Intn(len(personas))

	// Devuelve la persona correspondiente al índice aleatorio.
	return personas[ganadorIdx]
}

// Función principal "main".
func main() {
	// Establece la semilla para el generador de números aleatorios.
	rand.Seed(time.Now().UnixNano())

	// Crea un slice de nombres aleatorios.
	nombres := GenerarNombresAleatorios(10)

	// Crea un slice de personas con nombres y edades aleatorias.
	personas := make([]Persona, len(nombres))
	for i, nombre := range nombres {
		personas[i] = Persona{
			nombre: nombre,
			edad:   rand.Intn(100),
		}
	}

	// Muestra los nombres y edades de las personas.
	fmt.Println("Participantes:")
	for _, persona := range personas {
		fmt.Printf("%s (%d años)\n", persona.nombre, persona.edad)
	}

	// Selecciona un ganador aleatorio.
	ganador := ElegirGanador(personas)

	// Muestra el nombre del ganador.
	fmt.Printf("\n¡El ganador es %s!\n", ganador.nombre)

	// Clasifica a los participantes por edad en orden descendente.
	strings.Slice(personas, func(i, j int) bool {
		return personas[i].edad > personas[j].edad
	})

	// Muestra el podio de los 3 primeros clasificados.
	fmt.Println("\nPodium:")
	for i, persona := range personas[:3] {
		fmt.Printf("%d. %s (%d años)\n", i+1, persona.nombre, persona.edad)
	}
}
```

**Explicación del código:**

* El código define un tipo personalizado llamado `Persona` con campos `nombre` y `edad`.
* Define una función `GenerarNombresAleatorios` que genera un slice de nombres aleatorios.
* Define una función `ElegirGanador` que selecciona un ganador aleatorio entre un slice de personas.
* La función `main` crea un slice de nombres aleatorios y un slice de personas con nombres y edades aleatorias.
* Muestra los nombres y edades de las personas.
* Selecciona un ganador aleatorio y muestra su nombre.
* Ordena a las personas por edad en orden descendente.
* Muestra el podio de los 3 primeros clasificados.