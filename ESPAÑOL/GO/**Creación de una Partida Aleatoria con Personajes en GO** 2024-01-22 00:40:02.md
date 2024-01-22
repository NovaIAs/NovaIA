```go
// Importación de librerías
package main

import (
	"fmt"
	"math/rand"
	"time"
)

// Definición de estructura
type Personaje struct {
	nombre  string
	edad    int
	clase   string
	habilidades []string
}

// Creación de personajes
var personajes = []Personaje{
	{
		nombre:  "Aragorn",
		edad:    35,
		clase:   "Guerrero",
		habilidades: []string{"Espada", "Arco", "Escudo"},
	},
	{
		nombre:  "Legolas",
		edad:    25,
		clase:   "Arquero",
		habilidades: []string{"Arco", "Dagas", "Acrobacias"},
	},
	{
		nombre:  "Gandalf",
		edad:    500,
		clase:   "Mago",
		habilidades: []string{"Magia de fuego", "Magia de hielo", "Magia de curación"},
	},
}

// Función para generar un número aleatorio
func generarNumeroAleatorio(min, max int) int {
	rand.Seed(time.Now().UnixNano())
	return min + rand.Intn(max-min+1)
}

// Función para crear una partida
func crearPartida() {
	// Crear un grupo de jugadores
	jugadores := make([]Personaje, 3)
	for i := 0; i < 3; i++ {
		// Seleccionar un personaje aleatorio de la lista
		indice := generarNumeroAleatorio(0, len(personajes)-1)
		jugadores[i] = personajes[indice]
	}

	// Mostrar los jugadores en la partida
	fmt.Println("Jugadores en la partida:")
	for _, jugador := range jugadores {
		fmt.Println(jugador.nombre)
	}
}

// Función principal del programa
func main() {
	// Crear una partida
	crearPartida()
}
```
El código crea tres estructuras `Personaje` con sus respectivos atributos de `nombre`, `edad`, `clase` y `habilidades`.

Luego, define un array `personajes` con tres elementos que son objetos de tipo `Personaje` con valores específicos para cada atributo.

La función `generarNumeroAleatorio` genera un número aleatorio entre un rango mínimo y máximo especificados.

La función `crearPartida` crea un array `jugadores` con tres elementos, cada uno de los cuales es un objeto de tipo `Personaje`. A continuación, selecciona tres personajes aleatorios de la lista `personajes` y los asigna a los elementos del array `jugadores`.

Por último, la función `main` llama a la función `crearPartida` para crear una partida y mostrar los jugadores en la partida.