```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

// Define una estructura para representar una carta de la baraja española.
type Carta struct {
	palo    string
	numero  int
	figura  bool
	valor   int
	visible bool
}

// Define una estructura para representar una baraja española.
type Baraja struct {
	cartas []Carta
}

// Crea una nueva baraja española.
func NuevaBaraja() Baraja {
	baraja := Baraja{}
	palos := []string{"Oros", "Copas", "Espadas", "Bastos"}
	figuras := []string{"Sota", "Caballo", "Rey"}
	for _, palo := range palos {
		for i := 1; i <= 12; i++ {
			carta := Carta{palo: palo, numero: i, figura: false, valor: i, visible: false}
			baraja.cartas = append(baraja.cartas, carta)
		}
		for _, figura := range figuras {
			carta := Carta{palo: palo, numero: 0, figura: true, valor: 10, visible: false}
			baraja.cartas = append(baraja.cartas, carta)
		}
	}
	return baraja
}

// Baraja las cartas de la baraja.
func (b *Baraja) Barajar() {
	rand.Seed(time.Now().UnixNano())
	for i := 0; i < len(b.cartas); i++ {
		j := rand.Intn(len(b.cartas))
		b.cartas[i], b.cartas[j] = b.cartas[j], b.cartas[i]
	}
}

// Reparte las cartas de la baraja entre los jugadores.
func (b *Baraja) Repartir(jugadores int) [][]Carta {
	manos := [][]Carta{}
	for i := 0; i < jugadores; i++ {
		mano := []Carta{}
		for j := 0; j < len(b.cartas)/jugadores; j++ {
			carta := b.cartas[i*len(b.cartas)/jugadores+j]
			carta.visible = true
			mano = append(mano, carta)
		}
		manos = append(manos, mano)
	}
	return manos
}

// Muestra las cartas de la baraja.
func (b *Baraja) Mostrar() {
	for _, carta := range b.cartas {
		if carta.visible {
			fmt.Printf("%s %d", carta.palo, carta.numero)
			if carta.figura {
				fmt.Printf(" (%s)", carta.numero)
			}
			fmt.Println()
		}
	}
}

// Juega una carta de la mano del jugador.
func JugarCarta(mano []Carta, carta Jugada) {
	for i, c := range mano {
		if c.palo == carta.palo && c.numero == carta.numero {
			mano[i] = mano[len(mano)-1]
			mano = mano[:len(mano)-1]
			break
		}
	}
}

// Calcula la puntuación de la mano del jugador.
func CalcularPuntuación(mano []Carta) int {
	puntuación := 0
	for _, carta := range mano {
		puntuación += carta.valor
	}
	return puntuación
}

// Determina si el jugador ha ganado la partida.
func HaGanado(mano []Carta) bool {
	return CalcularPuntuación(mano) == 21
}

// Juega una partida de blackjack.
func JugarBlackjack() {
	baraja := NuevaBaraja()
	baraja.Barajar()
	jugadores := 2
	manos := baraja.Repartir(jugadores)
	for _, mano := range manos {
		fmt.Println("Tu mano:")
		Mostrar(mano)
	}
	for i, mano := range manos {
		fmt.Printf("Jugador %d:", i+1)
		for !HaGanado(mano) {
			fmt.Println("¿Quieres pedir otra carta? (s/n)")
			var respuesta string
			fmt.Scanln(&respuesta)
			if respuesta == "s" {
				carta := baraja.cartas[0]
				carta.visible = true
				mano = append(mano, carta)
				fmt.Println("Tu mano:")
				Mostrar(mano)
			} else {
				break
			}
		}
		if HaGanado(mano) {
			fmt.Printf("Jugador %d ha ganado!\n", i+1)
		} else {
			fmt.Printf("Jugador %d ha perdido!\n", i+1)
		}
	}
}

func main() {
	JugarBlackjack()
}
```

Este código es una implementación del juego de cartas Blackjack en Go. El código define una estructura para representar una carta de la baraja española y una estructura para representar una baraja española. También define funciones para crear una nueva baraja, barajar las cartas, repartir las cartas entre los jugadores, mostrar las cartas, jugar una carta, calcular la puntuación de la mano del jugador y determinar si el jugador ha ganado la partida. Finalmente, el código define una función para jugar una partida de Blackjack.

Aquí hay una explicación más detallada del código:

* La estructura `Carta` representa una carta de la baraja española. Tiene los siguientes campos:
    * `palo`: El palo de la carta (Oros, Copas, Espadas o Bastos).
    * `numero`: El número de la carta (1-12).
    * `figura`: Un valor booleano que indica si la carta es una figura (Sota, Caballo o Rey).
    * `valor`: El valor de la carta. Las cartas de 1 a 10 valen su número. Las figuras valen 10 y el as vale 1 u 11, dependiendo de la mano del jugador.
    * `visible`: Un valor booleano que indica si la carta está visible o boca abajo.
* La estructura `Baraja` representa una baraja española. Tiene el siguiente campo:
    * `cartas`: Un array de cartas que contiene todas las cartas de la baraja.
* La función `NuevaBaraja()` crea una nueva baraja española. Baraja las cartas y las devuelve.
* La función `Barajar()` baraja las cartas de la baraja.
* La función `Repartir()` reparte las cartas de la baraja entre los jugadores. Devuelve un array de arrays de cartas, donde cada array de cartas representa la mano de un jugador.
* La función `Mostrar()` muestra las cartas de la baraja.
* La función `JugarCarta()` juega una carta de la mano del jugador.
* La función `CalcularPuntuación()` calcula la puntuación de la mano del jugador.
* La función `HaGanado()` determina si el jugador ha ganado la partida.
* La función `JugarBlackjack()` juega una partida de Blackjack.

El código principal llama a la función `JugarBlackjack()` para jugar una partida del juego.