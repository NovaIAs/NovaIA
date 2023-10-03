Claro! Aqui está um exemplo de código complexo em Go. Este código é um simulador de jogo de cartas chamado "Blackjack". Vou explicar cada parte do código para facilitar o entendimento:

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

type Card struct {
	Suit  string
	Value string
}

type Deck struct {
	Cards []Card
}

type Player struct {
	Name  string
	Hand  []Card
	Score int
}

func (d *Deck) Initialize() {
	suits := []string{"Hearts", "Diamonds", "Spades", "Clubs"}
	values := []string{"Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King"}

	for _, suit := range suits {
		for _, value := range values {
			card := Card{Suit: suit, Value: value}
			d.Cards = append(d.Cards, card)
		}
	}

	rand.Seed(time.Now().UnixNano())
	rand.Shuffle(len(d.Cards), func(i, j int) { d.Cards[i], d.Cards[j] = d.Cards[j], d.Cards[i] })
}

func (d *Deck) DrawCard() Card {
	card := d.Cards[0]
	d.Cards = d.Cards[1:]
	return card
}

func (p *Player) CalculateScore() {
	p.Score = 0
	aceCount := 0

	for _, card := range p.Hand {
		switch card.Value {
		case "Ace":
			p.Score += 11
			aceCount++
		case "King", "Queen", "Jack":
			p.Score += 10
		default:
			p.Score += 1
		}
	}

	for i := 0; i < aceCount; i++ {
		if p.Score > 21 {
			p.Score -= 10
		}
	}
}

func main() {
	deck := Deck{}
	deck.Initialize()

	player := Player{Name: "Player"}
	dealer := Player{Name: "Dealer"}

	player.Hand = append(player.Hand, deck.DrawCard(), deck.DrawCard())
	dealer.Hand = append(dealer.Hand, deck.DrawCard(), deck.DrawCard())

	player.CalculateScore()
	dealer.CalculateScore()

	fmt.Println("Player's Hand:", player.Hand)
	fmt.Println("Player's Score:", player.Score)
	fmt.Println("Dealer's Hand:", dealer.Hand)
	fmt.Println("Dealer's Score:", dealer.Score)
}
```

Este código simula um jogo de cartas chamado "Blackjack". Vou explicar as principais partes:

- A estrutura `Card` representa uma carta do baralho, contendo um naipe (`Suit`) e um valor (`Value`).
- A estrutura `Deck` representa um baralho, que é uma coleção de cartas. Ele possui um slice de cartas (`Cards`) e possui um método `Initialize()` que preenche o baralho com todas as cartas possíveis, embaralha-as e as distribui.
- A estrutura `Player` representa um jogador do jogo. Ele possui um nome (`Name`), uma mão de cartas (`Hand`), e uma pontuação (`Score`). O jogador também possui um método `CalculateScore()` que calcula a pontuação com base nas cartas em sua mão.
- O método `Initialize()` do baralho preenche o slice `Cards` com todas as combinações possíveis de naipes e valores. Em seguida, ele embaralha as cartas usando a função `Shuffle()` do pacote `rand`.
- O método `DrawCard()` do baralho retira a primeira carta do slice `Cards` e a retorna.
- O método `CalculateScore()` do jogador calcula a pontuação com base nas cartas em sua mão. Ele itera sobre as cartas, somando os valores correspondentes. Caso haja um ás na mão, o jogador adiciona 11 à pontuação e verifica se excede 21. Se exceder, ele subtrai 10 para considerar o ás como valor 1.
- No `main()`, é criado um baralho (`deck`) e inicializado. Em seguida, são criados dois jogadores (`player` e `dealer`) e suas mãos são preenchidas com duas cartas cada. Depois disso, a pontuação de cada jogador é calculada e exibida na tela.

Espero que este código atenda às suas expectativas!