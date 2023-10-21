Claro, aqui está um código complexo em GO:

```
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    rand.Seed(time.Now().UnixNano())

    numPlayers := rand.Intn(10) + 1
    players := make([]string, numPlayers)

    for i := 0; i < numPlayers; i++ {
        players[i] = fmt.Sprintf("Player %d", i+1)
    }

    fmt.Println("Players:", players)

    numRounds := rand.Intn(10) + 1
    fmt.Println("Number of rounds:", numRounds)

    for i := 0; i < numRounds; i++ {
        fmt.Printf("Round %d\n", i+1)

        for j := 0; j < numPlayers; j++ {
            score := rand.Intn(101)
            fmt.Printf("%s: %d\n", players[j], score)
        }
    }
}
```

Este código é um simulador de um jogo com um número aleatório de jogadores e rodadas. Ele usa a biblioteca rand para gerar números aleatórios e a biblioteca fmt para imprimir mensagens na tela. 

Primeiro, ele gera um número aleatório de jogadores e cria uma slice de jogadores com o tamanho correspondente. Em seguida, ele preenche a slice com nomes de jogadores usando um loop for.

Depois, ele gera um número aleatório de rodadas e imprime na tela. Em seguida, ele usa outro loop for para simular cada rodada. Dentro deste loop, ele usa um loop for aninhado para simular cada jogador. Para cada jogador, ele gera um número aleatório de pontos e imprime na tela o nome do jogador e sua pontuação.

Este código é complexo em termos de estruturas de controle e manipulação de dados, além de usar bibliotecas externas do GO. Ele é altamente personalizável, já que os números aleatórios podem ser ajustados para gerar um número maior ou menor de jogadores e rodadas.