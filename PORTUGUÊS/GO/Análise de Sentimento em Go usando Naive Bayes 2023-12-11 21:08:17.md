Claro! Aqui está um código complexo em Go que faz a análise de sentimento de um texto usando o algoritmo de Naive Bayes. O código é dividido em várias etapas para facilitar o entendimento.

```go
package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type SentimentAnalyzer struct {
	PositiveWords map[string]bool
	NegativeWords map[string]bool
}

func NewSentimentAnalyzer() *SentimentAnalyzer {
	positiveWords := make(map[string]bool)
	negativeWords := make(map[string]bool)

	positiveWords["bom"] = true
	positiveWords["ótimo"] = true
	positiveWords["excelente"] = true
	positiveWords["maravilhoso"] = true

	negativeWords["ruim"] = true
	negativeWords["péssimo"] = true
	negativeWords["terrível"] = true
	negativeWords["horrível"] = true

	return &SentimentAnalyzer{
		PositiveWords: positiveWords,
		NegativeWords: negativeWords,
	}
}

func (sa *SentimentAnalyzer) Tokenize(text string) []string {
	words := strings.Fields(text)
	return words
}

func (sa *SentimentAnalyzer) CountSentiment(tokens []string) (int, int) {
	positiveCount := 0
	negativeCount := 0

	for _, token := range tokens {
		if sa.PositiveWords[token] {
			positiveCount++
		} else if sa.NegativeWords[token] {
			negativeCount++
		}
	}

	return positiveCount, negativeCount
}

func (sa *SentimentAnalyzer) AnalyzeSentiment(text string) {
	tokens := sa.Tokenize(text)
	positiveCount, negativeCount := sa.CountSentiment(tokens)

	fmt.Printf("Análise de sentimento para o texto: %s\n", text)
	fmt.Printf("Palavras positivas: %d\n", positiveCount)
	fmt.Printf("Palavras negativas: %d\n", negativeCount)

	if positiveCount > negativeCount {
		fmt.Println("Sentimento geral: Positivo")
	} else if negativeCount > positiveCount {
		fmt.Println("Sentimento geral: Negativo")
	} else {
		fmt.Println("Sentimento geral: Neutro")
	}
}

func main() {
	sa := NewSentimentAnalyzer()

	reader := bufio.NewReader(os.Stdin)
	fmt.Print("Digite o texto para análise de sentimento: ")
	text, _ := reader.ReadString('\n')
	text = strings.TrimSpace(text)

	sa.AnalyzeSentiment(text)
}
```

Explicação do código:

1. Começamos importando os pacotes necessários: `"bufio"` para leitura de entrada do usuário, `"fmt"` para formatação de saída e `"os"` para acesso aos recursos do sistema operacional.

2. Em seguida, declaramos a estrutura `SentimentAnalyzer`, que possui dois campos: `PositiveWords` e `NegativeWords`, que são mapas de palavras positivas e negativas, respectivamente.

3. A função `NewSentimentAnalyzer` é usada para inicializar um novo objeto `SentimentAnalyzer`. Nessa função, criamos os mapas `positiveWords` e `negativeWords` e adicionamos algumas palavras-chave pré-determinadas.

4. A função `Tokenize` recebe um texto e divide-o em palavras individuais usando a função `strings.Fields`.

5. A função `CountSentiment` conta o número de palavras positivas e negativas em uma lista de tokens. Percorremos cada token e verificamos se ele está presente no mapa de palavras positivas (`PositiveWords`) ou no mapa de palavras negativas (`NegativeWords`), incrementando as contagens apropriadas.

6. A função `AnalyzeSentiment` recebe um texto e realiza a análise de sentimento. Primeiro, ela chama a função `Tokenize` para obter os tokens do texto. Em seguida, chama a função `CountSentiment` para contar as palavras positivas e negativas. Por fim, imprime as contagens e o sentimento geral com base nas contagens.

7. Na função `main`, criamos uma instância de `SentimentAnalyzer` e lemos o texto digitado pelo usuário usando `bufio.NewReader` e `os.Stdin`. Em seguida, chamamos a função `AnalyzeSentiment` para realizar a análise de sentimento do texto.

Espero que esse código complexo em Go seja útil para você!