Claro! Aqui está um código complexo em Go que faz a análise de sentimento de um texto usando a API de Processamento de Linguagem Natural do Google Cloud. Ele utiliza a biblioteca "google.golang.org/api/language/v1" para interagir com a API. Vou explicar o código ao longo do mesmo:

```go
package main

import (
	"context"
	"fmt"
	"log"
	"os"

	language "cloud.google.com/go/language/apiv1"
	"golang.org/x/text/language"
)

func main() {
	// Crie um contexto de fundo e instancie um cliente de idioma
	ctx := context.Background()
	client, err := language.NewClient(ctx)
	if err != nil {
		log.Fatalf("Falha ao criar cliente de idioma: %v", err)
	}

	// Especifique o texto para análise de sentimento
	text := "Eu amo como este código é tão complexo e diferenciado!"

	// Chame a função para analisar o sentimento do texto
	sentiment, err := analyzeSentiment(ctx, client, text)
	if err != nil {
		log.Fatalf("Falha ao analisar o sentimento: %v", err)
	}

	// Imprima o resultado
	fmt.Printf("Sentimento: %s\n", sentiment)
}

// analyzeSentiment é uma função que recebe o contexto, o cliente e o texto a ser analisado e retorna o sentimento detectado.
func analyzeSentiment(ctx context.Context, client *language.Client, text string) (string, error) {
	// Converta o texto fornecido para um tipo de mensagem do idioma
	message := &languagepb.AnnotateTextRequest{
		Document: &languagepb.Document{
			Source: &languagepb.Document_Content{
				Content: text,
			},
			Type: languagepb.Document_PLAIN_TEXT,
		},
		Features: &languagepb.AnnotateTextRequest_Features{
			ExtractSyntax:     false,
			ExtractEntities:   false,
			ExtractDocumentSentiment: true,
		},
	}

	// Faça a chamada à API para analisar o sentimento
	resp, err := client.AnalyzeSentiment(ctx, message)
	if err != nil {
		return "", fmt.Errorf("Falha ao analisar o sentimento: %v", err)
	}

	// Obtenha o sentimento do resultado da análise
	sentiment := resp.DocumentSentiment.Score

	// Determine a polaridade do sentimento
	var sentimentLabel string
	if sentiment >= 0.2 {
		sentimentLabel = "Positivo"
	} else if sentiment <= -0.2 {
		sentimentLabel = "Negativo"
	} else {
		sentimentLabel = "Neutro"
	}

	return sentimentLabel, nil
}
```

Neste código, estamos usando a biblioteca "google.golang.org/api/language/v1" para interagir com a API de Processamento de Linguagem Natural do Google Cloud. Começamos por importar os pacotes necessários e, em seguida, criamos um contexto de fundo e instanciamos um cliente de idioma.

Em seguida, especificamos o texto que desejamos analisar. Neste caso, o texto é "Eu amo como este código é tão complexo e diferenciado!".

Chamamos então a função `analyzeSentiment` passando o contexto, cliente e texto como argumentos. Essa função é responsável por enviar uma solicitação à API de Processamento de Linguagem Natural para analisar o sentimento do texto fornecido.

Dentro da função `analyzeSentiment`, convertemos o texto fornecido em um tipo de mensagem do idioma apropriado e fazemos a chamada à API para analisar o sentimento. O resultado retornado contém o sentimento detectado, que é um valor numérico entre -1 e 1. Em seguida, determinamos a polaridade do sentimento com base no valor retornado.

Finalmente, imprimimos o resultado na função `main`. Neste exemplo, o sentimento é "Positivo", pois o texto fornecido é positivo.

Este código é apenas um exemplo de como usar a API de Processamento de Linguagem Natural do Google Cloud para analisar o sentimento de um texto. Você pode adaptá-lo para suas necessidades específicas e explorar outras funcionalidades oferecidas por essa API.