Claro! Aqui está um exemplo de um código complexo em Go que envolve a manipulação de estruturas de dados e algoritmos, combinado com a leitura e processamento de arquivos em texto. O código abaixo tem como objetivo contar a quantidade de ocorrências de cada palavra em um arquivo de texto.

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
)

type WordCount struct {
    Word  string
    Count int
}

func main() {
    // Solicita ao usuário o nome do arquivo
    fmt.Print("Digite o nome do arquivo: ")
    reader := bufio.NewReader(os.Stdin)
    filename, _ := reader.ReadString('\n')
    filename = strings.TrimSpace(filename)

    // Abre o arquivo
    file, err := os.Open(filename)
    if err != nil {
        panic(err)
    }
    defer file.Close()

    // Cria um scanner para ler o arquivo linha por linha
    scanner := bufio.NewScanner(file)
    scanner.Split(bufio.ScanLines)

    // Mapa que armazena a contagem de palavras
    wordCountMap := make(map[string]int)

    // Processa cada linha do arquivo
    for scanner.Scan() {
        line := scanner.Text()
        words := strings.Fields(line)

        // Conta a quantidade de ocorrências de cada palavra
        for _, word := range words {
            wordCountMap[word]++
        }
    }

    // Converte o mapa de contagem para uma lista de estruturas WordCount
    wordCountList := make([]WordCount, 0, len(wordCountMap))
    for word, count := range wordCountMap {
        wordCountList = append(wordCountList, WordCount{word, count})
    }

    // Classifica a lista em ordem decrescente de contagem
    sortWordCountList(wordCountList)

    // Exibe o resultado
    fmt.Println("Palavra\t\tContagem")
    fmt.Println("-------------------------")
    for _, wc := range wordCountList {
        fmt.Println(wc.Word, "\t\t", wc.Count)
    }
}

// Função para classificar a lista de WordCount em ordem decrescente de contagem
func sortWordCountList(list []WordCount) {
    for i := 0; i < len(list)-1; i++ {
        for j := i + 1; j < len(list); j++ {
            if list[j].Count > list[i].Count {
                list[i], list[j] = list[j], list[i]
            }
        }
    }
}
```

Neste código, o programa solicita ao usuário o nome de um arquivo de texto e, em seguida, lê o arquivo linha por linha usando um scanner. A cada linha, as palavras são separadas e contadas, incrementando a contagem no mapa `wordCountMap`. Depois de processar todo o arquivo, o mapa é convertido em uma lista de estruturas `WordCount`. Em seguida, a lista é classificada em ordem decrescente de contagem usando a função `sortWordCountList`. Por fim, o resultado é exibido na tela, mostrando cada palavra e a quantidade de vezes que ela aparece no arquivo.