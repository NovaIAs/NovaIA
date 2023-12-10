Claro! Aqui está um exemplo de um código complexo em Go que usa concorrência para criar um programa de busca de palavras-chave em um diretório de arquivos, fornecendo estatísticas sobre o número de ocorrências de cada palavra-chave encontrada.

```go
package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"sync"
)

type Result struct {
	filename string
	counts   map[string]int
}

func main() {
	if len(os.Args) < 3 {
		fmt.Println("Uso: go run main.go [diretório] [palavras-chave]")
		os.Exit(1)
	}

	root := os.Args[1]
	keywords := os.Args[2:]

	files, err := ioutil.ReadDir(root)
	if err != nil {
		fmt.Printf("Erro ao ler diretório: %s\n", err.Error())
		os.Exit(1)
	}

	result := make(chan Result, len(files))
	wg := sync.WaitGroup{}

	for _, file := range files {
		if !file.IsDir() {
			wg.Add(1)
			go func(filename string) {
				defer wg.Done()

				content, err := ioutil.ReadFile(filename)
				if err != nil {
					fmt.Printf("Erro ao ler arquivo %s: %s\n", filename, err.Error())
					return
				}

				counts := make(map[string]int)
				for _, keyword := range keywords {
					counts[keyword] = strings.Count(string(content), keyword)
				}

				result <- Result{filename, counts}
			}(filepath.Join(root, file.Name()))
		}
	}

	go func() {
		wg.Wait()
		close(result)
	}()

	totalCounts := make(map[string]int)
	for res := range result {
		fmt.Println("Arquivo:", res.filename)
		for keyword, count := range res.counts {
			fmt.Printf("  %s: %d\n", keyword, count)
			totalCounts[keyword] += count
		}
		fmt.Println()
	}

	fmt.Println("Total:")
	for keyword, count := range totalCounts {
		fmt.Printf("  %s: %d\n", keyword, count)
	}
}
```

Neste código, começamos verificando se o número correto de argumentos de linha de comando foi fornecido: o diretório raiz e as palavras-chave a serem pesquisadas. Caso contrário, exibimos uma mensagem de uso e saímos do programa.

Em seguida, lemos os arquivos do diretório especificado e iniciamos uma goroutine para cada arquivo. Cada goroutine lê o conteúdo do arquivo e conta o número de ocorrências de cada palavra-chave usando a função `strings.Count()`. Os resultados são enviados para o canal `result`.

Após todas as goroutines serem concluídas, fechamos o canal `result` e começamos a imprimir os resultados. Para cada resultado recebido do canal, exibimos o nome do arquivo e as contagens de palavras-chave correspondentes. Além disso, atualizamos a contagem total das palavras-chave.

Por fim, imprimimos a contagem total para cada palavra-chave.

Este código demonstra o uso de concorrência em Go para processar arquivos em paralelo e fornecer estatísticas sobre as palavras-chave encontradas.