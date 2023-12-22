Claro! Aqui está um exemplo de um código complexo em Go que faz uso de concorrência para buscar informações de diferentes APIs e armazená-las em um banco de dados:

```go
package main

import (
	"database/sql"
	"fmt"
	"log"
	"net/http"
	"sync"
	"time"

	_ "github.com/go-sql-driver/mysql"
)

type APIResponse struct {
	API    string
	Result string
}

func fetchAPI(url string, wg *sync.WaitGroup, ch chan<- APIResponse) {
	defer wg.Done()

	resp, err := http.Get(url)
	if err != nil {
		log.Printf("Erro ao buscar a API %s: %s", url, err.Error())
		ch <- APIResponse{API: url, Result: ""}
		return
	}
	defer resp.Body.Close()

	// Simulando algum processamento dos dados da API
	time.Sleep(time.Millisecond * 500)

	ch <- APIResponse{API: url, Result: "Dados da API " + url}
}

func main() {
	db, err := sql.Open("mysql", "usuario:senha@tcp(localhost:3306)/database")
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()

	err = db.Ping()
	if err != nil {
		log.Fatal(err)
	}

	urls := []string{"https://api1.com", "https://api2.com", "https://api3.com"}

	var wg sync.WaitGroup
	ch := make(chan APIResponse)

	for _, url := range urls {
		wg.Add(1)
		go fetchAPI(url, &wg, ch)
	}

	go func() {
		wg.Wait()
		close(ch)
	}()

	for resp := range ch {
		stmt, err := db.Prepare("INSERT INTO api_results(api, result) VALUES (?, ?)")
		if err != nil {
			log.Fatal(err)
		}

		_, err = stmt.Exec(resp.API, resp.Result)
		if err != nil {
			log.Fatal(err)
		}

		stmt.Close()

		fmt.Printf("Dados da API %s salvos no banco de dados\n", resp.API)
	}
}
```

Neste código, utilizamos a biblioteca padrão `database/sql` para se conectar a um banco de dados MySQL. Primeiro, é necessário configurar a conexão com o banco de dados no trecho `sql.Open()`. Em seguida, verificamos se a conexão está ativa com o `db.Ping()`.

A lista de URLs das APIs que desejamos buscar é definida em `urls`. Em seguida, criamos um `sync.WaitGroup` para garantir que todas as goroutines sejam concluídas antes de fechar o canal. Utilizamos um canal `ch` para receber as respostas das APIs.

No loop `for` que itera sobre as URLs, adicionamos uma goroutine para buscar cada API utilizando a função `fetchAPI`. Essa função faz uma requisição HTTP para a API, processa os dados e envia a resposta para o canal.

A função `main` também possui uma goroutine anônima que espera todas as goroutines do `sync.WaitGroup` serem concluídas e fecha o canal `ch`.

Dentro do loop `for` que percorre as respostas no canal `ch`, preparamos uma declaração SQL para inserir os dados no banco de dados utilizando `db.Prepare()`. Em seguida, executamos a declaração com `stmt.Exec()` e imprimimos uma mensagem informando que os dados foram salvos.

Esse código demonstra como utilizar concorrência para buscar informações de várias APIs e armazená-las em um banco de dados utilizando Go. Observe que é necessário ajustar as configurações do banco de dados de acordo com o ambiente em que for executado.