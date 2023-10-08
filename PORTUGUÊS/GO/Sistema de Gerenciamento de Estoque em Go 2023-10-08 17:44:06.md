Claro! Aqui está um código complexo em Go que realiza uma simulação de um sistema de gerenciamento de estoque de uma loja. Vou comentar o código para facilitar o entendimento:

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

type Produto struct {
	ID          int
	Nome        string
	Quantidade  int
	Preco       float64
	DataCriacao time.Time
}

type Estoque struct {
	Produtos []Produto
}

func main() {
	estoque := Estoque{}
	rand.Seed(time.Now().UnixNano())

	// Inicializando o estoque com alguns produtos
	estoque.adicionarProduto(Produto{ID: 1, Nome: "Camiseta", Quantidade: 10, Preco: 29.99})
	estoque.adicionarProduto(Produto{ID: 2, Nome: "Calça Jeans", Quantidade: 5, Preco: 79.99})
	estoque.adicionarProduto(Produto{ID: 3, Nome: "Tênis", Quantidade: 8, Preco: 99.99})

	// Exibindo o estoque inicial
	fmt.Println("Estoque inicial:")
	estoque.mostrarEstoque()

	// Adicionando produtos aleatórios ao estoque
	for i := 0; i < 5; i++ {
		produto := Produto{
			ID:          rand.Intn(100),
			Nome:        fmt.Sprintf("Produto %d", i+4),
			Quantidade:  rand.Intn(20),
			Preco:       float64(rand.Intn(10000)) / 100,
			DataCriacao: time.Now(),
		}
		estoque.adicionarProduto(produto)
	}

	// Exibindo o estoque atualizado
	fmt.Println("\nEstoque atualizado:")
	estoque.mostrarEstoque()

	// Atualizando a quantidade de um produto
	produtoID := 2
	quantidadeAtualizada := 15
	estoque.atualizarQuantidade(produtoID, quantidadeAtualizada)

	// Exibindo o estoque após a atualização de quantidade
	fmt.Printf("\nEstoque após atualização de quantidade do produto %d:\n", produtoID)
	estoque.mostrarEstoque()

	// Removendo um produto do estoque
	produtoIDRemover := 3
	estoque.removerProduto(produtoIDRemover)

	// Exibindo o estoque após a remoção do produto
	fmt.Printf("\nEstoque após remoção do produto %d:\n", produtoIDRemover)
	estoque.mostrarEstoque()
}

// Método para adicionar um produto ao estoque
func (e *Estoque) adicionarProduto(produto Produto) {
	e.Produtos = append(e.Produtos, produto)
}

// Método para atualizar a quantidade de um produto no estoque
func (e *Estoque) atualizarQuantidade(produtoID int, novaQuantidade int) {
	for i, produto := range e.Produtos {
		if produto.ID == produtoID {
			e.Produtos[i].Quantidade = novaQuantidade
			break
		}
	}
}

// Método para remover um produto do estoque
func (e *Estoque) removerProduto(produtoID int) {
	for i, produto := range e.Produtos {
		if produto.ID == produtoID {
			e.Produtos = append(e.Produtos[:i], e.Produtos[i+1:]...)
			break
		}
	}
}

// Método para exibir o estoque na tela
func (e *Estoque) mostrarEstoque() {
	for _, produto := range e.Produtos {
		fmt.Printf("ID: %d | Nome: %s | Quantidade: %d | Preço: R$ %.2f | Data de criação: %s\n", produto.ID, produto.Nome, produto.Quantidade, produto.Preco, produto.DataCriacao.Format("2006-01-02 15:04:05"))
	}
}
```

Neste código, utilizamos a linguagem Go para simular um sistema de gerenciamento de estoque de uma loja. Criamos duas estruturas: `Produto` e `Estoque`. A estrutura `Produto` representa cada item do estoque, contendo informações como ID, nome, quantidade, preço e data de criação. A estrutura `Estoque` é responsável por armazenar todos os produtos.

O programa principal (`main`) inicia o estoque com alguns produtos predefinidos e, em seguida, adiciona produtos aleatórios ao estoque. Após cada modificação no estoque, o programa exibe o estoque atualizado na tela.

Também implementamos métodos para adicionar, atualizar e remover produtos do estoque. O método `mostrarEstoque` é responsável por exibir os produtos do estoque na tela, formatando as informações de maneira adequada.

Espero que este código atenda às suas expectativas! Caso tenha alguma dúvida, estou à disposição para ajudar.