Claro! Aqui está um exemplo de um código complexo em Go, escrito em português, que envolve uma implementação de uma árvore binária de busca (Binary Search Tree - BST).

```go
package main

import (
	"fmt"
)

type No struct {
	valor  int
	esquerda *No
	direita *No
}

type ArvoreBinaria struct {
	raiz *No
}

func (arvore *ArvoreBinaria) Inserir(valor int) {
	if arvore.raiz == nil {
		arvore.raiz = &No{valor: valor}
	} else {
		arvore.raiz.inserir(valor)
	}
}

func (no *No) inserir(valor int) {
	if valor < no.valor {
		if no.esquerda == nil {
			no.esquerda = &No{valor: valor}
		} else {
			no.esquerda.inserir(valor)
		}
	} else {
		if no.direita == nil {
			no.direita = &No{valor: valor}
		} else {
			no.direita.inserir(valor)
		}
	}
}

func (arvore *ArvoreBinaria) Buscar(valor int) bool {
	return arvore.raiz.buscar(valor)
}

func (no *No) buscar(valor int) bool {
	if no == nil {
		return false
	}

	if valor == no.valor {
		return true
	} else if valor < no.valor {
		return no.esquerda.buscar(valor)
	} else {
		return no.direita.buscar(valor)
	}
}

func main() {
	arvore := ArvoreBinaria{}

	arvore.Inserir(10)
	arvore.Inserir(5)
	arvore.Inserir(15)
	arvore.Inserir(2)
	arvore.Inserir(7)

	fmt.Println("A árvore contém o valor 5?", arvore.Buscar(5))    // true
	fmt.Println("A árvore contém o valor 12?", arvore.Buscar(12))  // false
	fmt.Println("A árvore contém o valor 2?", arvore.Buscar(2))    // true
	fmt.Println("A árvore contém o valor 20?", arvore.Buscar(20))  // false
}
```

Neste exemplo, começamos definindo a estrutura `No`, que representa um nó da árvore. Cada nó contém um valor e referências para os nós à esquerda e direita. 

Em seguida, temos a estrutura `ArvoreBinaria`, que representa a árvore binária de busca. Essa estrutura contém apenas um campo, `raiz`, que é um ponteiro para o nó raiz da árvore.

Dentro da estrutura `ArvoreBinaria`, temos o método `Inserir`, responsável por inserir um novo valor na árvore. Se a árvore estiver vazia, o valor é inserido como raiz. Caso contrário, a inserção é delegada ao método `inserir` do nó raiz.

No método `inserir`, verificamos se o valor a ser inserido é menor ou maior que o valor do nó atual. Se for menor, procuramos o lugar correto na subárvore esquerda. Caso contrário, procuramos na subárvore direita. A recursão é utilizada até encontrar um lugar vazio para inserir o valor.

Em seguida, temos o método `Buscar`, que verifica se um determinado valor está presente na árvore. A busca é iniciada no nó raiz, e a recursão é utilizada para percorrer os nós da árvore. Se o valor for encontrado, retornamos true; caso contrário, retornamos false.

No `main`, criamos uma instância da árvore binária e inserimos alguns valores. Em seguida, utilizamos o método `Buscar` para verificar se alguns valores estão presentes na árvore, e imprimimos os resultados na tela.

Espero que este exemplo tenha sido útil! Se tiver alguma dúvida ou precisar de mais ajuda, é só me perguntar.