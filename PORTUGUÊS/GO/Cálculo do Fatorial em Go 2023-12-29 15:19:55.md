Claro! Aqui está um exemplo de um código complexo em Go que realiza uma operação matemática de fatorial de um número:

```go
package main

import (
	"fmt"
)

func fatorial(n int) int {
	if n == 0 {
		return 1
	} else {
		return n * fatorial(n-1)
	}
}

func main() {
	var numero int
	fmt.Print("Digite um número inteiro: ")
	fmt.Scanln(&numero)

	resultado := fatorial(numero)
	fmt.Printf("O fatorial de %d é %d\n", numero, resultado)
}
```

Neste código, começamos importando o pacote "fmt", que nos permite realizar a entrada e saída de dados. Em seguida, temos uma função chamada `fatorial` que recebe um número inteiro como parâmetro e retorna o fatorial desse número.

A função `fatorial` é implementada usando uma recursão. Se o número passado for igual a 0, a função retorna 1 (pois o fatorial de 0 é 1). Caso contrário, ela chama a si mesma com o argumento `n-1` e multiplica esse resultado pelo número atual (`n`).

Na função `main`, declaramos uma variável `numero` para armazenar o número digitado pelo usuário. Usamos o `fmt.Scanln` para ler o valor digitado e armazená-lo nessa variável.

Em seguida, chamamos a função `fatorial` passando o número digitado como argumento e armazenamos o resultado na variável `resultado`.

Por fim, usamos o `fmt.Printf` para exibir o número e seu fatorial correspondente.

Esse código realiza o cálculo do fatorial de um número de forma recursiva, exibindo o resultado na tela.