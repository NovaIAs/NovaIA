Claro! Aqui está um exemplo de código em Go que realiza a codificação e decodificação de uma mensagem usando a técnica de Cifra de César. A Cifra de César é um tipo de criptografia que substitui cada letra por outra letra a um determinado número de posições à frente no alfabeto.

```go
package main

import (
	"fmt"
	"strings"
)

const alfabeto = "abcdefghijklmnopqrstuvwxyz"

func criptografar(mensagem string, chave int) string {
	mensagem = strings.ToLower(mensagem)
	cifra := ""

	for _, letra := range mensagem {
		if strings.ContainsRune(alfabeto, letra) {
			posicao := strings.IndexRune(alfabeto, letra)
			novaPosicao := (posicao + chave) % len(alfabeto)
			cifra += string(alfabeto[novaPosicao])
		} else {
			cifra += string(letra)
		}
	}

	return cifra
}

func descriptografar(cifra string, chave int) string {
	mensagem := ""

	for _, letra := range cifra {
		if strings.ContainsRune(alfabeto, letra) {
			posicao := strings.IndexRune(alfabeto, letra)
			novaPosicao := (posicao - chave + len(alfabeto)) % len(alfabeto)
			mensagem += string(alfabeto[novaPosicao])
		} else {
			mensagem += string(letra)
		}
	}

	return mensagem
}

func main() {
	mensagemOriginal := "Olá, mundo!"
	chave := 3

	cifra := criptografar(mensagemOriginal, chave)
	fmt.Println("Mensagem criptografada:", cifra)

	mensagemDecifrada := descriptografar(cifra, chave)
	fmt.Println("Mensagem decifrada:", mensagemDecifrada)
}
```

Nesse código, temos duas funções principais: `criptografar` e `descriptografar`. 

A função `criptografar` recebe uma mensagem e a chave de criptografia como parâmetros. Ela itera sobre cada letra da mensagem, verificando se a letra pertence ao alfabeto. Se sim, ela determina a posição da letra no alfabeto, calcula a nova posição somando a chave e realiza o módulo do resultado com o tamanho do alfabeto. A letra correspondente à nova posição é adicionada à cifra. Se a letra não pertence ao alfabeto, ela é simplesmente adicionada à cifra sem ser modificada. No final, a função retorna a cifra.

A função `descriptografar` realiza o processo inverso. Ela recebe a cifra e a chave de descriptografia como parâmetros. Assim como na função `criptografar`, ela itera sobre cada letra da cifra. Se a letra pertence ao alfabeto, ela determina a posição da letra na cifra, calcula a nova posição subtraindo a chave e realiza o módulo do resultado com o tamanho do alfabeto. A letra correspondente à nova posição é adicionada à mensagem. Se a letra não pertence ao alfabeto, ela é simplesmente adicionada à mensagem sem ser modificada. No final, a função retorna a mensagem original.

No `main`, um exemplo de uso é dado, onde uma mensagem original é criptografada com uma chave de valor 3 e, em seguida, a cifra é descriptografada com a mesma chave. O resultado é impresso no console.