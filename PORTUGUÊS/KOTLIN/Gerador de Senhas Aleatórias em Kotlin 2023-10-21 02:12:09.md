Claro, vou gerar um código complexo em Kotlin para você. Este código é um gerador de senhas aleatórias, utilizando uma combinação de letras maiúsculas, minúsculas, números e caracteres especiais.

```
import java.util.*

fun main() {
    val tamanhoSenha = 12
    val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789$%&@#!?"
    val random = Random()
    var senha = ""

    for (i in 0 until tamanhoSenha) {
        val index = random.nextInt(caracteres.length)
        senha += caracteres[index]
    }

    println("Sua senha aleatória é: $senha")
}
```

Aqui está uma breve explicação do código:

- Primeiro, importamos a biblioteca `java.util.*` para utilizar a classe `Random`, que será responsável por gerar números aleatórios.
- Em seguida, definimos a variável `tamanhoSenha` com o valor de 12, que será o tamanho da senha gerada.
- A variável `caracteres` é uma string que contém todas as possibilidades de caracteres que podem ser utilizados na senha (letras maiúsculas, minúsculas, números e caracteres especiais).
- Criamos a variável `random` do tipo `Random`, que será utilizada para gerar números aleatórios.
- Iniciamos a variável `senha` com uma string vazia.
- Utilizamos um loop `for` para percorrer de 0 até o tamanho da senha desejada (no caso, 12).
- Em cada iteração do loop, geramos um número aleatório entre 0 e o tamanho da string `caracteres` utilizando o método `nextInt()` da classe `Random`.
- Utilizamos o número gerado para acessar o caractere correspondente na string `caracteres` e adicionamos à variável `senha`.
- Ao final do loop, a variável `senha` terá uma combinação aleatória de caracteres maiúsculos, minúsculos, números e caracteres especiais.
- Por fim, imprimimos a senha gerada utilizando o método `println()`.

Espero que este código atenda às suas expectativas!