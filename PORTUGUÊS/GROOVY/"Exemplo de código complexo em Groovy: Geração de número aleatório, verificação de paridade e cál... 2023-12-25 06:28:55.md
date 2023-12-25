Claro! Aqui está um exemplo de código complexo em Groovy:

```groovy
import java.util.Random

def gerarNumeroAleatorio() {
    Random random = new Random()
    return random.nextInt(100)
}

def verificarParidade(numero) {
    if (numero % 2 == 0) {
        return "par"
    } else {
        return "ímpar"
    }
}

def calcularFatorial(numero) {
    def fatorial = 1
    
    for (int i = 1; i <= numero; i++) {
        fatorial *= i
    }
    
    return fatorial
}

def main() {
    println("Bem-vindo(a) ao programa complexo em Groovy!")
    
    def numero = gerarNumeroAleatorio()
    println("Número gerado: ${numero}")
    
    def paridade = verificarParidade(numero)
    println("O número é ${paridade}")
    
    def fatorial = calcularFatorial(numero)
    println("O fatorial de ${numero} é ${fatorial}")
}

main()
```

Neste código, temos a definição de três funções:

1. `gerarNumeroAleatorio()`: Essa função utiliza a classe `Random` para gerar um número aleatório entre 0 e 99. O número gerado é retornado.

2. `verificarParidade(numero)`: Essa função verifica se um número é par ou ímpar. Ela recebe como parâmetro um número e verifica se o resto da divisão desse número por 2 é igual a zero. Se for, o número é par; caso contrário, é ímpar. O resultado da verificação é retornado como uma string.

3. `calcularFatorial(numero)`: Essa função calcula o fatorial de um número. Ela recebe como parâmetro um número e utiliza um laço de repetição `for` para multiplicar todos os números de 1 até o número em questão. O resultado do fatorial é retornado.

No método `main()`, temos a execução principal do programa:

1. É exibida uma mensagem de boas-vindas.

2. É chamada a função `gerarNumeroAleatorio()` para gerar um número aleatório.

3. O número gerado é exibido na tela.

4. É chamada a função `verificarParidade(numero)` para verificar a paridade do número gerado.

5. O resultado da verificação de paridade é exibido na tela.

6. É chamada a função `calcularFatorial(numero)` para calcular o fatorial do número gerado.

7. O resultado do cálculo fatorial é exibido na tela.

Dessa forma, o código gera um número aleatório, verifica se é par ou ímpar e calcula o seu fatorial, exibindo os resultados na tela.