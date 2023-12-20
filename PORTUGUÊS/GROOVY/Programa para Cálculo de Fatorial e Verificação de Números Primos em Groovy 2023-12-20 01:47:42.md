Claro! Aqui está um código complexo em Groovy:

```
import java.util.*

def calculaFatorial(numero) {
    if (numero < 0) {
        return "Não é possível calcular o fatorial de um número negativo."
    } else if (numero == 0 || numero == 1) {
        return 1
    } else {
        def resultado = 1
        
        for (int i = 2; i <= numero; i++) {
            resultado *= i
        }
        
        return resultado
    }
}

def verificaPrimo(numero) {
    if (numero < 2) {
        return false
    }
    
    for (int i = 2; i <= Math.sqrt(numero); i++) {
        if (numero % i == 0) {
            return false
        }
    }
    
    return true
}

def main() {
    Scanner scanner = new Scanner(System.in)
    println("Bem-vindo(a) ao programa!")
    
    while (true) {
        println("\nEscolha uma das opções:")
        println("1 - Calcular o fatorial de um número")
        println("2 - Verificar se um número é primo")
        println("3 - Sair do programa")
        
        print("Opção escolhida: ")
        def opcao = scanner.nextInt()
        
        switch (opcao) {
            case 1:
                print("Digite um número inteiro: ")
                def numeroFatorial = scanner.nextInt()
                def resultadoFatorial = calculaFatorial(numeroFatorial)
                println("O fatorial de ${numeroFatorial} é ${resultadoFatorial}")
                break
            case 2:
                print("Digite um número inteiro: ")
                def numeroPrimo = scanner.nextInt()
                def ehPrimo = verificaPrimo(numeroPrimo)
                if (ehPrimo) {
                    println("${numeroPrimo} é um número primo.")
                } else {
                    println("${numeroPrimo} não é um número primo.")
                }
                break
            case 3:
                println("Saindo do programa...")
                return
            default:
                println("Opção inválida, por favor escolha uma opção válida.")
                break
        }
    }
}

main()
```

Explicação do código:

Este código em Groovy implementa um pequeno programa que oferece ao usuário algumas opções para calcular o fatorial de um número ou verificar se um número é primo. O programa utiliza um loop infinito para que o usuário possa escolher várias opções consecutivamente até decidir sair do programa.

A função `calculaFatorial` recebe um número inteiro como parâmetro e calcula o fatorial desse número utilizando um loop `for`. O fatorial de um número é o produto de todos os números inteiros positivos menores ou iguais ao número em questão. A função retorna o resultado do cálculo ou uma mensagem de erro caso o número seja negativo.

A função `verificaPrimo` recebe um número inteiro como parâmetro e verifica se esse número é primo. Um número primo é aquele que é divisível apenas por 1 e por ele mesmo, sem deixar resto. A função utiliza um loop `for` para verificar se o número é divisível por algum número entre 2 e a raiz quadrada do número em questão. Se o número for divisível por algum desses números, a função retorna `false`, caso contrário retorna `true`.

A função `main` é o ponto de entrada do programa. Ela exibe um menu de opções ao usuário e executa a ação correspondente à opção escolhida pelo usuário. O programa utiliza um loop `while (true)` para manter o menu exibido até que o usuário escolha a opção de sair. O programa lê a opção escolhida pelo usuário utilizando a classe `Scanner` e executa a ação correspondente ao `case` da opção escolhida.

Espero que este código atenda às suas expectativas!