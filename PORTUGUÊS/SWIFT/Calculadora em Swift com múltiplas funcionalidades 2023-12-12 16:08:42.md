Certamente! Aqui está um código complexo em Swift que cria uma calculadora com várias funcionalidades:

```swift
import Foundation

enum Operacao {
    case soma, subtracao, multiplicacao, divisao, potencia
}

func realizarOperacao(numero1: Double, numero2: Double, operacao: Operacao) -> Double {
    switch operacao {
    case .soma:
        return numero1 + numero2
    case .subtracao:
        return numero1 - numero2
    case .multiplicacao:
        return numero1 * numero2
    case .divisao:
        guard numero2 != 0 else {
            print("Erro: Divisão por zero não é permitida!")
            return 0
        }
        return numero1 / numero2
    case .potencia:
        return pow(numero1, numero2)
    }
}

func mostrarMenu() {
    print("Bem-vindo à Calculadora!")
    print("Escolha a operação:")
    print("1 - Soma")
    print("2 - Subtração")
    print("3 - Multiplicação")
    print("4 - Divisão")
    print("5 - Potência")
    print("0 - Sair")
}

func lerNumero(mensagem: String) -> Double {
    print(mensagem, terminator: "")
    guard let input = readLine(), let numero = Double(input) else {
        print("Entrada inválida. Tente novamente.")
        return lerNumero(mensagem: mensagem)
    }
    return numero
}

var continua = true

while continua {
    mostrarMenu()
    
    guard let input = readLine(), let opcao = Int(input) else {
        print("Opção inválida. Tente novamente.")
        continue
    }
    
    switch opcao {
    case 0:
        continua = false
        print("Obrigado por usar a Calculadora!")
    case 1:
        let numero1 = lerNumero(mensagem: "Digite o primeiro número: ")
        let numero2 = lerNumero(mensagem: "Digite o segundo número: ")
        let resultado = realizarOperacao(numero1: numero1, numero2: numero2, operacao: .soma)
        print("O resultado da soma é: \(resultado)")
    case 2:
        let numero1 = lerNumero(mensagem: "Digite o primeiro número: ")
        let numero2 = lerNumero(mensagem: "Digite o segundo número: ")
        let resultado = realizarOperacao(numero1: numero1, numero2: numero2, operacao: .subtracao)
        print("O resultado da subtração é: \(resultado)")
    case 3:
        let numero1 = lerNumero(mensagem: "Digite o primeiro número: ")
        let numero2 = lerNumero(mensagem: "Digite o segundo número: ")
        let resultado = realizarOperacao(numero1: numero1, numero2: numero2, operacao: .multiplicacao)
        print("O resultado da multiplicação é: \(resultado)")
    case 4:
        let numero1 = lerNumero(mensagem: "Digite o primeiro número: ")
        let numero2 = lerNumero(mensagem: "Digite o segundo número: ")
        let resultado = realizarOperacao(numero1: numero1, numero2: numero2, operacao: .divisao)
        print("O resultado da divisão é: \(resultado)")
    case 5:
        let numero1 = lerNumero(mensagem: "Digite o número base: ")
        let numero2 = lerNumero(mensagem: "Digite o expoente: ")
        let resultado = realizarOperacao(numero1: numero1, numero2: numero2, operacao: .potencia)
        print("O resultado da potência é: \(resultado)")
    default:
        print("Opção inválida. Tente novamente.")
    }
}
```

Este código em Swift cria uma calculadora com as operações de soma, subtração, multiplicação, divisão e potência. Primeiro, definimos um enum `Operacao` com os casos correspondentes a cada operação matemática.

Em seguida, temos a função `realizarOperacao` que recebe dois números e a operação desejada, e retorna o resultado da operação. Ela utiliza um switch para realizar a operação correta de acordo com o caso selecionado.

O próximo bloco de código é responsável por mostrar o menu de opções para o usuário e ler a entrada do usuário para escolher a operação desejada. A função `lerNumero` é utilizada para ler um número válido da entrada do usuário.

Dentro do loop principal, utilizamos um switch para executar o bloco de código correspondente à opção escolhida pelo usuário. Cada caso chama a função `lerNumero` para obter os números necessários e então chama a função `realizarOperacao` para calcular o resultado. O resultado é então exibido na tela.

O loop continua até que o usuário escolha a opção 0 para sair do programa.

Espero que este código atenda às suas expectativas!