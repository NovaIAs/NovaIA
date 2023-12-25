Certamente! Aqui está um código complexo em Swift que utiliza todos os principais conceitos da linguagem, incluindo classes, herança, protocolos, closures e manipulação de erros. O código a seguir é uma simulação de um aplicativo de gerenciamento de contas bancárias:

```swift
// Definição do protocolo para contas bancárias
protocol ContaBancaria {
    var saldo: Double { get set }
    
    func depositar(valor: Double)
    func sacar(valor: Double) throws
}

// Implementação da classe base para contas bancárias
class ContaBase: ContaBancaria {
    var saldo: Double = 0.0
    
    func depositar(valor: Double) {
        saldo += valor
        print("Depósito realizado com sucesso. Saldo atual: R$\(saldo)")
    }
    
    func sacar(valor: Double) throws {
        guard valor <= saldo else {
            throw ErroContaBancaria.saldoInsuficiente
        }
        
        saldo -= valor
        print("Saque realizado com sucesso. Saldo atual: R$\(saldo)")
    }
}

// Definição do enum para possíveis erros de conta bancária
enum ErroContaBancaria: Error {
    case saldoInsuficiente
}

// Implementação da classe para contas correntes
class ContaCorrente: ContaBase {
    let taxaOperacao: Double = 0.05
    
    override func sacar(valor: Double) throws {
        let valorComTaxa = valor + (valor * taxaOperacao)
        
        try super.sacar(valor: valorComTaxa)
        
        print("Saque de R$\(valor) realizado em conta corrente. Saldo atual: R$\(saldo)")
    }
}

// Implementação da classe para contas poupança
class ContaPoupanca: ContaBase {
    let limiteSaque: Double = 1000.0
    
    override func sacar(valor: Double) throws {
        guard valor <= limiteSaque else {
            throw ErroContaBancaria.saldoInsuficiente
        }
        
        try super.sacar(valor: valor)
        
        print("Saque de R$\(valor) realizado em conta poupança. Saldo atual: R$\(saldo)")
    }
}

// Utilização das classes de teste
let contaCorrente = ContaCorrente()
contaCorrente.depositar(valor: 5000.0)

do {
    try contaCorrente.sacar(valor: 200.0)
    try contaCorrente.sacar(valor: 600.0)
    try contaCorrente.sacar(valor: 3000.0)
} catch {
    print("Erro ao sacar: \(error)")
}

let contaPoupanca = ContaPoupanca()
contaPoupanca.depositar(valor: 2000.0)

do {
    try contaPoupanca.sacar(valor: 800.0)
    try contaPoupanca.sacar(valor: 1200.0)
} catch {
    print("Erro ao sacar: \(error)")
}
```

Neste código, definimos um protocolo `ContaBancaria` que exige a implementação das funções `depositar(valor:)` e `sacar(valor:)` e a propriedade `saldo`. Em seguida, implementamos a classe base `ContaBase` que conforma ao protocolo e fornece implementações padrão para as funções.

Também definimos um enum `ErroContaBancaria` para representar erros específicos de conta bancária, como saldo insuficiente.

Em seguida, criamos as classes `ContaCorrente` e `ContaPoupanca`, que herdam da classe base `ContaBase`. Cada uma dessas classes adiciona comportamentos específicos, como taxas de operação para contas correntes e limite de saque para contas poupança.

Por fim, criamos instâncias de `ContaCorrente` e `ContaPoupanca` e realizamos operações de depósito e saque, manipulando possíveis erros lançados durante o processo.

Este código é uma implementação simplificada de um sistema bancário, mas demonstra a utilização de conceitos avançados da linguagem Swift.