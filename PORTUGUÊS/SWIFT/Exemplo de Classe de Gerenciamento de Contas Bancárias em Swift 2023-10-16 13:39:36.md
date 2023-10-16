Claro! Aqui está um exemplo de um código complexo em Swift que envolve a criação de uma classe de gerenciamento de contas bancárias:

```swift
import Foundation

// Definindo a classe Account para representar uma conta bancária
class Account {
    var accountNumber: String
    var balance: Double
    
    init(accountNumber: String, balance: Double) {
        self.accountNumber = accountNumber
        self.balance = balance
    }
    
    // Método para depositar dinheiro na conta
    func deposit(amount: Double) {
        balance += amount
        print("Depósito de \(amount) realizado com sucesso. Saldo atual: \(balance)")
    }
    
    // Método para sacar dinheiro da conta
    func withdraw(amount: Double) {
        if balance >= amount {
            balance -= amount
            print("Saque de \(amount) realizado com sucesso. Saldo atual: \(balance)")
        } else {
            print("Saldo insuficiente para o saque.")
        }
    }
    
    // Método para transferir dinheiro para outra conta
    func transfer(amount: Double, to account: Account) {
        if balance >= amount {
            balance -= amount
            account.balance += amount
            print("Transferência de \(amount) realizada com sucesso para a conta \(account.accountNumber). Saldo atual: \(balance)")
        } else {
            print("Saldo insuficiente para a transferência.")
        }
    }
}

// Criando duas contas bancárias
let account1 = Account(accountNumber: "123456789", balance: 1000)
let account2 = Account(accountNumber: "987654321", balance: 500)

// Realizando operações nas contas
account1.deposit(amount: 500)
account1.withdraw(amount: 200)
account1.transfer(amount: 300, to: account2)
```

Neste código, definimos uma classe chamada `Account` que representa uma conta bancária. Ela possui duas propriedades: `accountNumber`, que armazena o número da conta, e `balance`, que armazena o saldo atual da conta.

Em seguida, implementamos três métodos principais para a classe `Account`:

- O método `deposit` permite depositar dinheiro na conta, aumentando o saldo atual.
- O método `withdraw` permite sacar dinheiro da conta, desde que o saldo seja suficiente.
- O método `transfer` permite transferir dinheiro de uma conta para outra, desde que o saldo seja suficiente na conta de origem.

No final do código, criamos duas instâncias da classe `Account` para representar duas contas bancárias diferentes. Em seguida, realizamos algumas operações, como depósito, saque e transferência entre as contas, para demonstrar o funcionamento da classe.

Espero que este exemplo complexo tenha sido útil para ilustrar a criação de uma classe de gerenciamento de contas bancárias em Swift.