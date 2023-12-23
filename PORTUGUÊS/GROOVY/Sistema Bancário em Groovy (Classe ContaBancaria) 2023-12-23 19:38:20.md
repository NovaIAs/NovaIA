Claro! Aqui está um exemplo de um código complexo em Groovy:

```groovy
import java.util.concurrent.TimeUnit

class ContaBancaria {
    String nomeTitular
    double saldo

    void depositar(double valor) {
        saldo += valor
        println("Depósito de R$ ${valor} realizado com sucesso na conta de ${nomeTitular}.")
    }

    void sacar(double valor) {
        if (saldo >= valor) {
            saldo -= valor
            println("Saque de R$ ${valor} realizado com sucesso na conta de ${nomeTitular}.")
        } else {
            println("Saldo insuficiente na conta de ${nomeTitular}.")
        }
    }

    void transferir(double valor, ContaBancaria contaDestino) {
        if (saldo >= valor) {
            saldo -= valor
            contaDestino.depositar(valor)
            println("Transferência de R$ ${valor} realizada com sucesso da conta de ${nomeTitular} para a conta de ${contaDestino.nomeTitular}.")
        } else {
            println("Saldo insuficiente na conta de ${nomeTitular}.")
        }
    }
}

def conta1 = new ContaBancaria()
conta1.nomeTitular = "João"
conta1.saldo = 1000.0

def conta2 = new ContaBancaria()
conta2.nomeTitular = "Maria"
conta2.saldo = 500.0

conta1.depositar(200.0)
conta1.sacar(150.0)
conta1.transferir(300.0, conta2)

println("Saldo da conta de ${conta1.nomeTitular}: R$ ${conta1.saldo}.")
println("Saldo da conta de ${conta2.nomeTitular}: R$ ${conta2.saldo}.")
```

Neste código, temos uma classe `ContaBancaria` que representa uma conta bancária. Ela possui os atributos `nomeTitular` (que armazena o nome do titular da conta) e `saldo` (que armazena o saldo atual da conta).

A classe `ContaBancaria` também possui três métodos: `depositar`, `sacar` e `transferir`. O método `depositar` recebe um valor e adiciona esse valor ao saldo da conta. O método `sacar` recebe um valor e verifica se o saldo é suficiente para efetuar o saque. Se for, subtrai o valor do saldo; caso contrário, exibe uma mensagem de saldo insuficiente. O método `transferir` recebe um valor e uma conta de destino. Ele verifica se o saldo é suficiente para efetuar a transferência. Se for, subtrai o valor do saldo da conta de origem e adiciona o valor ao saldo da conta de destino. Caso contrário, exibe uma mensagem de saldo insuficiente.

No código principal, criamos duas contas bancárias (`conta1` e `conta2`) e definimos seus titulares e saldos iniciais. Em seguida, realizamos algumas operações, como depósito, saque e transferência entre as contas. Por fim, exibimos os saldos atualizados das contas.

Este código simula um sistema bancário simples, demonstrando como criar e manipular objetos em Groovy. Espero que seja útil para você!