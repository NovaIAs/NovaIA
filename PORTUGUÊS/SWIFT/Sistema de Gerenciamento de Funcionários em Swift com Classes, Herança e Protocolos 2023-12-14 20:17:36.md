Certamente! Aqui está um código complexo em Swift, que faz uso de classes, herança, protocolos e closures para criar um sistema de gerenciamento de funcionários em uma empresa:

```swift
// Definindo a classe base Funcionario
class Funcionario {
    var nome: String
    var salario: Double
    
    init(nome: String, salario: Double) {
        self.nome = nome
        self.salario = salario
    }
    
    func calcularSalario() -> Double {
        return salario
    }
}

// Definindo a classe Gerente que herda de Funcionario
class Gerente: Funcionario {
    var bonus: Double
    
    init(nome: String, salario: Double, bonus: Double) {
        self.bonus = bonus
        super.init(nome: nome, salario: salario)
    }
    
    override func calcularSalario() -> Double {
        return salario + bonus
    }
}

// Definindo a classe Desenvolvedor que herda de Funcionario e implementa o protocolo Equatable
class Desenvolvedor: Funcionario, Equatable {
    static func == (lhs: Desenvolvedor, rhs: Desenvolvedor) -> Bool {
        return lhs.nome == rhs.nome && lhs.salario == rhs.salario
    }
}

// Implementando um protocolo para gerenciar a equipe de funcionários
protocol Equipe {
    associatedtype T: Funcionario
    
    var membros: [T] { get set }
    
    func adicionarMembro(_ membro: T)
    func removerMembro(_ membro: T)
    func listarMembros()
}

// Implementando o protocolo Equipe para a classe Gerente
extension Gerente: Equipe {
    var membros: [Desenvolvedor] {
        get { return [] }
        set {}
    }
    
    func adicionarMembro(_ membro: Desenvolvedor) {
        membros.append(membro)
    }
    
    func removerMembro(_ membro: Desenvolvedor) {
        membros = membros.filter { $0 != membro }
    }
    
    func listarMembros() {
        print("Membros da equipe do gerente \(nome):")
        for membro in membros {
            print(membro.nome)
        }
    }
}

// Implementando o protocolo Equipe para a classe Desenvolvedor
extension Desenvolvedor: Equipe {
    var membros: [Desenvolvedor] {
        get { return [] }
        set {}
    }
    
    func adicionarMembro(_ membro: Desenvolvedor) {
        // Não é possível adicionar membros para um desenvolvedor
        print("Um desenvolvedor não pode ter membros na equipe.")
    }
    
    func removerMembro(_ membro: Desenvolvedor) {
        // Não é possível remover membros para um desenvolvedor
        print("Um desenvolvedor não pode ter membros na equipe.")
    }
    
    func listarMembros() {
        // Não existem membros para um desenvolvedor
        print("Um desenvolvedor não tem membros na equipe.")
    }
}

// Criando instâncias de Gerente
let gerente1 = Gerente(nome: "João", salario: 5000, bonus: 1000)
let gerente2 = Gerente(nome: "Maria", salario: 6000, bonus: 1500)

// Criando instâncias de Desenvolvedor
let dev1 = Desenvolvedor(nome: "Pedro", salario: 3000)
let dev2 = Desenvolvedor(nome: "Ana", salario: 3500)
let dev3 = Desenvolvedor(nome: "Carlos", salario: 4000)

// Adicionando membros à equipe de cada gerente
gerente1.adicionarMembro(dev1)
gerente1.adicionarMembro(dev2)
gerente2.adicionarMembro(dev3)

// Listando os membros de cada equipe
gerente1.listarMembros()
gerente2.listarMembros()
```

Este código em Swift apresenta uma estrutura de classes para gerenciar funcionários em uma empresa, com foco em gerentes e desenvolvedores. A classe `Funcionario` é a classe base, contendo propriedades como `nome` e `salario`, e o método `calcularSalario()`.

A classe `Gerente` herda da classe `Funcionario` e adiciona a propriedade `bonus`, além de sobrescrever o método `calcularSalario()` para incluir o bônus no cálculo do salário final.

A classe `Desenvolvedor` também herda da classe `Funcionario` e implementa o protocolo `Equatable`, o que permite comparar objetos do tipo `Desenvolvedor` com base no nome e no salário.

O protocolo `Equipe` é utilizado para gerenciar a equipe de funcionários de um gerente ou desenvolvedor. Cada classe que desejar ter uma equipe deve implementar esse protocolo. No caso do `Gerente`, o protocolo é implementado usando um array de `Desenvolvedor`, enquanto no caso do `Desenvolvedor`, o protocolo é implementado para mostrar que um desenvolvedor não tem membros na equipe.

Por fim, o código cria instâncias de gerentes e desenvolvedores, adiciona membros às equipes de cada gerente e lista os membros de cada equipe.

Espero que este código complexo em Swift atenda às suas expectativas!