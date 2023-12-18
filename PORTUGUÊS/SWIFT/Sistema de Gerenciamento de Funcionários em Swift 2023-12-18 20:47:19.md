Claro! Aqui está um código complexo em Swift, que faz uso de diferentes recursos da linguagem e é bastante extenso. O código é um exemplo de um sistema de gerenciamento de funcionários, utilizando orientação a objetos e padrões de design.

```swift
import Foundation

// Definindo uma classe base para os funcionários
class Funcionario {
    var nome: String
    var salario: Double
    
    init(nome: String, salario: Double) {
        self.nome = nome
        self.salario = salario
    }
    
    func calcularSalarioMensal() -> Double {
        return salario
    }
}

// Definindo uma classe específica para os funcionários em tempo integral
class FuncionarioTempoIntegral: Funcionario {
    var bonus: Double
    
    init(nome: String, salario: Double, bonus: Double) {
        self.bonus = bonus
        super.init(nome: nome, salario: salario)
    }
    
    override func calcularSalarioMensal() -> Double {
        return salario + bonus
    }
}

// Definindo uma classe específica para os funcionários em meio período
class FuncionarioMeioPeriodo: Funcionario {
    var horasTrabalhadas: Int
    
    init(nome: String, salario: Double, horasTrabalhadas: Int) {
        self.horasTrabalhadas = horasTrabalhadas
        super.init(nome: nome, salario: salario)
    }
    
    override func calcularSalarioMensal() -> Double {
        let salarioPorHora = salario / 160
        return salarioPorHora * Double(horasTrabalhadas)
    }
}

// Criando uma lista de funcionários
var funcionarios: [Funcionario] = []

// Adicionando funcionários em tempo integral
let funcionario1 = FuncionarioTempoIntegral(nome: "João", salario: 5000, bonus: 1000)
let funcionario2 = FuncionarioTempoIntegral(nome: "Maria", salario: 6000, bonus: 2000)
funcionarios.append(funcionario1)
funcionarios.append(funcionario2)

// Adicionando funcionários em meio período
let funcionario3 = FuncionarioMeioPeriodo(nome: "Pedro", salario: 20, horasTrabalhadas: 80)
let funcionario4 = FuncionarioMeioPeriodo(nome: "Ana", salario: 25, horasTrabalhadas: 120)
funcionarios.append(funcionario3)
funcionarios.append(funcionario4)

// Calculando e exibindo o salário mensal de cada funcionário
for funcionario in funcionarios {
    let salarioMensal = funcionario.calcularSalarioMensal()
    print("\(funcionario.nome): R$ \(salarioMensal)")
}
```

Neste código, temos três classes: `Funcionario` (classe base abstrata), `FuncionarioTempoIntegral` e `FuncionarioMeioPeriodo`. Cada uma dessas classes representa um tipo de funcionário com suas particularidades salariais.

Os funcionários em tempo integral possuem um salário base e um bônus adicional. Já os funcionários em meio período têm um salário por hora e a quantidade de horas trabalhadas. Ambas as classes implementam o método `calcularSalarioMensal()`, que retorna o salário mensal de cada funcionário.

Em seguida, são criados objetos de funcionários em tempo integral e em meio período, e eles são adicionados a uma lista de funcionários.

Por fim, um loop é usado para calcular e exibir o salário mensal de cada funcionário da lista.

Espero que esse código complexo em Swift atenda às suas expectativas!