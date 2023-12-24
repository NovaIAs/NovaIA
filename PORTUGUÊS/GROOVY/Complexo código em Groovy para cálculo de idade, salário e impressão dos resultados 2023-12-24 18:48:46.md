Claro! Aqui está um código complexo em Groovy que realiza uma série de tarefas complexas e distintas:

```groovy
import java.time.LocalDate
import java.time.Period

class Pessoa {
    String nome
    LocalDate dataNascimento
    
    Pessoa(String nome, LocalDate dataNascimento) {
        this.nome = nome
        this.dataNascimento = dataNascimento
    }
    
    int calcularIdade() {
        LocalDate hoje = LocalDate.now()
        Period periodo = Period.between(dataNascimento, hoje)
        return periodo.years
    }
}

interface Funcionario {
    double calcularSalario()
}

class Gerente extends Pessoa implements Funcionario {
    double salarioBase
    double bonus
    
    Gerente(String nome, LocalDate dataNascimento, double salarioBase, double bonus) {
        super(nome, dataNascimento)
        this.salarioBase = salarioBase
        this.bonus = bonus
    }
    
    @Override
    double calcularSalario() {
        return salarioBase + bonus
    }
}

class Desenvolvedor extends Pessoa implements Funcionario {
    double salarioHora
    int horasTrabalhadas
    
    Desenvolvedor(String nome, LocalDate dataNascimento, double salarioHora, int horasTrabalhadas) {
        super(nome, dataNascimento)
        this.salarioHora = salarioHora
        this.horasTrabalhadas = horasTrabalhadas
    }
    
    @Override
    double calcularSalario() {
        return salarioHora * horasTrabalhadas
    }
}

Pessoa pessoa1 = new Pessoa("João", LocalDate.of(1990, 5, 15))
Gerente gerente1 = new Gerente("Maria", LocalDate.of(1985, 10, 20), 5000, 1000)
Desenvolvedor desenvolvedor1 = new Desenvolvedor("Pedro", LocalDate.of(1995, 12, 8), 50, 160)

println("Nome: ${pessoa1.nome}")
println("Idade: ${pessoa1.calcularIdade()} anos")

println("Nome: ${gerente1.nome}")
println("Idade: ${gerente1.calcularIdade()} anos")
println("Salário: R$ ${gerente1.calcularSalario()}")

println("Nome: ${desenvolvedor1.nome}")
println("Idade: ${desenvolvedor1.calcularIdade()} anos")
println("Salário: R$ ${desenvolvedor1.calcularSalario()}")
```

Neste código, temos a definição de três classes: `Pessoa`, `Gerente` e `Desenvolvedor`. A classe `Pessoa` possui um nome e uma data de nascimento, e possui um método `calcularIdade()` que retorna a idade da pessoa com base na data de nascimento.

A classe `Gerente` é uma subclasse de `Pessoa` e também implementa a interface `Funcionario`. Ela possui um salário base e um bônus, e implementa o método `calcularSalario()` para retornar a soma do salário base com o bônus.

A classe `Desenvolvedor` é outra subclasse de `Pessoa` e também implementa a interface `Funcionario`. Ela possui um salário por hora e o número de horas trabalhadas, e implementa o método `calcularSalario()` para retornar o valor do salário com base no salário por hora e nas horas trabalhadas.

No final do código, são criados três objetos: `pessoa1`, `gerente1` e `desenvolvedor1`, e são impressos na tela o nome, a idade e o salário de cada um.

Espero que esse código complexo e diferenciado atenda às suas expectativas!