Claro! Aqui está um exemplo de código complexo em Groovy:

```groovy
import groovy.transform.CompileStatic

@CompileStatic
class Empresa {
    String nome
    List<Funcionario> funcionarios

    void adicionarFuncionario(Funcionario novoFuncionario) {
        if (!funcionarios.contains(novoFuncionario)) {
            funcionarios.add(novoFuncionario)
        } else {
            println("O funcionário ${novoFuncionario.nome} já está registrado na empresa.")
        }
    }

    void removerFuncionario(Funcionario funcionario) {
        if (funcionarios.contains(funcionario)) {
            funcionarios.remove(funcionario)
            println("O funcionário ${funcionario.nome} foi removido.")
        } else {
            println("O funcionário ${funcionario.nome} não está registrado na empresa.")
        }
    }
}

@CompileStatic
class Funcionario {
    String nome
    String cargo
    Double salario

    Funcionario(String nome, String cargo, Double salario) {
        this.nome = nome
        this.cargo = cargo
        this.salario = salario
    }

    String toString() {
        return "$nome - $cargo - R$$salario"
    }
}

def empresa = new Empresa(nome: "Minha Empresa", funcionarios: [])

def funcionario1 = new Funcionario("João", "Desenvolvedor", 5000.0)
def funcionario2 = new Funcionario("Maria", "Gerente", 8000.0)
def funcionario3 = new Funcionario("Pedro", "Analista", 6000.0)

empresa.adicionarFuncionario(funcionario1)
empresa.adicionarFuncionario(funcionario2)
empresa.adicionarFuncionario(funcionario3)

empresa.removerFuncionario(funcionario2)

println("Funcionários da empresa:")
empresa.funcionarios.each { funcionario ->
    println(funcionario)
}
```

Neste código, criamos duas classes em Groovy: "Empresa" e "Funcionario". A classe "Empresa" possui um nome e uma lista de funcionários. Ela possui métodos para adicionar e remover funcionários dessa lista.

A classe "Funcionario" possui um nome, cargo e salário. Ela também possui um construtor para inicializar esses atributos.

No código principal, criamos uma instância da classe "Empresa" chamada "empresa". Em seguida, criamos três instâncias da classe "Funcionario" chamadas "funcionario1", "funcionario2" e "funcionario3".

Utilizamos o método "adicionarFuncionario" da classe "Empresa" para adicionar os funcionários à lista da empresa. Em seguida, utilizamos o método "removerFuncionario" para remover o funcionário "funcionario2" da lista.

Por fim, imprimimos a lista de funcionários da empresa utilizando um loop "each" e o método "toString" da classe "Funcionario".

Espero que esse código atenda às suas expectativas!