Claro! Aqui está um código complexo em Groovy:

```groovy
// Este código é um exemplo de um sistema de gerenciamento de tarefas avançado com várias funcionalidades.

import java.util.ArrayList
import java.time.LocalDate

class Tarefa {
    String descricao
    LocalDate dataLimite
    boolean concluida

    Tarefa(String descricao, LocalDate dataLimite) {
        this.descricao = descricao
        this.dataLimite = dataLimite
        this.concluida = false
    }

    void concluir() {
        this.concluida = true
    }
}

class Projeto {
    String nome
    ArrayList<Tarefa> tarefas

    Projeto(String nome) {
        this.nome = nome
        this.tarefas = new ArrayList<Tarefa>()
    }

    void adicionarTarefa(String descricao, LocalDate dataLimite) {
        Tarefa novaTarefa = new Tarefa(descricao, dataLimite)
        tarefas.add(novaTarefa)
    }

    void removerTarefa(int indice) {
        if (indice >= 0 && indice < tarefas.size()) {
            tarefas.remove(indice)
        }
    }

    void concluirTarefa(int indice) {
        if (indice >= 0 && indice < tarefas.size()) {
            tarefas[indice].concluir()
        }
    }

    void exibirTarefas() {
        println("Tarefas do projeto $nome:")
        for (int i = 0; i < tarefas.size(); i++) {
            Tarefa tarefa = tarefas[i]
            String status = tarefa.concluida ? "Concluída" : "Pendente"
            println("[$i] ${tarefa.descricao} - Data Limite: ${tarefa.dataLimite} - Status: $status")
        }
    }
}

class GerenciadorTarefas {
    ArrayList<Projeto> projetos

    GerenciadorTarefas() {
        this.projetos = new ArrayList<Projeto>()
    }

    void adicionarProjeto(String nome) {
        Projeto novoProjeto = new Projeto(nome)
        projetos.add(novoProjeto)
    }

    void removerProjeto(int indice) {
        if (indice >= 0 && indice < projetos.size()) {
            projetos.remove(indice)
        }
    }

    Projeto obterProjeto(int indice) {
        if (indice >= 0 && indice < projetos.size()) {
            return projetos[indice]
        } else {
            return null
        }
    }

    void exibirProjetos() {
        println("Projetos:")
        for (int i = 0; i < projetos.size(); i++) {
            Projeto projeto = projetos[i]
            println("[$i] ${projeto.nome}")
        }
    }
}

def gerenciador = new GerenciadorTarefas()

gerenciador.adicionarProjeto("Projeto A")
gerenciador.adicionarProjeto("Projeto B")

def projetoA = gerenciador.obterProjeto(0)
def projetoB = gerenciador.obterProjeto(1)

projetoA.adicionarTarefa("Implementar autenticação de usuários", LocalDate.parse("2022-01-31"))
projetoA.adicionarTarefa("Criar tela de login", LocalDate.parse("2022-01-15"))

projetoB.adicionarTarefa("Desenvolver funcionalidade X", LocalDate.parse("2022-02-28"))
projetoB.adicionarTarefa("Corrigir bugs da versão 1.0", LocalDate.parse("2022-02-15"))

projetoA.concluirTarefa(0)
projetoB.concluirTarefa(1)

gerenciador.exibirProjetos()
projetoA.exibirTarefas()
projetoB.exibirTarefas()
```

Este código em Groovy é um exemplo de um sistema de gerenciamento de tarefas avançado com várias funcionalidades. Ele utiliza classes para representar tarefas e projetos. A classe `Tarefa` possui atributos como descrição, data limite e status de conclusão. A classe `Projeto` possui uma lista de tarefas e métodos para adicionar, remover e concluir tarefas, além de exibir as tarefas do projeto. A classe `GerenciadorTarefas` é responsável por gerenciar os projetos, permitindo adicionar, remover e obter projetos, além de exibir a lista de projetos.

No código principal, são criados dois projetos, "Projeto A" e "Projeto B", e são adicionadas algumas tarefas em cada projeto. Algumas tarefas são marcadas como concluídas. Por fim, são exibidos os projetos e suas respectivas tarefas.

Este código é apenas um exemplo e pode ser adaptado e ampliado de acordo com as necessidades específicas do projeto.