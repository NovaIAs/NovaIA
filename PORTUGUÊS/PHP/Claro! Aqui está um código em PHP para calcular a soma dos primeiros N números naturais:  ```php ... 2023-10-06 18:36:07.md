Claro! Aqui está um código complexo em PHP que pode ser usado para criar um sistema de gerenciamento de tarefas:

```php
<?php

// Classe Task representa uma tarefa com suas propriedades
class Task {
    private $id;
    private $titulo;
    private $descricao;
    private $dataCriacao;
    private $dataConclusao;
    private $status;

    // Construtor da classe Task
    public function __construct($id, $titulo, $descricao, $dataCriacao, $dataConclusao, $status) {
        $this->id = $id;
        $this->titulo = $titulo;
        $this->descricao = $descricao;
        $this->dataCriacao = $dataCriacao;
        $this->dataConclusao = $dataConclusao;
        $this->status = $status;
    }

    // Métodos getters e setters para acessar e modificar as propriedades da tarefa
    public function getId() {
        return $this->id;
    }

    public function getTitulo() {
        return $this->titulo;
    }

    public function setTitulo($titulo) {
        $this->titulo = $titulo;
    }

    public function getDescricao() {
        return $this->descricao;
    }

    public function setDescricao($descricao) {
        $this->descricao = $descricao;
    }

    public function getDataCriacao() {
        return $this->dataCriacao;
    }

    public function setDataCriacao($dataCriacao) {
        $this->dataCriacao = $dataCriacao;
    }

    public function getDataConclusao() {
        return $this->dataConclusao;
    }

    public function setDataConclusao($dataConclusao) {
        $this->dataConclusao = $dataConclusao;
    }

    public function getStatus() {
        return $this->status;
    }

    public function setStatus($status) {
        $this->status = $status;
    }
}

// Classe TaskManager gerencia as tarefas
class TaskManager {
    private $tasks;

    // Construtor da classe TaskManager
    public function __construct() {
        $this->tasks = array();
    }

    // Adiciona uma nova tarefa à lista de tarefas
    public function adicionarTarefa($tarefa) {
        $this->tasks[] = $tarefa;
    }

    // Retorna todas as tarefas
    public function getTarefas() {
        return $this->tasks;
    }

    // Retorna as tarefas concluídas
    public function getTarefasConcluidas() {
        $tarefasConcluidas = array();

        foreach ($this->tasks as $tarefa) {
            if ($tarefa->getStatus() == "Concluída") {
                $tarefasConcluidas[] = $tarefa;
            }
        }

        return $tarefasConcluidas;
    }

    // Retorna as tarefas pendentes
    public function getTarefasPendentes() {
        $tarefasPendentes = array();

        foreach ($this->tasks as $tarefa) {
            if ($tarefa->getStatus() == "Pendente") {
                $tarefasPendentes[] = $tarefa;
            }
        }

        return $tarefasPendentes;
    }
}

// Criando instâncias de tarefas
$tarefa1 = new Task(1, "Completar projeto", "Completar o projeto XYZ", "2022-01-01", "2022-01-10", "Concluída");
$tarefa2 = new Task(2, "Revisar código", "Revisar o código do projeto", "2022-01-02", "", "Pendente");
$tarefa3 = new Task(3, "Testar funcionalidades", "Testar as funcionalidades do projeto", "2022-01-03", "", "Pendente");

// Criando instância do gerenciador de tarefas
$gerenciadorTarefas = new TaskManager();

// Adicionando tarefas ao gerenciador
$gerenciadorTarefas->adicionarTarefa($tarefa1);
$gerenciadorTarefas->adicionarTarefa($tarefa2);
$gerenciadorTarefas->adicionarTarefa($tarefa3);

// Exibindo todas as tarefas
echo "Todas as tarefas:\n";
foreach ($gerenciadorTarefas->getTarefas() as $tarefa) {
    echo "ID: " . $tarefa->getId() . "\n";
    echo "Título: " . $tarefa->getTitulo() . "\n";
    echo "Descrição: " . $tarefa->getDescricao() . "\n";
    echo "Data de Criação: " . $tarefa->getDataCriacao() . "\n";
    echo "Data de Conclusão: " . $tarefa->getDataConclusao() . "\n";
    echo "Status: " . $tarefa->getStatus() . "\n";
    echo "\n";
}

// Exibindo tarefas concluídas
echo "Tarefas concluídas:\n";
foreach ($gerenciadorTarefas->getTarefasConcluidas() as $tarefa) {
    echo "ID: " . $tarefa->getId() . "\n";
    echo "Título: " . $tarefa->getTitulo() . "\n";
    echo "Descrição: " . $tarefa->getDescricao() . "\n";
    echo "Data de Criação: " . $tarefa->getDataCriacao() . "\n";
    echo "Data de Conclusão: " . $tarefa->getDataConclusao() . "\n";
    echo "Status: " . $tarefa->getStatus() . "\n";
    echo "\n";
}

// Exibindo tarefas pendentes
echo "Tarefas pendentes:\n";
foreach ($gerenciadorTarefas->getTarefasPendentes() as $tarefa) {
    echo "ID: " . $tarefa->getId() . "\n";
    echo "Título: " . $tarefa->getTitulo() . "\n";
    echo "Descrição: " . $tarefa->getDescricao() . "\n";
    echo "Data de Criação: " . $tarefa->getDataCriacao() . "\n";
    echo "Data de Conclusão: " . $tarefa->getDataConclusao() . "\n";
    echo "Status: " . $tarefa->getStatus() . "\n";
    echo "\n";
}

?>
```

Neste código, temos duas classes: `Task` (representando uma tarefa) e `TaskManager` (gerenciando as tarefas). A classe `Task` possui propriedades como ID, título, descrição, data de criação, data de conclusão e status. A classe `TaskManager` gerencia as tarefas, permitindo adicionar tarefas, retornar todas as tarefas, retornar as tarefas concluídas e retornar as tarefas pendentes.

No código de exemplo, são criadas três instâncias da classe `Task` e adicionadas ao `TaskManager`. Em seguida, as tarefas são exibidas separadamente: todas as tarefas, tarefas concluídas e tarefas pendentes.

Este código é apenas um exemplo de como poderia ser implementado um sistema de gerenciamento de tarefas em PHP. Ele pode ser adaptado e expandido de acordo com as necessidades específicas de um projeto.