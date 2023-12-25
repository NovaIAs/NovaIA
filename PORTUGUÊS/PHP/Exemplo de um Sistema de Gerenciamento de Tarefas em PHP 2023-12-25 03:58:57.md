Claro! Aqui está um exemplo de um código em PHP que cria um sistema de gerenciamento de tarefas simples em um ambiente de trabalho:

```php
<?php

// Classe para representar uma tarefa
class Tarefa
{
    private $id;
    private $descricao;
    private $concluida;

    public function __construct($id, $descricao, $concluida)
    {
        $this->id = $id;
        $this->descricao = $descricao;
        $this->concluida = $concluida;
    }

    public function getId()
    {
        return $this->id;
    }

    public function getDescricao()
    {
        return $this->descricao;
    }

    public function isConcluida()
    {
        return $this->concluida;
    }
}

// Classe para representar um gerenciador de tarefas
class GerenciadorTarefas
{
    private $tarefas = array();

    public function adicionarTarefa($tarefa)
    {
        $this->tarefas[] = $tarefa;
    }

    public function listarTarefas()
    {
        echo "Lista de Tarefas:\n\n";

        foreach ($this->tarefas as $tarefa) {
            $status = $tarefa->isConcluida() ? "[Concluída]" : "[Pendente]";
            echo $tarefa->getId() . ": " . $tarefa->getDescricao() . " " . $status . "\n";
        }
    }

    public function marcarTarefaConcluida($id)
    {
        foreach ($this->tarefas as $tarefa) {
            if ($tarefa->getId() == $id) {
                $tarefa->concluida = true;
                echo "Tarefa " . $tarefa->getId() . " marcada como concluída.\n";
                return;
            }
        }

        echo "Tarefa não encontrada.\n";
    }
}

// Criar algumas tarefas
$tarefa1 = new Tarefa(1, "Ler um livro", false);
$tarefa2 = new Tarefa(2, "Fazer exercícios físicos", false);
$tarefa3 = new Tarefa(3, "Estudar programação", true);

// Criar o gerenciador de tarefas
$gerenciadorTarefas = new GerenciadorTarefas();

// Adicionar as tarefas ao gerenciador
$gerenciadorTarefas->adicionarTarefa($tarefa1);
$gerenciadorTarefas->adicionarTarefa($tarefa2);
$gerenciadorTarefas->adicionarTarefa($tarefa3);

// Listar as tarefas
$gerenciadorTarefas->listarTarefas();

// Marcar a tarefa 1 como concluída
$gerenciadorTarefas->marcarTarefaConcluida(1);

// Listar as tarefas novamente
$gerenciadorTarefas->listarTarefas();
```

Neste exemplo, temos duas classes: `Tarefa` e `GerenciadorTarefas`. A classe `Tarefa` representa uma tarefa com atributos como identificador, descrição e status de conclusão. A classe `GerenciadorTarefas` é responsável por adicionar, listar e marcar tarefas como concluídas.

O código cria algumas instâncias da classe `Tarefa` e as adiciona ao `GerenciadorTarefas`. Em seguida, lista as tarefas e marca a primeira como concluída. Por fim, lista as tarefas novamente para mostrar a atualização.

Esse é apenas um exemplo básico de um sistema de gerenciamento de tarefas em PHP. Você pode expandir e adicionar mais funcionalidades conforme necessário.