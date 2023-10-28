Claro! Aqui está um exemplo de um código em PHP que implementa um sistema de gerenciamento de tarefas:

```php
<?php

// Classe para representar uma tarefa
class Tarefa {
    private $id;
    private $titulo;
    private $descricao;
    private $dataCriacao;
    private $dataConclusao;

    public function __construct($id, $titulo, $descricao, $dataCriacao, $dataConclusao) {
        $this->id = $id;
        $this->titulo = $titulo;
        $this->descricao = $descricao;
        $this->dataCriacao = $dataCriacao;
        $this->dataConclusao = $dataConclusao;
    }

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

    public function getDataConclusao() {
        return $this->dataConclusao;
    }

    public function setDataConclusao($dataConclusao) {
        $this->dataConclusao = $dataConclusao;
    }
}

// Classe para representar o sistema de gerenciamento de tarefas
class GerenciadorTarefas {
    private $tarefas = [];

    // Adicionar uma nova tarefa
    public function adicionarTarefa($tarefa) {
        $this->tarefas[] = $tarefa;
    }

    // Remover uma tarefa pelo ID
    public function removerTarefa($id) {
        foreach ($this->tarefas as $index => $tarefa) {
            if ($tarefa->getId() == $id) {
                unset($this->tarefas[$index]);
                break;
            }
        }
    }

    // Listar todas as tarefas
    public function listarTarefas() {
        foreach ($this->tarefas as $tarefa) {
            echo "ID: " . $tarefa->getId() . "\n";
            echo "Título: " . $tarefa->getTitulo() . "\n";
            echo "Descrição: " . $tarefa->getDescricao() . "\n";
            echo "Data de Criação: " . $tarefa->getDataCriacao() . "\n";
            echo "Data de Conclusão: " . $tarefa->getDataConclusao() . "\n";
            echo "-----------------------\n";
        }
    }
}

// Exemplo de uso do sistema de gerenciamento de tarefas
$gerenciador = new GerenciadorTarefas();

$tarefa1 = new Tarefa(1, "Fazer compras", "Comprar leite e pão", "2021-01-01", null);
$tarefa2 = new Tarefa(2, "Estudar programação", "Estudar PHP e JavaScript", "2021-02-01", null);

$gerenciador->adicionarTarefa($tarefa1);
$gerenciador->adicionarTarefa($tarefa2);

$gerenciador->listarTarefas();

$gerenciador->removerTarefa(1);

$gerenciador->listarTarefas();
```

Nesse código, temos duas classes principais: `Tarefa` e `GerenciadorTarefas`. A classe `Tarefa` representa uma tarefa individual, contendo informações como ID, título, descrição, data de criação e data de conclusão. A classe `GerenciadorTarefas` é responsável por adicionar, remover e listar as tarefas.

No exemplo de uso, criamos duas tarefas e as adicionamos ao gerenciador de tarefas. Em seguida, listamos todas as tarefas e removemos a primeira tarefa. Por fim, listamos as tarefas novamente para verificar as mudanças.

Esse código é um exemplo simples de um sistema de gerenciamento de tarefas em PHP. É importante ressaltar que existem diversas maneiras de implementar um sistema como esse, e esse código pode ser adaptado e expandido de acordo com as necessidades específicas do projeto.