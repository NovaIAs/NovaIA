Claro! Aqui está um exemplo de código em PHP que cria um sistema de gerenciamento de tarefas:

```php
<?php

// Classe para representar uma tarefa
class Tarefa {
    private $id;
    private $descricao;
    private $data;
    private $concluida;

    public function __construct($id, $descricao, $data, $concluida) {
        $this->id = $id;
        $this->descricao = $descricao;
        $this->data = $data;
        $this->concluida = $concluida;
    }

    public function getId() {
        return $this->id;
    }

    public function getDescricao() {
        return $this->descricao;
    }

    public function getData() {
        return $this->data;
    }

    public function isConcluida() {
        return $this->concluida;
    }
}

// Classe para gerenciar as tarefas
class GerenciadorTarefas {
    private $tarefas;

    public function __construct() {
        $this->tarefas = array();
    }

    public function adicionarTarefa($descricao, $data) {
        $id = count($this->tarefas) + 1;
        $concluida = false;
        $tarefa = new Tarefa($id, $descricao, $data, $concluida);
        $this->tarefas[] = $tarefa;
    }

    public function listarTarefas() {
        foreach ($this->tarefas as $tarefa) {
            $concluida = ($tarefa->isConcluida()) ? "Sim" : "Não";
            echo "ID: " . $tarefa->getId() . "<br>";
            echo "Descrição: " . $tarefa->getDescricao() . "<br>";
            echo "Data: " . $tarefa->getData() . "<br>";
            echo "Concluída: " . $concluida . "<br><br>";
        }
    }

    public function concluirTarefa($id) {
        foreach ($this->tarefas as $tarefa) {
            if ($tarefa->getId() == $id) {
                $tarefa->concluida = true;
                echo "Tarefa ID " . $id . " concluída com sucesso!";
                return;
            }
        }
        echo "Tarefa ID " . $id . " não encontrada.";
    }
}

// Exemplo de uso
$gerenciador = new GerenciadorTarefas();
$gerenciador->adicionarTarefa("Comprar leite", "2021-05-01");
$gerenciador->adicionarTarefa("Pagar conta de luz", "2021-05-02");
$gerenciador->adicionarTarefa("Estudar programação", "2021-05-03");

$gerenciador->listarTarefas();

$gerenciador->concluirTarefa(2);

$gerenciador->listarTarefas();

?>
```

Neste código, temos duas classes principais: `Tarefa` e `GerenciadorTarefas`. 

A classe `Tarefa` é utilizada para representar uma única tarefa, com seus atributos como id, descrição, data e se foi concluída ou não. Ela possui métodos getters para acessar esses atributos.

A classe `GerenciadorTarefas` é responsável por gerenciar as tarefas. Ela possui um array de objetos `Tarefa` e métodos para adicionar tarefas, listar todas as tarefas e concluir uma tarefa específica.

No exemplo de uso no final do código, criamos um objeto `GerenciadorTarefas`, adicionamos algumas tarefas, listamos todas elas, concluímos a segunda tarefa e, por fim, listamos novamente para verificar a atualização.

Espero que esse código complexo e diferenciado atenda às suas expectativas!