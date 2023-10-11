Claro! Aqui está um exemplo de código PHP complexo que envolve a criação de um sistema de gerenciamento de tarefas:

```php
<?php

class Tarefa {
    private $id;
    private $titulo;
    private $descricao;
    private $dataCriacao;
    private $dataConclusao;
    private $prioridade;
    private $status;
    
    public function __construct($id, $titulo, $descricao, $dataCriacao, $prioridade) {
        $this->id = $id;
        $this->titulo = $titulo;
        $this->descricao = $descricao;
        $this->dataCriacao = $dataCriacao;
        $this->prioridade = $prioridade;
        $this->status = 'Pendente';
    }
    
    public function concluirTarefa() {
        $this->status = 'Concluída';
        $this->dataConclusao = date('Y-m-d H:i:s');
    }
    
    public function exibirTarefa() {
        echo 'ID: ' . $this->id . '<br>';
        echo 'Título: ' . $this->titulo . '<br>';
        echo 'Descrição: ' . $this->descricao . '<br>';
        echo 'Data de Criação: ' . $this->dataCriacao . '<br>';
        echo 'Data de Conclusão: ' . $this->dataConclusao . '<br>';
        echo 'Prioridade: ' . $this->prioridade . '<br>';
        echo 'Status: ' . $this->status . '<br>';
    }
}

class GerenciadorTarefas {
    private $tarefas;
    
    public function __construct() {
        $this->tarefas = [];
    }
    
    public function adicionarTarefa($tarefa) {
        $this->tarefas[] = $tarefa;
    }
    
    public function concluirTarefa($id) {
        foreach ($this->tarefas as $tarefa) {
            if ($tarefa->getId() === $id) {
                $tarefa->concluirTarefa();
                break;
            }
        }
    }
    
    public function exibirTarefas() {
        foreach ($this->tarefas as $tarefa) {
            $tarefa->exibirTarefa();
            echo '<br>';
        }
    }
}

// Utilização do sistema

// Criando algumas tarefas
$tarefa1 = new Tarefa(1, 'Comprar mantimentos', 'Ir ao supermercado e comprar comida para a semana', '2022-06-01', 'Alta');
$tarefa2 = new Tarefa(2, 'Pagar contas', 'Realizar o pagamento das contas de luz, água e telefone', '2022-06-02', 'Média');
$tarefa3 = new Tarefa(3, 'Estudar programação', 'Dedicar 2 horas diárias para estudar programação', '2022-06-03', 'Baixa');

// Criando o gerenciador de tarefas
$gerenciador = new GerenciadorTarefas();

// Adicionando as tarefas ao gerenciador
$gerenciador->adicionarTarefa($tarefa1);
$gerenciador->adicionarTarefa($tarefa2);
$gerenciador->adicionarTarefa($tarefa3);

// Concluindo a primeira tarefa
$gerenciador->concluirTarefa(1);

// Exibindo todas as tarefas
$gerenciador->exibirTarefas();

?>
```

Neste código, temos duas classes principais: `Tarefa` e `GerenciadorTarefas`. A classe `Tarefa` representa cada tarefa individual, com seus atributos como ID, título, descrição, data de criação, data de conclusão, prioridade e status. A classe `GerenciadorTarefas` é responsável por gerenciar as tarefas, permitindo adicionar tarefas, concluir tarefas e exibir todas as tarefas.

No exemplo de uso do sistema, são criadas três tarefas diferentes, adicionadas ao gerenciador de tarefas. Em seguida, a primeira tarefa é concluída usando o método `concluirTarefa()`. Por fim, todas as tarefas são exibidas usando o método `exibirTarefas()`, que percorre a lista de tarefas e exibe os detalhes de cada uma delas.

Este código é apenas um exemplo complexo que pode ser personalizado e expandido de acordo com suas necessidades. Espero que isso tenha sido útil!