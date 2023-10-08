Claro! Abaixo está um código em PHP que cria um sistema de gerenciamento de tarefas. O sistema permite que os usuários cadastrem tarefas, marquem como concluídas, editem e excluam tarefas. Também incluí um sistema básico de autenticação de usuários.

```php
<?php

// Classe para representar uma tarefa
class Tarefa {
    private $id;
    private $titulo;
    private $descricao;
    private $concluida;

    public function __construct($id, $titulo, $descricao, $concluida) {
        $this->id = $id;
        $this->titulo = $titulo;
        $this->descricao = $descricao;
        $this->concluida = $concluida;
    }

    public function getId() {
        return $this->id;
    }

    public function getTitulo() {
        return $this->titulo;
    }

    public function getDescricao() {
        return $this->descricao;
    }

    public function isConcluida() {
        return $this->concluida;
    }
}

// Classe para armazenar as tarefas
class GerenciadorDeTarefas {
    private $tarefas;

    public function __construct() {
        $this->tarefas = [];
    }

    public function adicionarTarefa($titulo, $descricao) {
        $id = count($this->tarefas) + 1;
        $tarefa = new Tarefa($id, $titulo, $descricao, false);
        $this->tarefas[] = $tarefa;
    }

    public function getTarefas() {
        return $this->tarefas;
    }

    public function marcarConcluida($id) {
        foreach ($this->tarefas as $tarefa) {
            if ($tarefa->getId() == $id) {
                $tarefa->concluida = true;
                break;
            }
        }
    }

    public function editarTarefa($id, $titulo, $descricao) {
        foreach ($this->tarefas as $tarefa) {
            if ($tarefa->getId() == $id) {
                $tarefa->titulo = $titulo;
                $tarefa->descricao = $descricao;
                break;
            }
        }
    }

    public function excluirTarefa($id) {
        foreach ($this->tarefas as $key => $tarefa) {
            if ($tarefa->getId() == $id) {
                unset($this->tarefas[$key]);
                break;
            }
        }
        $this->tarefas = array_values($this->tarefas);
    }
}

// Classe para autenticação de usuários
class Autenticacao {
    public function autenticar($usuario, $senha) {
        // Lógica de autenticação aqui
        // Retorna true se autenticado com sucesso, false caso contrário
    }
}

// Exemplo de uso do sistema de gerenciamento de tarefas

$gerenciador = new GerenciadorDeTarefas();

// Adicionar uma tarefa
$gerenciador->adicionarTarefa("Comprar leite", "Comprar leite no mercado");

// Marcar uma tarefa como concluída
$gerenciador->marcarConcluida(1);

// Editar uma tarefa
$gerenciador->editarTarefa(1, "Comprar pão", "Comprar pão na padaria");

// Excluir uma tarefa
$gerenciador->excluirTarefa(1);

// Listar todas as tarefas
$tarefas = $gerenciador->getTarefas();
foreach ($tarefas as $tarefa) {
    echo "ID: " . $tarefa->getId() . "\n";
    echo "Título: " . $tarefa->getTitulo() . "\n";
    echo "Descrição: " . $tarefa->getDescricao() . "\n";
    echo "Concluída: " . ($tarefa->isConcluida() ? "Sim" : "Não") . "\n";
    echo "----------------------\n";
}
```

Esse código implementa um sistema básico de gerenciamento de tarefas em PHP. Ele utiliza classes para representar tarefas e o gerenciador de tarefas. A classe `Tarefa` representa uma tarefa individual, com propriedades como ID, título, descrição e status de conclusão. A classe `GerenciadorDeTarefas` é responsável por adicionar, marcar como concluída, editar e excluir tarefas. 

Além disso, também poderia ser adicionado um sistema de autenticação de usuários antes de permitir qualquer ação no sistema. A classe `Autenticacao` seria responsável por autenticar um usuário com base em um nome de usuário e senha. No exemplo acima, a lógica de autenticação foi omitida.

Espero que esse código atenda às suas expectativas!