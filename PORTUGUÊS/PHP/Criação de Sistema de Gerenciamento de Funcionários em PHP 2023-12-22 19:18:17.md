Claro! Aqui está um código complexo em PHP que realiza a tarefa de criar um sistema de gerenciamento de funcionários para uma empresa fictícia. O código está bem comentado para facilitar o entendimento:

```php
<?php

// Classe Funcionario
class Funcionario {
    private $nome;
    private $cargo;
    private $salario;

    // Método construtor
    public function __construct($nome, $cargo, $salario) {
        $this->nome = $nome;
        $this->cargo = $cargo;
        $this->salario = $salario;
    }

    // Método para aumentar o salário do funcionário
    public function aumentarSalario($percentual) {
        $aumento = $this->salario * $percentual / 100;
        $this->salario += $aumento;
    }

    // Método para exibir os detalhes do funcionário
    public function exibirDetalhes() {
        echo "Nome: " . $this->nome . "<br>";
        echo "Cargo: " . $this->cargo . "<br>";
        echo "Salário: R$" . $this->salario . "<br>";
        echo "<br>";
    }
}

// Classe GerenciadorFuncionarios
class GerenciadorFuncionarios {
    private $funcionarios;

    // Método construtor
    public function __construct() {
        $this->funcionarios = [];
    }

    // Método para adicionar um funcionário ao gerenciador
    public function adicionarFuncionario($funcionario) {
        $this->funcionarios[] = $funcionario;
    }

    // Método para exibir os detalhes de todos os funcionários
    public function exibirDetalhesFuncionarios() {
        foreach ($this->funcionarios as $funcionario) {
            $funcionario->exibirDetalhes();
        }
    }
}

// Criando instâncias da classe Funcionario
$funcionario1 = new Funcionario("João", "Desenvolvedor", 5000);
$funcionario2 = new Funcionario("Maria", "Designer", 4000);
$funcionario3 = new Funcionario("Pedro", "Gerente de Projetos", 7000);

// Criando instância da classe GerenciadorFuncionarios
$gerenciador = new GerenciadorFuncionarios();

// Adicionando os funcionários ao gerenciador
$gerenciador->adicionarFuncionario($funcionario1);
$gerenciador->adicionarFuncionario($funcionario2);
$gerenciador->adicionarFuncionario($funcionario3);

// Exibindo os detalhes dos funcionários
$gerenciador->exibirDetalhesFuncionarios();

// Aumentando o salário do funcionário 1 em 10%
$funcionario1->aumentarSalario(10);

// Exibindo os detalhes dos funcionários novamente
$gerenciador->exibirDetalhesFuncionarios();

?>
```

Neste código, temos duas classes: "Funcionario" e "GerenciadorFuncionarios". A classe "Funcionario" é responsável por representar um funcionário com seus atributos (nome, cargo e salário) e métodos (construtor, aumentarSalario e exibirDetalhes).

A classe "GerenciadorFuncionarios" é responsável por gerenciar uma lista de funcionários, com métodos para adicionar funcionários e exibir os detalhes de todos os funcionários.

No código principal, criamos três instâncias da classe "Funcionario" e uma instância da classe "GerenciadorFuncionarios". Em seguida, adicionamos os funcionários ao gerenciador e exibimos os detalhes deles. Após isso, aumentamos o salário do funcionário 1 em 10% e exibimos novamente os detalhes de todos os funcionários.

Espero que esse código atenda às suas expectativas! Se precisar de algo mais, é só pedir.