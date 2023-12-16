Claro! Vou criar um código complexo em PHP que envolve um sistema de gerenciamento de funcionários em uma empresa. Esse código incluirá recursos como cadastro de funcionários, cálculo de salário, exibição de relatórios e muito mais. Vamos lá:

```php
<?php

class Funcionario {
    private $nome;
    private $cargo;
    private $salario;

    public function __construct($nome, $cargo, $salario) {
        $this->nome = $nome;
        $this->cargo = $cargo;
        $this->salario = $salario;
    }

    public function getNome() {
        return $this->nome;
    }

    public function getCargo() {
        return $this->cargo;
    }

    public function getSalario() {
        return $this->salario;
    }
}

class GerenciadorFuncionarios {
    private $funcionarios;

    public function __construct() {
        $this->funcionarios = [];
    }

    public function adicionarFuncionario(Funcionario $funcionario) {
        $this->funcionarios[] = $funcionario;
    }

    public function calcularSalarioTotal() {
        $total = 0;

        foreach ($this->funcionarios as $funcionario) {
            $total += $funcionario->getSalario();
        }

        return $total;
    }

    public function exibirRelatorioFuncionarios() {
        echo "Relatório de Funcionários:\n";

        foreach ($this->funcionarios as $funcionario) {
            echo "Nome: " . $funcionario->getNome() . ", Cargo: " . $funcionario->getCargo() . ", Salário: R$" . $funcionario->getSalario() . "\n";
        }
    }
}

// Exemplo de uso

$gerenciador = new GerenciadorFuncionarios();

$funcionario1 = new Funcionario("João", "Desenvolvedor", 5000);
$gerenciador->adicionarFuncionario($funcionario1);

$funcionario2 = new Funcionario("Maria", "Designer", 4000);
$gerenciador->adicionarFuncionario($funcionario2);

$funcionario3 = new Funcionario("Pedro", "Gerente de Projetos", 6000);
$gerenciador->adicionarFuncionario($funcionario3);

$totalSalarios = $gerenciador->calcularSalarioTotal();

echo "Total de Salários: R$" . $totalSalarios . "\n";

$gerenciador->exibirRelatorioFuncionarios();
```

Neste código, temos duas classes: `Funcionario` e `GerenciadorFuncionarios`. A classe `Funcionario` representa um funcionário em uma empresa e possui propriedades como nome, cargo e salário. A classe `GerenciadorFuncionarios` é responsável por armazenar e gerenciar uma lista de funcionários.

No exemplo de uso, criamos três funcionários e os adicionamos ao gerenciador de funcionários. Em seguida, calculamos o total de salários e exibimos um relatório dos funcionários cadastrados.

Esse código é apenas um exemplo, mas pode ser expandido com mais funcionalidades, como edição e remoção de funcionários, cálculos adicionais, entre outros recursos específicos para a gestão de funcionários em uma empresa.