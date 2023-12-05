Certamente! Aqui está um código em PHP complexo que envolve manipulação de dados em um sistema de gerenciamento de funcionários. O código segue as melhores práticas e utiliza a abordagem Orientada a Objetos para oferecer flexibilidade e facilidade de manutenção.

```php
<? php
// Definindo uma classe base para representar um funcionário
class Funcionario {
  protected $nome;
  protected $idade;
  protected $salario;

  public function __construct($nome, $idade, $salario) {
    $this->nome = $nome;
    $this->idade = $idade;
    $this->salario = $salario;
  }

  public function obterNome() {
    return $this->nome;
  }

  public function obterIdade() {
    return $this->idade;
  }

  public function obterSalario() {
    return $this->salario;
  }

  public function aumentarSalario($percentual) {
    $this->salario += $this->salario * ($percentual / 100);
  }

  public function exibirInformacoes() {
    echo "Nome: " . $this->nome . ", Idade: " . $this->idade . ", Salário: R$" . $this->salario;
  }
}

// Definindo uma classe derivada para representar um gerente, que herda de Funcionario
class Gerente extends Funcionario {
  private $departamento;

  public function __construct($nome, $idade, $salario, $departamento) {
    parent::__construct($nome, $idade, $salario);
    $this->departamento = $departamento;
  }

  public function obterDepartamento() {
    return $this->departamento;
  }

  // Sobrescrevendo o método exibirInformacoes() da classe base
  public function exibirInformacoes() {
    parent::exibirInformacoes();
    echo ", Departamento: " . $this->departamento;
  }
}

// Exemplo de uso do código
$gerente = new Gerente("João", 35, 5000, "Vendas");
$gerente->aumentarSalario(10);
$gerente->exibirInformacoes();
?>
```

Neste código, temos uma classe base chamada "Funcionario" que possui atributos para nome, idade e salário, juntamente com métodos para obter informações e aumentar o salário do funcionário.

Também temos uma classe derivada chamada "Gerente", que herda da classe "Funcionario" e adiciona um atributo para o departamento do gerente. Essa classe também sobrescreve o método "exibirInformacoes()" da classe base para exibir informações adicionais relevantes para um gerente.

No exemplo de uso no final do código, criamos um objeto "Gerente" chamado "João", definindo os valores iniciais para nome, idade, salário e departamento. Em seguida, aumentamos o salário em 10% e exibimos as informações completas do gerente, que incluem o nome, idade, salário e departamento.