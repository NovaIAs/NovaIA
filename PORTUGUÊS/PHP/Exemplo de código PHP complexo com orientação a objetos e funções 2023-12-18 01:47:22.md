Claro! Aqui está um exemplo de código PHP complexo, que utiliza várias funcionalidades e é explicado em detalhes:

```php
<?php
// Classe Pessoa
class Pessoa {
    // Propriedades
    private $nome;
    private $idade;
    
    // Construtor
    public function __construct($nome, $idade) {
        $this->nome = $nome;
        $this->idade = $idade;
    }
    
    // Método para obter o nome
    public function getNome() {
        return $this->nome;
    }
    
    // Método para obter a idade
    public function getIdade() {
        return $this->idade;
    }
    
    // Método para verificar se a pessoa é maior de idade
    public function maiorDeIdade() {
        if ($this->idade >= 18) {
            return true;
        } else {
            return false;
        }
    }
}

// Função para calcular a média de uma lista de números
function calcularMedia($numeros) {
    $soma = array_sum($numeros);
    $media = $soma / count($numeros);
    
    return $media;
}

// Criação de objetos e utilização das funcionalidades
$pessoa1 = new Pessoa("João", 25);
$pessoa2 = new Pessoa("Maria", 17);

echo "Nome: " . $pessoa1->getNome() . "<br>";
echo "Idade: " . $pessoa1->getIdade() . "<br>";
echo "Maior de idade? ";
if ($pessoa1->maiorDeIdade()) {
    echo "Sim";
} else {
    echo "Não";
}
echo "<br><br>";

echo "Nome: " . $pessoa2->getNome() . "<br>";
echo "Idade: " . $pessoa2->getIdade() . "<br>";
echo "Maior de idade? ";
if ($pessoa2->maiorDeIdade()) {
    echo "Sim";
} else {
    echo "Não";
}
echo "<br><br>";

$numeros = [1, 2, 3, 4, 5];
$media = calcularMedia($numeros);
echo "A média dos números " . implode(", ", $numeros) . " é: " . $media;
?>
```

Neste código, criei uma classe chamada "Pessoa" que possui propriedades como nome e idade, além de métodos para obter o nome, idade e verificar se a pessoa é maior de idade. Em seguida, criei uma função chamada "calcularMedia" para calcular a média de uma lista de números.

Na parte principal do código, criei dois objetos da classe Pessoa e utilizei as funcionalidades para exibir informações sobre cada pessoa e verificar se são maiores de idade. Em seguida, utilizei a função "calcularMedia" para obter a média de uma lista de números e exibir o resultado.

Esse código mostra um exemplo de orientação a objetos, encapsulamento, uso de métodos e funções em PHP. É importante ressaltar que este é apenas um exemplo e que existem inúmeras possibilidades de códigos complexos em PHP, dependendo dos requisitos do projeto.