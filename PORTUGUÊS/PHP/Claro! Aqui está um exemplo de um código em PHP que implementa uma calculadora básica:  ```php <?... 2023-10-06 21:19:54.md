Claro! Aqui está um exemplo de um código complexo em PHP que implementa uma calculadora com funcionalidades avançadas:

```php
<?php

// Classe principal da calculadora
class CalculadoraAvancada {
    private $memoria;
    
    public function __construct() {
        $this->memoria = 0;
    }
    
    public function adicionar($num) {
        $this->memoria += $num;
    }
    
    public function subtrair($num) {
        $this->memoria -= $num;
    }
    
    public function multiplicar($num) {
        $this->memoria *= $num;
    }
    
    public function dividir($num) {
        if ($num != 0) {
            $this->memoria /= $num;
        } else {
            throw new Exception("Não é possível dividir por zero.");
        }
    }
    
    public function potencia($num) {
        $this->memoria = pow($this->memoria, $num);
    }
    
    public function raizQuadrada() {
        $this->memoria = sqrt($this->memoria);
    }
    
    public function limparMemoria() {
        $this->memoria = 0;
    }
    
    public function getResultado() {
        return $this->memoria;
    }
}

// Exemplo de uso da calculadora
$calculadora = new CalculadoraAvancada();
$calculadora->adicionar(5);
$calculadora->multiplicar(2);
$calculadora->potencia(3);
$calculadora->dividir(10);
$calculadora->raizQuadrada();

echo "O resultado é: " . $calculadora->getResultado();

?>
```

Neste código, criamos a classe `CalculadoraAvancada` que possui vários métodos para realizar operações matemáticas avançadas. A classe inclui métodos para adicionar, subtrair, multiplicar e dividir valores, bem como calcular potências e raiz quadrada. Há também um método para limpar a memória da calculadora e um método para obter o resultado atual.

No exemplo de uso, instanciamos um objeto `CalculadoraAvancada` e realizamos várias operações sequencialmente. No final, exibimos o resultado na tela.

Este código é apenas um exemplo de uma calculadora avançada e pode ser expandido com funcionalidades adicionais, como operações trigonométricas, conversões de unidade, entre outras. Você pode personalizá-lo de acordo com suas necessidades específicas.