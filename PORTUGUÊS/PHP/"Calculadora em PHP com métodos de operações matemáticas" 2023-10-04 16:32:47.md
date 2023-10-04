Claro! Aqui está um exemplo de código em PHP que cria uma classe chamada "Calculadora" com diferentes métodos para realizar operações matemáticas básicas. Cada método é explicado em detalhes para facilitar o entendimento:

```php
<?php

// Definindo a classe Calculadora
class Calculadora {
    
    // Método para realizar a soma de dois números
    public function somar($num1, $num2) {
        $resultado = $num1 + $num2;
        return $resultado;
    }
    
    // Método para realizar a subtração de dois números
    public function subtrair($num1, $num2) {
        $resultado = $num1 - $num2;
        return $resultado;
    }
    
    // Método para realizar a multiplicação de dois números
    public function multiplicar($num1, $num2) {
        $resultado = $num1 * $num2;
        return $resultado;
    }
    
    // Método para realizar a divisão de dois números
    public function dividir($num1, $num2) {
        if ($num2 != 0) {
            $resultado = $num1 / $num2;
            return $resultado;
        } else {
            return "Não é possível dividir por zero.";
        }
    }
}

// Exemplo de utilização da classe Calculadora
$calculadora = new Calculadora();

$soma = $calculadora->somar(5, 3);
echo "A soma de 5 e 3 é: " . $soma . "<br>";

$subtracao = $calculadora->subtrair(10, 7);
echo "A subtração de 10 e 7 é: " . $subtracao . "<br>";

$multiplicacao = $calculadora->multiplicar(4, 6);
echo "A multiplicação de 4 e 6 é: " . $multiplicacao . "<br>";

$divisao = $calculadora->dividir(20, 5);
echo "A divisão de 20 por 5 é: " . $divisao . "<br>";

$divisaoPorZero = $calculadora->dividir(10, 0);
echo "Tentativa de divisão por zero: " . $divisaoPorZero . "<br>";

?>
```

Neste código em PHP, criamos a classe "Calculadora" que possui quatro métodos: "somar", "subtrair", "multiplicar" e "dividir". Cada método recebe dois parâmetros (num1 e num2) e realiza a operação correspondente, retornando o resultado.

No exemplo de utilização, instanciamos um objeto da classe Calculadora e chamamos cada um dos métodos, passando os valores desejados. Os resultados são exibidos na tela.

Além disso, incluímos uma verificação dentro do método "dividir" para evitar a divisão por zero. Caso o segundo número seja igual a zero, é exibida uma mensagem de erro.

Esse código é um exemplo simples, mas pode ser expandido com mais operações ou funcionalidades, tornando-o ainda mais complexo e personalizado para atender às necessidades específicas de um projeto.