Certamente! Aqui está um código em PHP que exibe a sequência de Fibonacci até um determinado número, utilizando orientação a objetos e um método recursivo para calcular os números:

```php
<?php

class Fibonacci {
    private $limite;
    
    public function __construct($limite) {
        $this->limite = $limite;
    }
    
    public function calcularFibonacci() {
        for ($i = 0; $i < $this->limite; $i++) {
            echo $this->fibonacci($i) . ", ";
        }
    }
    
    private function fibonacci($n) {
        if ($n < 2) {
            return $n;
        }
        
        return $this->fibonacci($n - 1) + $this->fibonacci($n - 2);
    }
}

$limite = 10;

$fibonacci = new Fibonacci($limite);
$fibonacci->calcularFibonacci();

?>
```

Explicação do código:

1. A classe `Fibonacci` é criada para calcular e exibir a sequência de Fibonacci até um determinado limite.
2. O construtor da classe recebe o parâmetro `limite`.
3. O método `calcularFibonacci` é responsável por iterar até o limite especificado e chamar o método `fibonacci` para calcular e exibir o valor de cada número da sequência.
4. O método `fibonacci` é um método recursivo que retorna o valor do número de Fibonacci dado um índice `n`. Ele utiliza a definição matemática da sequência de Fibonacci para fazer os cálculos.
5. No final do código, um objeto `Fibonacci` é criado com um limite de 10 e o método `calcularFibonacci` é chamado para exibir a sequência até esse limite.

Esse código é uma implementação simples da sequência de Fibonacci em PHP, mas utilizando orientação a objetos e um método recursivo para calcular os valores. Ele exibirá os primeiros 10 números da sequência: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34.