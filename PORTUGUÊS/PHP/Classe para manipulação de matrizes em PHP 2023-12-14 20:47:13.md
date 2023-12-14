Certamente! Aqui está um exemplo de um código complexo em PHP:

```php
<?php

// Classe para manipulação de matrizes
class Matriz {
    private $linhas;
    private $colunas;
    private $dados;

    // Construtor da classe
    public function __construct($linhas, $colunas) {
        $this->linhas = $linhas;
        $this->colunas = $colunas;
        $this->dados = [];

        // Inicializa a matriz com valores aleatórios entre 1 e 100
        for ($i = 0; $i < $linhas; $i++) {
            $linha = [];

            for ($j = 0; $j < $colunas; $j++) {
                $linha[] = rand(1, 100);
            }

            $this->dados[] = $linha;
        }
    }

    // Função para exibir a matriz
    public function exibir() {
        for ($i = 0; $i < $this->linhas; $i++) {
            for ($j = 0; $j < $this->colunas; $j++) {
                echo $this->dados[$i][$j] . " ";
            }

            echo "<br>";
        }
    }

    // Função para somar duas matrizes
    public function somar(Matriz $outraMatriz) {
        if ($this->linhas != $outraMatriz->linhas || $this->colunas != $outraMatriz->colunas) {
            echo "As matrizes não possuem as mesmas dimensões e não podem ser somadas.";
            return;
        }

        $soma = new Matriz($this->linhas, $this->colunas);

        for ($i = 0; $i < $this->linhas; $i++) {
            for ($j = 0; $j < $this->colunas; $j++) {
                $soma->dados[$i][$j] = $this->dados[$i][$j] + $outraMatriz->dados[$i][$j];
            }
        }

        return $soma;
    }
}

// Criando duas matrizes aleatórias
$matriz1 = new Matriz(3, 3);
$matriz2 = new Matriz(3, 3);

// Exibindo as matrizes originais
echo "Matriz 1:<br>";
$matriz1->exibir();

echo "<br>Matriz 2:<br>";
$matriz2->exibir();

// Somando as matrizes
$somaMatrizes = $matriz1->somar($matriz2);

// Exibindo a matriz resultante da soma
echo "<br>Resultado da soma das matrizes:<br>";
$somaMatrizes->exibir();

?>
```

Neste código em PHP, criei uma classe chamada `Matriz` que permite a manipulação de matrizes. A classe possui propriedades para armazenar o número de linhas, colunas e os dados da matriz.

No construtor da classe, inicializo a matriz com valores aleatórios entre 1 e 100. A função `exibir` exibe a matriz na tela, percorrendo as células e imprimindo seus valores.

A função `somar` recebe como parâmetro outra matriz e verifica se as matrizes possuem as mesmas dimensões. Em caso afirmativo, realiza a soma das matrizes e retorna uma nova matriz resultante. Caso as matrizes não possuam as mesmas dimensões, uma mensagem de erro é exibida.

No código principal, crio duas matrizes aleatórias e as exibo na tela. Em seguida, realizo a soma das matrizes e exibo o resultado.

Espero que esse código atenda às suas expectativas!