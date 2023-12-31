Claro! Aqui está um exemplo de um código em PHP complexo que realiza a análise de texto e retorna a frequência de ocorrência de cada palavra. O código utiliza uma abordagem de contagem de palavras com a implementação de uma classe de análise de texto.

```php
<?php

class AnalisadorTexto {
    private $texto;

    public function __construct($texto) {
        $this->texto = $texto;
    }

    private function limparTexto() {
        $this->texto = strtolower($this->texto);
        $this->texto = preg_replace('/[^a-z0-9áéíóúàèìòùâêîôûãõç\s]/', '', $this->texto);
    }

    private function contarPalavras() {
        $palavras = explode(" ", $this->texto);
        $contador = array();

        foreach ($palavras as $palavra) {
            if (array_key_exists($palavra, $contador)) {
                $contador[$palavra]++;
            } else {
                $contador[$palavra] = 1;
            }
        }

        arsort($contador);

        return $contador;
    }

    public function analisar() {
        $this->limparTexto();
        $contador = $this->contarPalavras();

        echo "Palavras mais frequentes:\n";

        foreach ($contador as $palavra => $frequencia) {
            echo $palavra . ": " . $frequencia . " ocorrências\n";
        }
    }
}

$texto = "Este é um exemplo de texto. Este texto é usado para demonstrar o funcionamento do código.";

$analisador = new AnalisadorTexto($texto);
$analisador->analisar();

?>
```

Explicação do código:

A classe `AnalisadorTexto` é responsável por receber um texto como entrada e realizar a análise. 

O método `__construct` é o construtor da classe e recebe o texto que será analisado como parâmetro.

O método `limparTexto` é responsável por remover caracteres especiais e converter todas as letras para minúsculas. Ele utiliza a função `strtolower` para converter o texto para minúsculo e a função `preg_replace` para remover todos os caracteres não alfanuméricos, exceto espaços em branco.

O método `contarPalavras` é responsável por contar a frequência de ocorrência de cada palavra no texto. Ele utiliza a função `explode` para separar o texto em palavras individuais e armazená-las em um array. Em seguida, percorre o array de palavras e utiliza um array associativo chamado `$contador` para armazenar a frequência de cada palavra. Se a palavra já existir no array, incrementa o contador em 1. Caso contrário, adiciona a palavra ao array com o contador igual a 1. Após a contagem, o array `$contador` é ordenado em ordem decrescente de frequência utilizando a função `arsort`.

O método `analisar` é responsável por chamar os métodos `limparTexto` e `contarPalavras` e exibir as palavras mais frequentes. Ele percorre o array `$contador` e exibe cada palavra e sua frequência na saída.

No exemplo, é criada uma instância da classe `AnalisadorTexto` com um texto de exemplo. Em seguida, o método `analisar` é chamado para realizar a análise e exibir as palavras mais frequentes.