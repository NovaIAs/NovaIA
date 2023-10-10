Claro! Aqui está um exemplo de código em PHP que envolve manipulação de arquivos, geração de números aleatórios e cálculos matemáticos:

```php
<?php

// Função para gerar um número aleatório com a quantidade de dígitos especificada
function gerarNumeroAleatorio($qtdDigitos) {
    $min = pow(10, $qtdDigitos - 1);
    $max = pow(10, $qtdDigitos) - 1;
    return mt_rand($min, $max);
}

// Função para calcular o fatorial de um número
function calcularFatorial($numero) {
    if ($numero < 0) {
        return "Não é possível calcular o fatorial de números negativos.";
    } elseif ($numero == 0) {
        return 1;
    } else {
        $resultado = 1;
        for ($i = 1; $i <= $numero; $i++) {
            $resultado *= $i;
        }
        return $resultado;
    }
}

// Gera um número aleatório com 5 dígitos
$numeroAleatorio = gerarNumeroAleatorio(5);

// Calcula o fatorial do número aleatório gerado
$fatorial = calcularFatorial($numeroAleatorio);

// Cria um arquivo de texto com o número aleatório e o seu fatorial
$nomeArquivo = "resultado.txt";
$conteudoArquivo = "Número aleatório: " . $numeroAleatorio . "\n";
$conteudoArquivo .= "Fatorial do número: " . $fatorial;
file_put_contents($nomeArquivo, $conteudoArquivo);

// Exibe o conteúdo do arquivo na tela
echo file_get_contents($nomeArquivo);

?>
```

Neste código em PHP, temos duas funções: `gerarNumeroAleatorio` e `calcularFatorial`. A primeira função recebe como parâmetro a quantidade de dígitos desejada e retorna um número aleatório com essa quantidade de dígitos. Utilizamos a função `mt_rand` para gerar o número aleatório dentro de um intervalo específico.

A segunda função, `calcularFatorial`, recebe um número como parâmetro e retorna o fatorial desse número. O fatorial é calculado utilizando um laço de repetição `for` que multiplica os números de 1 até o número informado.

O código principal gera um número aleatório com 5 dígitos utilizando a função `gerarNumeroAleatorio` e em seguida calcula o fatorial desse número utilizando a função `calcularFatorial`. O resultado é armazenado em uma variável.

Em seguida, o código cria um arquivo chamado "resultado.txt" e escreve nele o número aleatório gerado e o seu fatorial. Utilizamos a função `file_put_contents` para escrever o conteúdo no arquivo.

Por fim, o código lê o conteúdo do arquivo utilizando a função `file_get_contents` e exibe na tela. Isso permite visualizar o número aleatório e o fatorial gerados.

Espero que esse exemplo atenda às suas expectativas!