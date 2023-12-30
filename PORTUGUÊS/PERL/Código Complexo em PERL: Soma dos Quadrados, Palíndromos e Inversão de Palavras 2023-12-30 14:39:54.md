Claro, aqui está um exemplo de um código complexo em PERL:

```perl
use strict;
use warnings;

# Função para calcular a soma dos quadrados dos números pares
sub soma_quadrados_pares {
    my $limite = shift;
    my $soma = 0;
    
    foreach my $numero (1..$limite) {
        if ($numero % 2 == 0) {
            $soma += $numero ** 2;
        }
    }
    
    return $soma;
}

# Função para verificar se um número é um palíndromo
sub eh_palindromo {
    my $numero = shift;
    my $str = lc($numero);
    $str =~ s/[^a-z0-9]//g;
    
    return $str eq reverse($str);
}

# Função para inverter as palavras em uma frase
sub inverter_palavras {
    my $frase = shift;
    my @palavras = split(" ", $frase);
    my $frase_invertida = join(" ", reverse(@palavras));
    
    return $frase_invertida;
}

# Testando as funções

print "Soma dos quadrados dos números pares até 10: " . soma_quadrados_pares(10) . "\n";

print "Verificando se '12321' é um palíndromo: " . (eh_palindromo("12321") ? "Sim" : "Não") . "\n";

print "Invertendo as palavras de 'O rato roeu a roupa do rei de Roma': " . inverter_palavras("O rato roeu a roupa do rei de Roma") . "\n";
```

Explicação do código:

1. Começamos importando os módulos `strict` e `warnings`, para garantir a segurança e a exibição de avisos durante a execução do código.

2. Em seguida, definimos três funções:

- A função `soma_quadrados_pares` recebe um limite como parâmetro e calcula a soma dos quadrados dos números pares até esse limite. Utilizamos um loop `foreach` para iterar de 1 até o limite e verificamos se cada número é par usando o operador `%`. Caso seja par, adicionamos o seu quadrado à variável `$soma`.

- A função `eh_palindromo` recebe um número como parâmetro e verifica se ele é um palíndromo. Convertemos o número para minúsculas e removemos todos os caracteres que não sejam letras ou números usando uma expressão regular. Em seguida, comparamos a string original com a sua versão invertida usando a função `reverse`.

- A função `inverter_palavras` recebe uma frase como parâmetro e inverte a ordem das palavras. Primeiro, dividimos a frase em palavras usando a função `split` e o espaço como separador. Depois, juntamos as palavras invertidas usando a função `reverse` e o espaço como separador novamente.

3. Por fim, testamos as funções imprimindo os resultados na saída padrão usando a função `print`. Calculamos a soma dos quadrados dos números pares até 10, verificamos se o número "12321" é um palíndromo e invertemos as palavras da frase "O rato roeu a roupa do rei de Roma".