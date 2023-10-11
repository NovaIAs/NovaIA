Claro! Aqui está um código complexo em PERL:

```
#!/usr/bin/perl

use strict;
use warnings;

# Função recursiva para encontrar o fatorial de um número
sub fatorial {
    my $n = shift;
    return 1 if $n == 0;
    return $n * fatorial($n - 1);
}

# Função para verificar se um número é primo
sub is_primo {
    my $num = shift;
    return 0 if $num < 2;
    for (my $i = 2; $i <= sqrt($num); $i++) {
        return 0 if $num % $i == 0;
    }
    return 1;
}

# Função para gerar uma sequência de Fibonacci
sub fibonacci {
    my $n = shift;
    my @fib = (0, 1);
    for (my $i = 2; $i <= $n; $i++) {
        $fib[$i] = $fib[$i - 1] + $fib[$i - 2];
    }
    return @fib;
}

# Função principal
sub main {
    print "Digite um número inteiro positivo: ";
    my $numero = <STDIN>;
    chomp $numero;

    # Chama as funções e exibe os resultados
    my $fatorial = fatorial($numero);
    print "O fatorial de $numero é $fatorial.\n";

    if (is_primo($numero)) {
        print "$numero é um número primo.\n";
    } else {
        print "$numero não é um número primo.\n";
    }

    print "Os primeiros $numero números da sequência de Fibonacci são: ";
    my @fibonacci = fibonacci($numero);
    print join(", ", @fibonacci) . ".\n";
}

# Chama a função principal
main();
```

Neste código em PERL, eu criei três funções diferentes:

1. A função `fatorial` recebe um número inteiro positivo e calcula o seu fatorial de forma recursiva. Ela utiliza o operador ternário `return` para retornar 1 quando o número é igual a 0 e o resultado da multiplicação do número pelo fatorial do número anterior quando o número é diferente de 0.

2. A função `is_primo` verifica se um número é primo. Ela recebe um número inteiro e utiliza um laço `for` para iterar de 2 até a raiz quadrada do número. Se o número for divisível por algum dos valores do laço, a função retorna 0, caso contrário, retorna 1.

3. A função `fibonacci` gera uma sequência de Fibonacci com base em um número inteiro positivo fornecido. Ela utiliza um array para armazenar os valores da sequência e um laço `for` para gerar os valores de Fibonacci. A função retorna o array com os valores da sequência.

A função `main` é a função principal do código. Ela solicita ao usuário que digite um número inteiro positivo, chama as três funções e exibe os resultados na tela.

Espero que este código atenda às suas expectativas!