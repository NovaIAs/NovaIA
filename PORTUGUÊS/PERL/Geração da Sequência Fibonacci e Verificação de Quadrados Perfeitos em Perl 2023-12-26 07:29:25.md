Claro! Abaixo está um código em Perl que gera uma sequência Fibonacci e verifica se cada número é um quadrado perfeito. Ele também exibe uma mensagem personalizada para cada número da sequência:

```perl
use strict;
use warnings;

sub fibonacci {
    my $n = shift;
    my @fib = (0, 1);

    for my $i (2 .. $n) {
        $fib[$i] = $fib[$i - 1] + $fib[$i - 2];
    }

    return @fib;
}

sub is_quadrado_perfeito {
    my $num = shift;
    my $raiz = int(sqrt($num));
    
    return $raiz * $raiz == $num;
}

sub exibir_mensagem {
    my $num = shift;
    
    if ($num % 2 == 0) {
        print "O número $num é par.";
    } else {
        print "O número $num é ímpar.";
    }
    
    if (is_quadrado_perfeito($num)) {
        print " Além disso, $num é um quadrado perfeito!";
    }
    
    print "\n";
}

my $n = 10; # Número de elementos da sequência Fibonacci a serem gerados
my @sequencia = fibonacci($n);

for my $num (@sequencia) {
    exibir_mensagem($num);
}
```

Explicação do código:

1. Começamos importando as pragmas `strict` e `warnings` para garantir um código mais robusto e evitar erros comuns.
2. Em seguida, definimos a função `fibonacci` que recebe um número inteiro `n` como argumento e retorna uma lista com os primeiros `n` elementos da sequência Fibonacci. Utilizamos um loop `for` para calcular cada número da sequência e armazenamos os valores em um array `@fib`.
3. A função `is_quadrado_perfeito` recebe um número como argumento e verifica se ele é um quadrado perfeito. Para isso, calculamos a raiz quadrada do número utilizando a função `sqrt` e arredondamos para baixo com `int`. Em seguida, verificamos se o quadrado da raiz é igual ao número original.
4. A função `exibir_mensagem` recebe um número como argumento e exibe uma mensagem personalizada para ele. Primeiro, verificamos se o número é par ou ímpar usando o operador `%`. Em seguida, utilizamos a função `is_quadrado_perfeito` para verificar se o número é um quadrado perfeito e exibimos uma mensagem adicional, se necessário.
5. Definimos a variável `$n` para determinar quantos elementos da sequência Fibonacci queremos gerar. Neste exemplo, definimos como 10.
6. Chamamos a função `fibonacci` passando `$n` como argumento e armazenamos a sequência gerada na variável `@sequencia`.
7. Por fim, percorremos a sequência com um loop `for` e chamamos a função `exibir_mensagem` para cada número.

Espero que este código atenda à sua solicitação de ser complexo e diferente. Fique à vontade para ajustar os valores e adicionar mais funcionalidades, se necessário.