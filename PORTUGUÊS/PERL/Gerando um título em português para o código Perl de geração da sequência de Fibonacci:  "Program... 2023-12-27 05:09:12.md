Claro! Aqui está um código complexo em Perl, que gera uma sequência de Fibonacci usando recursão:

```perl
use strict;
use warnings;

sub fibonacci {
    my ($n) = @_;

    if ($n == 0) {
        return 0;
    } elsif ($n == 1) {
        return 1;
    } else {
        return fibonacci($n - 1) + fibonacci($n - 2);
    }
}

sub imprimir_sequencia_fibonacci {
    my ($num) = @_;

    print "Sequência de Fibonacci até o número $num:\n";

    for (my $i = 0; $i <= $num; $i++) {
        my $fibonacci = fibonacci($i);
        print "$fibonacci ";
    }

    print "\n";
}

imprimir_sequencia_fibonacci(10);
```

Neste código, começamos definindo a função `fibonacci` que recebe um número inteiro `$n` como argumento. Usando recursão, a função calcula o valor do termo da sequência de Fibonacci de acordo com a fórmula matemática.

Em seguida, temos a função `imprimir_sequencia_fibonacci`, que recebe um número `$num` como argumento. Essa função imprime a sequência de Fibonacci até o número fornecido, chamando a função `fibonacci` dentro de um loop.

No exemplo acima, chamamos a função `imprimir_sequencia_fibonacci` com o valor 10, o que resultará na impressão da sequência de Fibonacci até o décimo termo.

Espero que este código complexo em Perl possa demonstrar a versatilidade e a capacidade dessa linguagem de programação.