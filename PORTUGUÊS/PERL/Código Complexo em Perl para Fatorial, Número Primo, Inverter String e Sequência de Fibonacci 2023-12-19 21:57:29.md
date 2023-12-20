Claro! Aqui está um código complexo em Perl para você:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Função recursiva para calcular o fatorial de um número
sub fatorial {
    my ($n) = @_;
    if ($n <= 1) {
        return 1;
    }
    else {
        return $n * fatorial($n - 1);
    }
}

# Função para verificar se um número é primo
sub primo {
    my ($num) = @_;
    if ($num < 2) {
        return 0;
    }
    for (my $i = 2; $i <= sqrt($num); $i++) {
        if ($num % $i == 0) {
            return 0;
        }
    }
    return 1;
}

# Função para inverter uma string
sub inverter_string {
    my ($string) = @_;
    return scalar reverse $string;
}

# Função para calcular a sequência de Fibonacci de um número
sub fibonacci {
    my ($n) = @_;
    my @fib = (0, 1);
    for (my $i = 2; $i <= $n; $i++) {
        $fib[$i] = $fib[$i - 1] + $fib[$i - 2];
    }
    return \@fib;
}

# Função principal
sub main {
    print "Digite um número: ";
    my $numero = <STDIN>;
    chomp $numero;
    
    print "O fatorial de $numero é: " . fatorial($numero) . "\n";
    print "O número $numero é primo\n" if primo($numero);
    print "O número $numero não é primo\n" unless primo($numero);
    print "A string invertida é: " . inverter_string($numero) . "\n";
    print "A sequência de Fibonacci até $numero é: ";
    my $fibonacci = fibonacci($numero);
    print join(", ", @$fibonacci) . "\n";
}

# Chamada da função principal
main();
```

Este código em Perl contém várias funções distintas. 

A função `fatorial` é responsável por calcular o fatorial de um número utilizando recursão.

A função `primo` verifica se um número é primo, percorrendo os números de 2 até a raiz quadrada do número e verificando se há divisores.

A função `inverter_string` recebe uma string como entrada e retorna a mesma string invertida.

A função `fibonacci` calcula a sequência de Fibonacci até um determinado número.

A função `main` é a função principal que solicita um número ao usuário, chama as outras funções e imprime os resultados na saída.

Espero que este código complexo em Perl atenda às suas expectativas!