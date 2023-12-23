Claro! Aqui está um exemplo de código complexo em Perl que utiliza várias funcionalidades da linguagem:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Sub-rotina para calcular o fatorial de um número
sub fatorial {
    my ($numero) = @_;
    return 1 if $numero == 0;
    return $numero * fatorial($numero - 1);
}

# Sub-rotina para calcular a sequência de Fibonacci
sub fibonacci {
    my ($n) = @_;
    return $n if $n < 2;
    return fibonacci($n - 1) + fibonacci($n - 2);
}

# Sub-rotina para verificar se um número é primo
sub primo {
    my ($numero) = @_;
    return 0 if $numero < 2;
    for (my $i = 2; $i <= sqrt($numero); $i++) {
        return 0 if $numero % $i == 0;
    }
    return 1;
}

# Sub-rotina para converter uma string em maiúsculas
sub converter_maiusculas {
    my ($texto) = @_;
    return uc($texto);
}

# Sub-rotina para imprimir a sequência de Collatz
sub collatz {
    my ($numero) = @_;
    print "$numero ";
    if ($numero == 1) {
        print "\n";
        return;
    }
    if ($numero % 2 == 0) {
        collatz($numero / 2);
    } else {
        collatz(3 * $numero + 1);
    }
}

# Programa principal
print "Digite um número: ";
my $numero = <STDIN>;
chomp $numero;

print "O fatorial de $numero é: " . fatorial($numero) . "\n";
print "O $numero° número da sequência de Fibonacci é: " . fibonacci($numero) . "\n";

if (primo($numero)) {
    print "$numero é um número primo.\n";
} else {
    print "$numero não é um número primo.\n";
}

print "Digite uma frase: ";
my $frase = <STDIN>;
chomp $frase;
print "A frase em maiúsculas é: " . converter_maiusculas($frase) . "\n";

print "A sequência de Collatz para o número $numero é: ";
collatz($numero);
```

Este código em Perl contém várias sub-rotinas que realizam diferentes tarefas:

1. A sub-rotina `fatorial` calcula o fatorial de um número usando uma função recursiva.
2. A sub-rotina `fibonacci` calcula o n-ésimo número da sequência de Fibonacci usando uma função recursiva.
3. A sub-rotina `primo` verifica se um número é primo ou não.
4. A sub-rotina `converter_maiusculas` converte uma string em maiúsculas usando a função `uc`.
5. A sub-rotina `collatz` imprime a sequência de Collatz para um número.

No programa principal, o usuário é solicitado a fornecer um número e uma frase. Em seguida, várias sub-rotinas são chamadas para realizar diferentes cálculos e operações com base nas entradas fornecidas. Os resultados são exibidos na saída padrão.

Espero que este código complexo em Perl atenda às suas expectativas!