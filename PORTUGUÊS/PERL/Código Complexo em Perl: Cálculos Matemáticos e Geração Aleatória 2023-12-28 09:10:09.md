Certamente! Aqui está um exemplo de código complexo em Perl:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Sub-rotina recursiva para fatoração de números
sub fatoracao {
    my $numero = shift;
    
    my @fatores;
    my $divisor = 2;
    
    while ($numero > 1) {
        if ($numero % $divisor == 0) {
            push @fatores, $divisor;
            $numero /= $divisor;
        } else {
            $divisor++;
        }
    }
    
    return @fatores;
}

# Função para verificar se um número é primo
sub eh_primo {
    my $numero = shift;
    
    if ($numero < 2) {
        return 0;
    }
    
    for (my $divisor = 2; $divisor <= sqrt($numero); $divisor++) {
        if ($numero % $divisor == 0) {
            return 0;
        }
    }
    
    return 1;
}

# Função para calcular o fatorial de um número
sub fatorial {
    my $numero = shift;
    
    my $fatorial = 1;
    
    for (my $i = 2; $i <= $numero; $i++) {
        $fatorial *= $i;
    }
    
    return $fatorial;
}

# Função para calcular a sequência de Fibonacci
sub fibonacci {
    my $n = shift;
    
    my @fibonacci = (0, 1);
    
    for (my $i = 2; $i <= $n; $i++) {
        $fibonacci[$i] = $fibonacci[$i - 1] + $fibonacci[$i - 2];
    }
    
    return @fibonacci;
}

# Função para calcular o número de combinações
sub combinacoes {
    my ($n, $r) = @_;
    
    my $numerador = fatorial($n);
    my $denominador = fatorial($r) * fatorial($n - $r);
    
    return $numerador / $denominador;
}

# Função para gerar um número aleatório entre dois valores
sub numero_aleatorio {
    my ($min, $max) = @_;
    
    return int(rand($max - $min + 1)) + $min;
}

# Função para gerar uma string aleatória
sub string_aleatoria {
    my $tamanho = shift;
    
    my @caracteres = ('A'..'Z', 'a'..'z', 0..9);
    my $string = '';
    
    for (my $i = 0; $i < $tamanho; $i++) {
        $string .= $caracteres[numero_aleatorio(0, scalar(@caracteres) - 1)];
    }
    
    return $string;
}

# Programa principal
print "Digite um número para fatorar: ";
my $numero = <STDIN>;
chomp $numero;

print "\nFatores: ";
my @fatores = fatoracao($numero);
print join(", ", @fatores);

print "\n\nDigite um número para verificar se é primo: ";
my $numero_primo = <STDIN>;
chomp $numero_primo;

if (eh_primo($numero_primo)) {
    print "\nO número $numero_primo é primo.";
} else {
    print "\nO número $numero_primo não é primo.";
}

print "\n\nDigite um número para calcular o fatorial: ";
my $numero_fatorial = <STDIN>;
chomp $numero_fatorial;

my $fatorial = fatorial($numero_fatorial);
print "\nFatorial de $numero_fatorial: $fatorial";

print "\n\nDigite um número para gerar a sequência de Fibonacci: ";
my $numero_fibonacci = <STDIN>;
chomp $numero_fibonacci;

my @sequencia_fibonacci = fibonacci($numero_fibonacci);
print "\nSequência de Fibonacci: ";
print join(", ", @sequencia_fibonacci);

print "\n\nDigite o número total de elementos (n) e o número de elementos por grupo (r) para calcular as combinações: ";
my ($n_combinacoes, $r_combinacoes) = split(' ', <STDIN>);

my $numero_combinacoes = combinacoes($n_combinacoes, $r_combinacoes);
print "\nNúmero de combinações: $numero_combinacoes";

print "\n\nDigite o valor mínimo e máximo para gerar um número aleatório: ";
my ($min_aleatorio, $max_aleatorio) = split(' ', <STDIN>);

my $numero_aleatorio = numero_aleatorio($min_aleatorio, $max_aleatorio);
print "\nNúmero aleatório: $numero_aleatorio";

print "\n\nDigite o tamanho da string aleatória: ";
my $tamanho_string = <STDIN>;
chomp $tamanho_string;

my $string_aleatoria = string_aleatoria($tamanho_string);
print "\nString aleatória: $string_aleatoria\n";
```

Este código em Perl consiste em um programa com várias sub-rotinas que realizam diferentes cálculos matemáticos. Aqui estão as principais funcionalidades:

1. `fatoracao`: Esta sub-rotina recebe um número como entrada e retorna uma lista de seus fatores primos.
2. `eh_primo`: Esta sub-rotina verifica se um número é primo ou não. Retorna 1 se o número for primo e 0 caso contrário.
3. `fatorial`: Esta sub-rotina calcula o fatorial de um número.
4. `fibonacci`: Esta sub-rotina gera a sequência de Fibonacci até um determinado número.
5. `combinacoes`: Esta sub-rotina calcula o número de combinações possíveis dado um número total de elementos e um número de elementos por grupo.
6. `numero_aleatorio`: Esta sub-rotina gera um número aleatório entre dois valores fornecidos.
7. `string_aleatoria`: Esta sub-rotina gera uma string aleatória com base em um tamanho específico.

O programa principal solicita a entrada do usuário para realizar diferentes cálculos, como fatoração de números, verificação de números primos, cálculo do fatorial, geração da sequência de Fibonacci, cálculo de combinações, geração de números aleatórios e geração de strings aleatórias.

Espero que este código atenda às suas expectativas!