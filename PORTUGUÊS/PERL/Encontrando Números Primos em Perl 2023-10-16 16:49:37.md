Claro! Aqui está um código complexo em Perl, que realiza a tarefa de encontrar todos os números primos em um intervalo específico:

```perl
#!/usr/bin/perl

use strict;
use warnings;

sub is_prime {
    my $number = shift;
    
    return 0 if $number < 2;
    
    for (my $i = 2; $i <= sqrt($number); $i++) {
        return 0 if $number % $i == 0;
    }
    
    return 1;
}

sub find_primes {
    my ($start, $end) = @_;
    
    my @primes;
    
    for (my $num = $start; $num <= $end; $num++) {
        push @primes, $num if is_prime($num);
    }
    
    return @primes;
}

print "Digite o número inicial do intervalo: ";
my $start = <STDIN>;
chomp $start;

print "Digite o número final do intervalo: ";
my $end = <STDIN>;
chomp $end;

my @primes = find_primes($start, $end);

print "Números primos encontrados no intervalo [$start, $end]:\n";
foreach my $prime (@primes) {
    print "$prime ";
}
print "\n";
```

Explicação do código:
- Começamos definindo uma função chamada `is_prime`, que verifica se um determinado número é primo. Ela recebe um número como parâmetro e retorna 0 se não for primo ou 1 se for primo.
- Dentro da função `is_prime`, verificamos se o número é menor que 2. Se for, retornamos 0, pois números menores que 2 não são primos.
- Em seguida, percorremos um loop de 2 até a raiz quadrada do número. Dentro do loop, verificamos se o número é divisível por cada valor do loop. Se for, retornamos 0, pois significa que não é primo.
- Se o número passar por todas as verificações, retornamos 1, indicando que é primo.
- A função `find_primes` recebe um intervalo inicial e final como parâmetros, e retorna uma lista de todos os números primos dentro desse intervalo.
- Dentro da função `find_primes`, inicializamos um array vazio chamado `@primes`.
- Em seguida, percorremos um loop de `$start` até `$end`. Para cada número dentro do intervalo, chamamos a função `is_prime` para verificar se é primo. Se for, adicionamos o número ao array `@primes` usando a função `push`.
- No final, retornamos o array `@primes`.
- Fora das funções, solicitamos ao usuário que digite o número inicial e final do intervalo pelo teclado, utilizando `STDIN`.
- Removemos a quebra de linha dos valores digitados pelo usuário usando `chomp`.
- Chamamos a função `find_primes` com os valores digitados pelo usuário e armazenamos o resultado na variável `@primes`.
- Por fim, exibimos os números primos encontrados no intervalo, percorrendo o array `@primes` com um loop `foreach` e imprimindo cada número na tela.