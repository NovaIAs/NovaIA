Claro! Aqui está um exemplo de código complexo em Perl:

```
#!/usr/bin/perl

use strict;
use warnings;

# Função recursiva para calcular o fatorial de um número
sub fatorial {
    my ($num) = @_;
    
    if ($num == 0) {
        return 1;
    } else {
        return $num * fatorial($num - 1);
    }
}

# Função para verificar se um número é primo
sub eh_primo {
    my ($num) = @_;
    
    if ($num <= 1) {
        return 0;
    }
    
    for (my $i = 2; $i <= sqrt($num); $i++) {
        if ($num % $i == 0) {
            return 0;
        }
    }
    
    return 1;
}

# Função para ordenar um array de números usando o algoritmo bubble sort
sub bubble_sort {
    my (@array) = @_;
    
    my $trocas;
    my $n = scalar(@array);
    
    do {
        $trocas = 0;
        
        for (my $i = 0; $i < $n-1; $i++) {
            if ($array[$i] > $array[$i+1]) {
                ($array[$i], $array[$i+1]) = ($array[$i+1], $array[$i]);
                $trocas = 1;
            }
        }
        
        $n--;
    } while ($trocas);
    
    return @array;
}

# Função principal
sub main {
    # Criar um array com 10 números
    my @numeros = (7, 2, 5, 1, 9, 3, 6, 4, 8, 0);
    
    # Imprimir o array original
    print "Array original: @numeros\n";
    
    # Calcular o fatorial de um número
    my $fatorial_de_5 = fatorial(5);
    print "O fatorial de 5 é: $fatorial_de_5\n";
    
    # Verificar se um número é primo
    my $numero = 17;
    if (eh_primo($numero)) {
        print "$numero é primo\n";
    } else {
        print "$numero não é primo\n";
    }
    
    # Ordenar o array usando bubble sort
    @numeros = bubble_sort(@numeros);
    
    # Imprimir o array ordenado
    print "Array ordenado: @numeros\n";
}

# Chamar a função principal
main();
```

Neste exemplo, criei um código em Perl que inclui três funções diferentes. A primeira função, `fatorial`, calcula o fatorial de um número de forma recursiva. A segunda função, `eh_primo`, verifica se um número é primo. A terceira função, `bubble_sort`, ordena um array de números usando o algoritmo bubble sort.

Na função principal, criei um array com 10 números e o imprimo na tela. Em seguida, calculo o fatorial de 5 usando a função `fatorial` e imprimo o resultado. Depois, verifico se o número 17 é primo usando a função `eh_primo` e imprimo o resultado correspondente. Por fim, ordeno o array usando a função `bubble_sort` e o imprimo novamente na tela.

Este código é apenas um exemplo de um programa complexo em Perl e pode ser adaptado e modificado de acordo com as necessidades específicas de um projeto.