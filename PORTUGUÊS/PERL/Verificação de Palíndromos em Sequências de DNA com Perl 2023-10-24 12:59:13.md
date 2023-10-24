Claro! Aqui está um exemplo de código complexo em Perl que realiza uma tarefa específica: verificar se uma sequência de DNA é um palíndromo. 

```
use strict;
use warnings;

sub eh_palindromo {
    my $sequencia = shift;
    my $complemento = reverse($sequencia);
    return $complemento eq $sequencia;
}

sub verificar_palindromos {
    my $sequencia_dna = shift;
    my $tamanho = length($sequencia_dna);
    my $inicio = 0;
    my $comprimento_minimo = 4;
    
    while ($inicio < $tamanho - $comprimento_minimo) {
        for my $comprimento ($comprimento_minimo .. $tamanho - $inicio) {
            my $sequencia = substr($sequencia_dna, $inicio, $comprimento);
            if (eh_palindromo($sequencia)) {
                print "Encontrado palíndromo: $sequencia\n";
            }
        }
        $inicio++;
    }
}

my $dna = "ACGTAGCTAAGTCGATCGATCGCTAGCTAGCTAGCTAGCTACGTACG";
verificar_palindromos($dna);
```

Este código começa definindo duas sub-rotinas. A primeira, `eh_palindromo`, recebe uma sequência de DNA e retorna verdadeiro se essa sequência for um palíndromo (ou seja, pode ser lida da mesma maneira de trás para frente). Para verificar isso, a função reverte a sequência e a compara com a sequência original.

A segunda sub-rotina, `verificar_palindromos`, recebe uma sequência de DNA e verifica se existem palíndromos de pelo menos 4 bases nessa sequência. Ela faz isso iterando sobre todas as possíveis substrings da sequência e chamando a função `eh_palindromo` para verificar se cada substring é um palíndromo. Se for encontrado um palíndromo, o código imprime a substring.

No exemplo fornecido, a sequência de DNA é "ACGTAGCTAAGTCGATCGATCGCTAGCTAGCTAGCTAGCTACGTACG". Você pode alterar essa sequência para testar diferentes casos.

Este código é apenas um exemplo complexo de uso do Perl para verificar palíndromos em sequências de DNA. É importante ressaltar que existem muitas abordagens diferentes para resolver esse problema e este código é apenas uma delas.