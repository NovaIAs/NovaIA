Claro! Aqui está um código complexo em Perl, com comentários explicativos em português, que realiza a ordenação de um array de números utilizando o algoritmo Bubble Sort:

```perl
# Função para ordenar um array utilizando Bubble Sort
sub bubble_sort {
    my @array = @_;  # Recebe o array como parâmetro

    for my $i (0 .. $#array) {
        for my $j (0 .. $#array - 1 - $i) {
            # Compara elementos adjacentes
            if ($array[$j] > $array[$j + 1]) {
                # Realiza a troca caso o elemento atual seja maior que o próximo
                ($array[$j], $array[$j + 1]) = ($array[$j + 1], $array[$j]);
            }
        }
    }

    return @array;  # Retorna o array ordenado
}

# Array de exemplo
my @numeros = (9, 2, 5, 1, 7, 4, 8, 3, 6);

# Chama a função bubble_sort para ordenar o array
@numeros = bubble_sort(@numeros);

# Imprime o array ordenado
print "Array ordenado: @numeros\n";
```

Neste código, a função `bubble_sort` recebe um array como parâmetro e utiliza o algoritmo Bubble Sort para ordená-lo. O Bubble Sort compara elementos adjacentes e realiza trocas quando necessário, movendo o maior elemento para o final do array a cada iteração.

A função `bubble_sort` utiliza dois loops `for` aninhados para percorrer o array. O loop externo (`$i`) controla o número de iterações, enquanto o loop interno (`$j`) compara os elementos adjacentes e realiza as trocas.

Após a ordenação, o array é retornado pela função e, em seguida, é impresso no formato "Array ordenado: [elementos]" utilizando `print`.