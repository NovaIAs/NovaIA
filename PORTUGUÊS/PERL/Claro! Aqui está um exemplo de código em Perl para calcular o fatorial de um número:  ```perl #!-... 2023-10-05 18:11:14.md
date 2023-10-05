Claro! Aqui está um exemplo de código complexo em Perl que faz uso de muitos recursos e é altamente personalizado:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Função recursiva para calcular o fatorial de um número
sub fatorial {
    my $n = shift;
    return 1 if $n <= 1;
    return $n * fatorial($n - 1);
}

# Sub-rotina para verificar se um número é primo
sub is_primo {
    my $num = shift;
    return 0 if $num <= 1;
    for (my $i = 2; $i <= sqrt($num); $i++) {
        return 0 if ($num % $i) == 0;
    }
    return 1;
}

# Função principal
sub main {
    print "Digite um número: ";
    my $numero = <STDIN>;
    chomp $numero;
    
    # Calcula o fatorial do número
    my $fatorial = fatorial($numero);
    
    # Verifica se o número é primo
    my $eh_primo = is_primo($numero);
    
    # Imprime os resultados
    print "O fatorial de $numero é $fatorial\n";
    if ($eh_primo) {
        print "$numero é um número primo\n";
    } else {
        print "$numero não é um número primo\n";
    }
}

# Chamada da função principal
main();
```

Neste código em Perl, estamos utilizando recursos avançados da linguagem, como sub-rotinas, recursividade e manipulação de entrada/saída. O objetivo do código é calcular o fatorial de um número fornecido pelo usuário e verificar se ele é primo.

Primeiro, definimos a função `fatorial` que recebe um número `n` como argumento e retorna o fatorial desse número. A função é implementada de forma recursiva, onde a condição de parada é quando `n` é menor ou igual a 1. Caso contrário, a função chama a si mesma passando `n-1` como argumento e multiplica o resultado pelo valor atual de `n`.

Em seguida, temos a sub-rotina `is_primo` que verifica se um número é primo. A função recebe um número `num` como argumento e retorna 0 se ele não for primo, ou 1 se for primo. Utilizamos um loop `for` para iterar de 2 até a raiz quadrada de `num` e verificamos se `num` é divisível por algum dos valores nesse intervalo. Se for divisível, sabemos que não é primo e retornamos 0. Caso contrário, retornamos 1.

Na função principal `main`, solicitamos ao usuário que digite um número e realizamos a leitura utilizando a função `STDIN`. Em seguida, removemos o caractere de nova linha utilizando `chomp`.

Depois disso, chamamos a função `fatorial` passando o número digitado como argumento e armazenamos o resultado em uma variável chamada `$fatorial`. Em seguida, chamamos a função `is_primo` passando o mesmo número e armazenamos o resultado em uma variável chamada `$eh_primo`.

Por fim, imprimimos os resultados na tela utilizando a função `print`. Exibimos o valor do fatorial do número digitado e, em seguida, verificamos se o número é primo e exibimos a mensagem correspondente.

Espero que esse código complexo em Perl atenda às suas expectativas! Lembre-se de adaptar e personalizar o código de acordo com suas necessidades.