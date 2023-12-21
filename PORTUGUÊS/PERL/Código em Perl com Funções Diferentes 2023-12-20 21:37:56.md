Claro! Aqui está um código complexo em Perl que realiza uma série de tarefas diferentes:

```perl
use strict;
use warnings;

# Função para verificar se um número é primo
sub eh_primo {
    my $numero = shift;
    if ($numero < 2) {
        return 0;
    }
    for (my $i = 2; $i <= sqrt($numero); $i++) {
        if ($numero % $i == 0) {
            return 0;
        }
    }
    return 1;
}

# Função para inverter uma string
sub inverter_string {
    my $string = shift;
    my $invertida = reverse $string;
    return $invertida;
}

# Função para calcular o fatorial de um número
sub fatorial {
    my $numero = shift;
    my $resultado = 1;
    for (my $i = 1; $i <= $numero; $i++) {
        $resultado *= $i;
    }
    return $resultado;
}

# Função para verificar se uma frase é um palíndromo
sub eh_palindromo {
    my $frase = shift;
    $frase =~ s/\s//g; # remover espaços em branco
    $frase = lc $frase; # converter para minúsculas
    my $invertida = inverter_string($frase);
    if ($frase eq $invertida) {
        return 1;
    }
    return 0;
}

# Função principal
sub main {
    # Verificar se um número é primo
    my $numero = 17;
    if (eh_primo($numero)) {
        print "$numero é primo\n";
    } else {
        print "$numero não é primo\n";
    }

    # Inverter uma string
    my $string = "O rato roeu a roupa do rei de Roma";
    my $invertida = inverter_string($string);
    print "String invertida: $invertida\n";

    # Calcular o fatorial de um número
    my $fatorial_numero = 5;
    my $resultado_fatorial = fatorial($fatorial_numero);
    print "Fatorial de $fatorial_numero é $resultado_fatorial\n";

    # Verificar se uma frase é um palíndromo
    my $frase = "Anotaram a data da maratona";
    if (eh_palindromo($frase)) {
        print "$frase é um palíndromo\n";
    } else {
        print "$frase não é um palíndromo\n";
    }
}

# Executar a função principal
main();
```

Este código em Perl possui algumas funções diferentes:

1. `eh_primo`: Recebe um número como parâmetro e verifica se ele é primo ou não. Utiliza um laço `for` para iterar sobre os números de 2 até a raiz quadrada do número e verifica se há algum divisor, retornando 1 se não há e 0 caso contrário.
2. `inverter_string`: Recebe uma string como parâmetro e retorna a string invertida. Utiliza a função `reverse` para inverter a string.
3. `fatorial`: Recebe um número como parâmetro e calcula o fatorial dele. Utiliza um laço `for` para multiplicar todos os números de 1 até o número fornecido.
4. `eh_palindromo`: Recebe uma frase como parâmetro e verifica se ela é um palíndromo. Remove os espaços em branco utilizando uma expressão regular e converte a frase para minúsculas. Em seguida, utiliza a função `inverter_string` para obter a versão invertida da frase e compara se elas são iguais, retornando 1 se sim e 0 caso contrário.

A função `main` é responsável por chamar e testar todas as funções anteriores. No exemplo fornecido, ela testa cada uma das funções com valores diferentes e imprime os resultados na saída padrão.