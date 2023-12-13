Claro! Aqui está um código em Perl que calcula a soma dos números primos em um intervalo específico:

```perl
use strict;
use warnings;

# Função para verificar se um número é primo
sub eh_primo {
    my $num = shift;
    
    # Um número é primo se não for divisível por nenhum número além de 1 e ele mesmo
    for(my $i = 2; $i <= sqrt($num); $i++) {
        if($num % $i == 0) {
            return 0;
        }
    }
    
    return 1;
}

# Função para calcular a soma dos números primos em um intervalo
sub soma_primos {
    my ($inicio, $fim) = @_;
    my $soma = 0;

    # Percorre todos os números no intervalo especificado
    for(my $num = $inicio; $num <= $fim; $num++) {
        if(eh_primo($num)) {
            $soma += $num;
        }
    }

    return $soma;
}

# Intervalo de números para calcular a soma dos primos
my $inicio = 1;
my $fim = 100;

# Chamada da função para calcular a soma dos primos no intervalo especificado
my $soma_primos = soma_primos($inicio, $fim);

print "A soma dos números primos no intervalo de $inicio a $fim é: $soma_primos\n";
```

Explicação do código:

- Primeiro, importamos os módulos `strict` e `warnings` para garantir que o código esteja em conformidade com as boas práticas de programação e para ajudar a identificar possíveis erros e avisos durante a execução.

- Em seguida, definimos a função `eh_primo` que recebe um número como argumento e verifica se ele é primo. Essa função itera de 2 até a raiz quadrada do número e verifica se o número é divisível por algum outro número nesse intervalo. Se for divisível, retorna 0, caso contrário, retorna 1.

- Depois, temos a função `soma_primos` que recebe dois números, `$inicio` e `$fim`, que representam o intervalo no qual queremos calcular a soma dos números primos. A função inicializa a variável `$soma` como 0 e, em seguida, itera por todos os números no intervalo. Se um número for primo, adicionamos seu valor à variável `$soma`.

- Por fim, definimos os valores para `$inicio` e `$fim` do intervalo desejado e chamamos a função `soma_primos` passando esses valores como argumentos. O resultado é armazenado na variável `$soma_primos` e, em seguida, é exibido na tela com uma mensagem informativa.