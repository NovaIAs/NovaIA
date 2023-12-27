Claro! Aqui está um código complexo em Perl, com explicações detalhadas:

```
use strict;
use warnings;

# Função recursiva para calcular o fatorial de um número
sub fatorial {
    my $numero = shift; # Recebe o número fornecido como argumento

    if ($numero == 0) {
        return 1; # Fatorial de 0 é 1
    } else {
        return $numero * fatorial($numero - 1); # Chama a função recursivamente para calcular o fatorial
    }
}

# Sub-rotina para verificar se um número é primo
sub verificar_primo {
    my $numero = shift; # Recebe o número fornecido como argumento

    if ($numero <= 1) {
        return 0; # Números menores ou iguais a 1 não são primos
    }

    for (my $i = 2; $i <= sqrt($numero); $i++) {
        if ($numero % $i == 0) {
            return 0; # O número é divisível por algum número entre 2 e sua raiz quadrada, portanto não é primo
        }
    }

    return 1; # O número é primo
}

# Sub-rotina para calcular a sequência de Fibonacci
sub fibonacci {
    my $n = shift; # Recebe o número fornecido como argumento

    my @fibonacci = (0, 1); # Inicializa a sequência de Fibonacci com os dois primeiros números

    for (my $i = 2; $i <= $n; $i++) {
        push @fibonacci, $fibonacci[$i-1] + $fibonacci[$i-2]; # Calcula os próximos números da sequência
    }

    return @fibonacci; # Retorna a sequência completa
}

# Sub-rotina para inverter uma string
sub inverter_string {
    my $string = shift; # Recebe a string fornecida como argumento

    my $inverted_string = reverse $string; # Inverte a string

    return $inverted_string; # Retorna a string invertida
}

# Sub-rotina para calcular a média de uma lista de números
sub calcular_media {
    my @numeros = @_; # Recebe a lista de números fornecida como argumento

    my $soma = 0;
    my $quantidade = scalar @numeros;

    foreach my $numero (@numeros) {
        $soma += $numero; # Calcula a soma dos números
    }

    my $media = $soma / $quantidade; # Calcula a média

    return $media; # Retorna a média
}

# Exemplo de uso das sub-rotinas
my $numero = 5;
my $fatorial = fatorial($numero);
print "O fatorial de $numero é $fatorial\n";

my $numero2 = 7;
if (verificar_primo($numero2)) {
    print "$numero2 é um número primo\n";
} else {
    print "$numero2 não é um número primo\n";
}

my $n = 10;
my @fibonacci_sequence = fibonacci($n);
print "Os primeiros $n números da sequência de Fibonacci são: @fibonacci_sequence\n";

my $string = "Hello, world!";
my $inverted_string = inverter_string($string);
print "A string invertida é: $inverted_string\n";

my @numeros = (1, 2, 3, 4, 5);
my $media = calcular_media(@numeros);
print "A média dos números (@numeros) é: $media\n";
```

Neste código, utilizei sub-rotinas em Perl para realizar diferentes operações:

1. A função `fatorial` calcula o fatorial de um número usando recursão.
2. A sub-rotina `verificar_primo` verifica se um número é primo, utilizando um loop `for` e o conceito de divisibilidade.
3. A sub-rotina `fibonacci` calcula a sequência de Fibonacci até um determinado número, usando um loop `for` e um array para armazenar os resultados.
4. A sub-rotina `inverter_string` inverte uma string utilizando a função `reverse`.
5. A sub-rotina `calcular_media` calcula a média de uma lista de números, somando os valores e dividindo-os pela quantidade de elementos.

No exemplo de uso das sub-rotinas, é possível ver como chamá-las e imprimir os resultados na tela. O código pode ser executado e testado com diferentes valores e argumentos.