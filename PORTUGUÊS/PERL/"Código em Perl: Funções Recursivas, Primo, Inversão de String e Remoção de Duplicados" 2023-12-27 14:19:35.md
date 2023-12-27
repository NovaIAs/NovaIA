Claro! Vou criar um código complexo em Perl e explicá-lo em detalhes para que você possa entender cada parte. Lembre-se de que o Perl é uma linguagem poderosa e flexível, então estarei criando um código único e diferenciado.

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Função recursiva para calcular o fatorial de um número
sub fatorial {
    my $n = shift;

    if ($n <= 1) {
        return 1;
    } else {
        return $n * fatorial($n - 1);
    }
}

# Função para calcular o número de Fibonacci de um índice específico
sub fibonacci {
    my $n = shift;

    if ($n <= 1) {
        return $n;
    } else {
        return fibonacci($n - 1) + fibonacci($n - 2);
    }
}

# Função para verificar se um número é primo
sub primo {
    my $n = shift;

    if ($n < 2) {
        return 0;
    }

    for my $i (2..sqrt($n)) {
        if ($n % $i == 0) {
            return 0;
        }
    }

    return 1;
}

# Função para inverter uma string
sub inverter_string {
    my $str = shift;
    my $str_invertida = reverse $str;

    return $str_invertida;
}

# Função para remover elementos duplicados de um array
sub remover_duplicados {
    my @array = @_;

    my %hash;
    foreach my $elemento (@array) {
        $hash{$elemento} = 1;
    }

    my @array_sem_duplicados = keys %hash;

    return @array_sem_duplicados;
}

# Testando as funções criadas
my $numero = 5;
my $fatorial = fatorial($numero);
print "O fatorial de $numero é: $fatorial\n";

my $indice_fibonacci = 10;
my $fibonacci = fibonacci($indice_fibonacci);
print "O número de Fibonacci no índice $indice_fibonacci é: $fibonacci\n";

my $numero_primo = 17;
if (primo($numero_primo)) {
    print "$numero_primo é um número primo.\n";
} else {
    print "$numero_primo não é um número primo.\n";
}

my $string = "Olá, mundo!";
my $string_invertida = inverter_string($string);
print "A string invertida é: $string_invertida\n";

my @array_com_duplicados = (1, 2, 3, 2, 4, 5, 3, 6, 7, 4);
my @array_sem_duplicados = remover_duplicados(@array_com_duplicados);
print "O array sem duplicados é: @array_sem_duplicados\n";
```

Explicação do código:

1. O código começa com a declaração do shebang `#!/usr/bin/perl`, que indica qual interpretador deve ser usado para executar o script.

2. Em seguida, são importados os módulos `strict` e `warnings`, que ajudam a identificar erros e a manter um código mais limpo e seguro.

3. A função `fatorial` é uma função recursiva que calcula o fatorial de um número. Ela recebe um argumento `$n` e verifica se é menor ou igual a 1. Se for, retorna 1. Caso contrário, retorna o produto de `$n` com o fatorial do número anterior.

4. A função `fibonacci` também é recursiva e calcula o número de Fibonacci de um índice específico. Assim como na função `fatorial`, ela verifica se o valor de `$n` é menor ou igual a 1 e retorna o próprio `$n` nesses casos. Caso contrário, retorna a soma dos números de Fibonacci dos dois índices anteriores.

5. A função `primo` verifica se um número é primo. Ela recebe um argumento `$n` e realiza um loop do número 2 até a raiz quadrada de `$n`. Se algum desses números for um divisor de `$n`, a função retorna 0, caso contrário, retorna 1.

6. A função `inverter_string` recebe uma string como argumento e utiliza a função `reverse` para inverter a ordem dos caracteres. O resultado é retornado.

7. A função `remover_duplicados` recebe um array como argumento. Ela utiliza um hash `%hash` para armazenar os elementos do array como chaves. Isso garante que nenhum elemento duplicado seja adicionado ao hash. Em seguida, as chaves do hash são retornadas como um novo array, que contém apenas os elementos únicos.

8. O código então testa as funções criadas com alguns exemplos. É calculado o fatorial de 5, o número de Fibonacci no índice 10 e verifica se o número 17 é primo. Além disso, inverte uma string e remove os elementos duplicados de um array.

Espero que este código complexo em Perl atenda às suas expectativas! Se tiver mais alguma solicitação ou dúvida, estou à disposição.