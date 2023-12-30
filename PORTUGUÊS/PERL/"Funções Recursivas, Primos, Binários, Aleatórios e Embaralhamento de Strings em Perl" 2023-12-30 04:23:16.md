Claro! Aqui está um código em Perl complexo e diferenciado:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Função recursiva para calcular o fatorial de um número
sub fatorial {
    my ($n) = @_;
    return 1 if $n == 0;
    return $n * fatorial($n-1);
}

# Função para verificar se um número é primo
sub primo {
    my ($num) = @_;
    return 0 if $num <= 1;
    for (my $i = 2; $i <= sqrt($num); $i++) {
        return 0 if $num % $i == 0;
    }
    return 1;
}

# Função para converter um número decimal em binário
sub decimal_para_binario {
    my ($decimal) = @_;
    my $binario = '';
    while ($decimal > 0) {
        $binario = $decimal % 2 . $binario;
        $decimal = int($decimal / 2);
    }
    return $binario;
}

# Função para gerar um número aleatório
sub gerar_numero_aleatorio {
    my ($min, $max) = @_;
    return int(rand($max - $min + 1)) + $min;
}

# Função para embaralhar uma string
sub embaralhar_string {
    my ($string) = @_;
    my @caracteres = split('', $string);
    my $tamanho = scalar @caracteres;
    for (my $i = 0; $i < $tamanho; $i++) {
        my $j = gerar_numero_aleatorio(0, $tamanho-1);
        ($caracteres[$i], $caracteres[$j]) = ($caracteres[$j], $caracteres[$i]);
    }
    return join('', @caracteres);
}

# Exemplo de uso das funções
my $numero = 5;
print "O fatorial de $numero é: ", fatorial($numero), "\n";

print "Verificando se o número $numero é primo: ";
if (primo($numero)) {
    print "É primo\n";
} else {
    print "Não é primo\n";
}

my $decimal = 10;
print "O número $decimal em binário é: ", decimal_para_binario($decimal), "\n";

my $min = 1;
my $max = 100;
print "Gerando um número aleatório entre $min e $max: ", gerar_numero_aleatorio($min, $max), "\n";

my $string = "exemplo";
print "Embaralhando a string '$string': ", embaralhar_string($string), "\n";
```

Explicação do código:

1. A primeira linha `#!/usr/bin/perl` é chamada de shebang e indica ao sistema operacional que o arquivo é um script em Perl.
2. As próximas duas linhas `use strict` e `use warnings` são diretivas que garantem que o código seja escrito seguindo boas práticas e exibem mensagens de aviso em caso de erros.
3. Em seguida, definimos uma função chamada `fatorial` que calcula o fatorial de um número utilizando recursão.
4. A função `primo` verifica se um número é primo, utilizando um algoritmo simples de divisão.
5. A função `decimal_para_binario` converte um número decimal em binário, utilizando divisões sucessivas.
6. A função `gerar_numero_aleatorio` gera um número aleatório dentro de um intervalo especificado.
7. A função `embaralhar_string` embaralha os caracteres de uma string, trocando sua posição de forma aleatória.
8. Por fim, temos um exemplo de uso das funções, onde chamamos cada uma delas com diferentes argumentos e exibimos os resultados na tela.

Esse código em Perl demonstra o uso de funções, recursão, estruturas de controle e manipulação de strings. Cada função tem um propósito específico e pode ser útil em diferentes situações de programação.