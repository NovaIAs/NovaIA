Claro! Aqui está um exemplo de um código complexo em Perl que realiza uma tarefa específica: encontrar todos os números primos em um intervalo fornecido pelo usuário.

```perl
use strict;
use warnings;

# Função para verificar se um número é primo
sub is_prime {
    my ($num) = @_;
    return 0 if $num < 2;
    for my $divisor (2 .. sqrt $num) {
        return 0 if $num % $divisor == 0;
    }
    return 1;
}

# Solicita ao usuário o intervalo de números
print "Digite o valor inicial: ";
my $inicio = <STDIN>;
chomp($inicio);

print "Digite o valor final: ";
my $fim = <STDIN>;
chomp($fim);

# Encontra todos os números primos no intervalo fornecido
print "Números primos encontrados:\n";
for my $numero ($inicio .. $fim) {
    print "$numero\n" if is_prime($numero);
}
```

Neste código, usamos o módulo `strict` e `warnings` para auxiliar na detecção de erros e tornar o código mais seguro. 

Em seguida, definimos uma função chamada `is_prime` que verifica se um número é primo. A função itera por todos os possíveis divisores do número, até a raiz quadrada do número, verificando se há algum divisor que o divide sem deixar resto. Se um divisor é encontrado, o número não é primo e a função retorna 0. Caso contrário, o número é primo e a função retorna 1.

Em seguida, pedimos ao usuário para inserir o valor inicial e final do intervalo de números que desejamos analisar. Usamos a função `chomp` para remover a quebra de linha do final da entrada do usuário.

Depois disso, percorremos todos os números dentro do intervalo fornecido e chamamos a função `is_prime` para cada número. Se o número for primo, ele é impresso na tela.

Espero que este exemplo atenda às suas necessidades de um código complexo em Perl! Lembre-se de adaptar o código de acordo com suas necessidades específicas.