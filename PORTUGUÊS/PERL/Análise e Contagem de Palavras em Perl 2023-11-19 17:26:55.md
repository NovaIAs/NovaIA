Claro! Aqui está um exemplo de um código complexo em Perl que realiza a análise e contagem de palavras em um texto:

```perl
use strict;
use warnings;

my %palavras;

print "Digite o texto: ";
my $texto = <STDIN>;
chomp($texto);

$texto = lc($texto);
$texto =~ s/[[:punct:]]//g;

my @palavras = split(' ', $texto);

foreach my $palavra (@palavras) {
    $palavras{$palavra}++;
}

print "-----------\n";
print "Contagem de palavras:\n";
print "-----------\n";

foreach my $palavra (sort keys %palavras) {
    printf "%-15s %2d\n", $palavra, $palavras{$palavra};
}
```

Explicação do código:

1. As primeiras duas linhas são usadas para ativar as verificações rigorosas de sintaxe e exibir avisos de potenciais erros.

2. A próxima linha declara um hash chamado `%palavras` que será usado para armazenar as palavras e suas respectivas contagens.

3. O programa solicita que o usuário digite um texto, que é armazenado na variável `$texto`.

4. A função `chomp` é usada para remover a quebra de linha ao final do texto.

5. A função `lc` é usada para converter o texto em letras minúsculas.

6. A expressão regular `s/[[:punct:]]//g` é usada para remover todos os caracteres de pontuação do texto.

7. O texto é dividido em palavras individuais usando o espaço como delimitador e as palavras resultantes são armazenadas em um array chamado `@palavras`.

8. Em seguida, o programa utiliza um laço `foreach` para percorrer cada palavra no array `@palavras`.

9. Dentro do laço, a palavra é adicionada ao hash `%palavras` e a sua contagem é incrementada. Se a palavra já existir no hash, a contagem é atualizada.

10. Após o laço `foreach`, é exibida uma seção de separação para melhor visualização dos resultados, seguido do cabeçalho "Contagem de palavras".

11. Outro laço `foreach` é utilizado para percorrer cada par chave-valor no hash `%palavras`. As chaves (palavras) são ordenadas alfabeticamente usando a função `sort`.

12. Dentro desse laço, o formato `%15s %2d` é usado para exibir cada palavra seguida da sua contagem, com a palavra ocupando no máximo 15 caracteres e a contagem ocupando no máximo 2 caracteres.

Espero que esse código atenda às suas especificações!