Claro! Aqui está um exemplo de um código em Perl que utiliza expressões regulares para analisar e manipular texto:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Definindo uma função que conta o número de ocorrências de uma palavra em um texto
sub contar_ocorrencias {
    my ($texto, $palavra) = @_;
    my $contador = 0;

    while ($texto =~ /$palavra/gi) {
        $contador++;
    }

    return $contador;
}

# Lendo o arquivo de entrada
open(my $arquivo, '<', 'texto.txt') or die "Não foi possível abrir o arquivo: $!";

# Lendo todo o conteúdo do arquivo
my $texto_completo = do { local $/; <$arquivo> };

# Fechando o arquivo
close($arquivo);

# Removendo caracteres especiais e convertendo para minúsculas
$texto_completo =~ s/[^\p{L}\s]//g;
$texto_completo = lc($texto_completo);

# Exibindo o texto completo
print "Texto completo:\n$texto_completo\n";

# Solicitando uma palavra ao usuário
print "Digite uma palavra para contar as ocorrências: ";
my $palavra = <STDIN>;
chomp($palavra);

# Contando o número de ocorrências da palavra no texto completo
my $ocorrencias = contar_ocorrencias($texto_completo, $palavra);

# Exibindo o resultado
print "A palavra '$palavra' ocorre $ocorrencias vezes no texto.\n";
```

Neste código, começamos definindo uma função chamada `contar_ocorrencias`, que recebe um texto e uma palavra como parâmetros e retorna o número de ocorrências dessa palavra no texto. Utilizamos uma expressão regular para fazer a contagem, usando a função `=~` para fazer a correspondência.

Em seguida, abrimos um arquivo de texto chamado "texto.txt" e lemos todo o seu conteúdo para a variável `$texto_completo`. Utilizamos a variável especial `$/` para definir o separador de linhas como vazio, o que nos permite ler todo o conteúdo do arquivo de uma só vez.

Após ler o arquivo, removemos os caracteres especiais e convertemos o texto para minúsculas. Para isso, utilizamos a função `s///` com uma expressão regular para substituir todos os caracteres que não são letras ou espaços em branco por nada. Em seguida, usamos a função `lc` para converter o texto para minúsculas.

Então, exibimos o texto completo na tela, para que o usuário possa visualizá-lo.

Após exibir o texto, solicitamos ao usuário que digite uma palavra para contar as ocorrências. Utilizamos a função `print` para exibir uma mensagem e a função `<STDIN>` para ler a entrada do usuário. Em seguida, usamos a função `chomp` para remover o caractere de nova linha da entrada.

Por fim, chamamos a função `contar_ocorrencias` passando o texto completo e a palavra digitada pelo usuário como argumentos. O resultado é armazenado na variável `$ocorrencias` e exibido na tela.

Espero que este código atenda às suas expectativas!