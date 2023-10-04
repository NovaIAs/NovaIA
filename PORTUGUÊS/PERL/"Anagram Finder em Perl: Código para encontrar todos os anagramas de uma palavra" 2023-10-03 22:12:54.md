Aqui está um código complexo em Perl que realiza uma tarefa específica - encontrar todos os anagramas de uma palavra fornecida. Este código utiliza a técnica de recursão para gerar todas as combinações possíveis de letras e, em seguida, verifica se cada combinação é um anagrama válido.

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Função para encontrar anagramas
sub encontrar_anagramas {
    my ($prefixo, $palavra) = @_;

    # Caso base: se a palavra estiver vazia, imprimimos o anagrama encontrado
    if ($palavra eq '') {
        print "$prefixo\n";
        return;
    }

    for (my $i = 0; $i < length($palavra); $i++) {
        # Removendo uma letra da palavra original
        my $nova_palavra = $palavra;
        substr($nova_palavra, $i, 1) = '';

        # Chamada recursiva para encontrar anagramas com a nova palavra
        encontrar_anagramas($prefixo . substr($palavra, $i, 1), $nova_palavra);
    }
}

# Solicita a entrada da palavra ao usuário
print "Digite uma palavra: ";
my $palavra = <STDIN>;
chomp $palavra;

# Chama a função principal para encontrar anagramas
encontrar_anagramas('', $palavra);
```

Explicação do código:
1. Começamos importando os módulos `strict` e `warnings` para garantir um código mais seguro e livre de erros.
2. Em seguida, definimos uma função chamada `encontrar_anagramas`, que recebe dois parâmetros: `$prefixo` (representando o anagrama em construção) e `$palavra` (a palavra original fornecida).
3. Dentro da função `encontrar_anagramas`, primeiro verificamos se a palavra está vazia. Se estiver, imprimimos o anagrama encontrado e retornamos.
4. Caso contrário, percorremos cada letra da palavra original usando um loop `for`.
5. Para cada letra, removemos a letra correspondente da palavra original e armazenamos a nova palavra resultante na variável `$nova_palavra`.
6. Em seguida, fazemos uma chamada recursiva para a função `encontrar_anagramas`, passando o prefixo atualizado (adicionando a letra removida) e a nova palavra.
7. O processo recursivo continua até que a palavra esteja vazia, momento em que um anagrama válido é encontrado e impresso.
8. Após a definição da função, solicitamos ao usuário que digite uma palavra.
9. Em seguida, chamamos a função `encontrar_anagramas` com um prefixo vazio e a palavra fornecida pelo usuário como argumentos.

Esse código utiliza a recursão para gerar todas as combinações possíveis de letras, portanto, a complexidade do código aumenta exponencialmente com o tamanho da palavra fornecida. Como resultado, ele pode ser considerado complexo e diferenciado.