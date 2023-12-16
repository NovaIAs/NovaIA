Claro! Aqui está um código complexo em Perl, explicado em detalhes:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Função principal
sub main {
    # Variáveis
    my $numero = 10;
    my @lista = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

    # Laço foreach para iterar sobre a lista
    foreach my $item (@lista) {
        # Chama a função recursiva
        my $resultado = recursao($item, $numero);

        # Imprime o resultado
        print "O resultado da recursão para o item $item é $resultado\n";
    }
}

# Função recursiva
sub recursao {
    # Parâmetros
    my ($item, $numero) = @_;

    # Condição de parada
    if ($numero <= 0) {
        return $item;
    }

    # Laço while
    while ($numero > 0) {
        # Condição if/else
        if ($item % 2 == 0) {
            $item = $item * 2;
            $numero--;
        } else {
            $item = $item + 2;
            $numero--;
        }
    }

    # Chamada recursiva
    return recursao($item, $numero);
}

# Chama a função principal
main();
```

Aqui está uma explicação do código:

1. Começamos definindo as diretivas `use strict;` e `use warnings;`. Elas são boas práticas para garantir um código mais seguro e sem erros.

2. Em seguida, definimos a função principal `main()`, onde a execução do programa começa.

3. Dentro da função principal, declaramos duas variáveis: `$numero` e `@lista`. `$numero` é inicializado com o valor 10 e `@lista` é inicializado com uma lista de números de 1 a 10.

4. Utilizamos um laço `foreach` para percorrer cada elemento da lista `@lista`.

5. Dentro do laço `foreach`, chamamos a função recursiva `recursao()` passando o elemento atual da lista e o valor da variável `$numero`.

6. A função `recursao()` recebe dois parâmetros: `$item` e `$numero`. Essa função realiza uma recursão baseada em duas condições: quando o `$numero` é menor ou igual a zero e quando o `$numero` é maior que zero.

7. No caso da condição de parada, quando `$numero` é menor ou igual a zero, a função retorna o valor de `$item`.

8. Caso a condição de parada não seja atendida, entramos em um laço `while` que executa enquanto `$numero` for maior que zero.

9. Dentro do laço `while`, há uma condição `if/else`. Se o `$item` for par, ele é multiplicado por 2 e `$numero` é decrementado. Caso contrário, `$item` é incrementado em 2 e `$numero` é decrementado.

10. Após o laço `while`, chamamos a função `recursao()` de forma recursiva, passando os novos valores de `$item` e `$numero`.

11. Por fim, a função `recursao()` retorna o resultado da recursão.

12. Voltando para a função `main()`, o resultado da recursão é impresso na tela junto com o item atual da lista.

13. Finalmente, chamamos a função `main()`, iniciando a execução do programa.

Espero que este código atenda às suas expectativas!