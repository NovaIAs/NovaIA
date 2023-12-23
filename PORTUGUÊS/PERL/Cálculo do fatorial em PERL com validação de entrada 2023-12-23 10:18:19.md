Certamente! Aqui está um código complexo em PERL que realiza uma tarefa específica.

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Função principal
sub main {
    print "Digite um número: ";
    my $numero = <STDIN>;
    chomp $numero;

    if ($numero =~ /^\d+$/) {
        my $resultado = calcular_fatorial($numero);
        print "O fatorial de $numero é: $resultado\n";
    } else {
        print "Entrada inválida. Por favor, digite apenas números inteiros.\n";
    }
}

# Função para calcular o fatorial de um número
sub calcular_fatorial {
    my ($num) = @_;

    my $fatorial = 1;

    for (my $i = 1; $i <= $num; $i++) {
        $fatorial *= $i;
    }

    return $fatorial;
}

# Chamada da função principal
main();
```

Este código em PERL solicita que o usuário digite um número inteiro e, em seguida, calcula o fatorial desse número. Vamos explicar algumas partes do código:

- A declaração `use strict;` e `use warnings;` é utilizada para habilitar um conjunto de regras de sintaxe rigorosas e exibir avisos durante a execução do código, o que ajuda a evitar erros comuns.

- A função `main` é o ponto de entrada do programa. Ele exibe uma mensagem solicitando um número ao usuário, lê a entrada do usuário usando `STDIN` e chama a função `calcular_fatorial` para calcular o fatorial do número fornecido.

- A função `calcular_fatorial` recebe um número como parâmetro e usa um loop `for` para calcular o fatorial desse número. O fatorial é calculado multiplicando o valor atual do fatorial pelo próximo número do loop. Por fim, o valor do fatorial é retornado.

- A linha `chomp $numero;` é usada para remover o caractere de nova linha (`\n`) que é inserido quando o usuário pressiona Enter após digitar o número.

- A expressão regular `if ($numero =~ /^\d+$/)` é usada para verificar se o número fornecido é um número inteiro válido. A expressão regular `^\d+$` verifica se o número consiste apenas em dígitos (0-9).

Ao executar este código em PERL, ele solicitará um número ao usuário, calculará o fatorial desse número e exibirá o resultado. Em caso de entrada inválida, será exibida uma mensagem de erro apropriada.

Este código é apenas um exemplo complexo e pode ser adaptado de várias maneiras para atender a diferentes requisitos.