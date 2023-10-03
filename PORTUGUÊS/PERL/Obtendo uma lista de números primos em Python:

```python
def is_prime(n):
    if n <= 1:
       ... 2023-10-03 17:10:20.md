Claro! Aqui está um código em Perl complexo e diferenciado, juntamente com uma explicação detalhada:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Função para calcular o fatorial de um número
sub fatorial {
    my $numero = shift;
    my $resultado = 1;

    for (my $i = 1; $i <= $numero; $i++) {
        $resultado *= $i;
    }

    return $resultado;
}

# Função para verificar se um número é um número primo
sub eh_primo {
    my $numero = shift;

    if ($numero < 2) {
        return 0;
    }

    for (my $i = 2; $i <= sqrt($numero); $i++) {
        if ($numero % $i == 0) {
            return 0;
        }
    }

    return 1;
}

# Função para imprimir os números primos e seus fatoriais
sub imprimir_primos_fatoriais {
    my ($inicio, $fim) = @_;

    print "Números primos e seus fatoriais entre $inicio e $fim:\n";

    for (my $numero = $inicio; $numero <= $fim; $numero++) {
        if (eh_primo($numero)) {
            my $fatorial = fatorial($numero);
            print "$numero! = $fatorial\n";
        }
    }
}

# Código principal
imprimir_primos_fatoriais(1, 100);
```

Explicação do código:
1. Começamos definindo duas funções, `fatorial` e `eh_primo`, que serão utilizadas posteriormente no código principal.
2. A função `fatorial` recebe um número como argumento, calcula e retorna o fatorial desse número.
3. A função `eh_primo` recebe um número como argumento, verifica se é um número primo e retorna 1 se for verdadeiro ou 0 se for falso.
4. A função `imprimir_primos_fatoriais` recebe dois números, `inicio` e `fim`, e imprime todos os números primos e seus respectivos fatoriais dentro desse intervalo.
5. No código principal, chamamos a função `imprimir_primos_fatoriais` passando os números 1 e 100 como argumentos.
6. Dentro da função `imprimir_primos_fatoriais`, utilizamos um loop para percorrer todos os números dentro do intervalo especificado.
7. Para cada número, verificamos se é primo utilizando a função `eh_primo`. Se for primo, calculamos seu fatorial utilizando a função `fatorial` e imprimimos o número e seu fatorial.
8. Ao executar o código, ele irá imprimir os números primos e seus fatoriais entre 1 e 100.

Espero que esse código complexo em Perl atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais ajuda, é só me avisar.