Claro! Aqui está um código complexo em Perl, que tem como objetivo calcular o fatorial de um número fornecido pelo usuário. O código foi desenvolvido com o intuito de utilizar estruturas de controle, funções e manipulação de strings em Perl.

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Função para calcular o fatorial de um número
sub calcular_fatorial {
    my ($numero) = @_;
    my $resultado = 1;

    # Verifica se o número é válido
    if ($numero < 0) {
        return "Erro: O número deve ser não negativo.";
    }

    # Calcula o fatorial
    for (my $i = 1; $i <= $numero; $i++) {
        $resultado *= $i;
    }

    return $resultado;
}

# Solicita ao usuário um número para calcular o fatorial
print "Digite um número não negativo: ";
my $numero = <STDIN>;
chomp($numero);

# Chama a função para calcular o fatorial
my $resultado = calcular_fatorial($numero);

# Imprime o resultado
print "O fatorial de $numero é: $resultado\n";
```

Explicação do código:
1. Começamos importando os módulos `strict` e `warnings`, que nos ajudam a garantir um código mais limpo e livre de erros.
2. Em seguida, definimos uma função chamada `calcular_fatorial` que recebe um número como parâmetro. Essa função é responsável por calcular o fatorial desse número.
3. Dentro da função, inicializamos a variável `$resultado` com 1, pois o fatorial de 0 é 1.
4. Verificamos se o número fornecido é válido, ou seja, se é não negativo. Caso contrário, retornamos uma mensagem de erro.
5. Utilizamos um loop `for` para iterar de 1 até o número fornecido, multiplicando cada número pelo valor atual de `$resultado`.
6. Ao final do loop, retornamos o resultado calculado.
7. Na sequência, solicitamos ao usuário um número para calcular o fatorial, utilizando a função `print` para exibir a mensagem.
8. Utilizamos a função `chomp` para remover o caractere de nova linha (`\n`) do final da entrada do usuário.
9. Chamamos a função `calcular_fatorial` passando o número fornecido pelo usuário como argumento.
10. Armazenamos o resultado retornado pela função na variável `$resultado`.
11. Por fim, utilizamos a função `print` para exibir o número fornecido e o resultado calculado.

Esse código em Perl utiliza recursos como a declaração de sub-rotinas (`sub`), estruturas de controle (`if`, `for`), manipulação de strings (`chomp`) e entrada/saída de dados (`print`, `<STDIN>`).