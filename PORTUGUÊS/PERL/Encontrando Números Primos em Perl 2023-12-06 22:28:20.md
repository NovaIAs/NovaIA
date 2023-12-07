Claro! Aqui está um código complexo em Perl que exibe os números primos dentro de um intervalo especificado pelo usuário:

```perl
# Função para verificar se um número é primo
sub eh_primo {
  my ($numero) = @_;

  # Verifica se o número é menor que 2
  if ($numero < 2) {
    return 0;
  }

  # Verifica se o número é divisível por qualquer outro número menor que ele mesmo
  for (my $i = 2; $i <= sqrt($numero); $i++) {
    if ($numero % $i == 0) {
      return 0;
    }
  }

  return 1;
}

# Solicita ao usuário para inserir o intervalo de números
print "Digite o número inicial: ";
my $inicio = <STDIN>;
chomp $inicio;

print "Digite o número final: ";
my $fim = <STDIN>;
chomp $fim;

# Verifica os números primos dentro do intervalo especificado pelo usuário
print "Números primos encontrados: ";
for (my $num = $inicio; $num <= $fim; $num++) {
  if (eh_primo($num)) {
    print "$num ";
  }
}
print "\n";
```

Explicação do código:
1. Começamos definindo uma função chamada `eh_primo` para verificar se um número é primo. Ela recebe um número pelo parâmetro `$numero`.
2. Dentro da função, verificamos se o número é menor que 2, pois números menores que 2 não são primos. Retorna 0 caso seja menor que 2.
3. Utilizamos um laço `for` para iterar de 2 até a raiz quadrada do número. Para cada iteração, verificamos se o número é divisível por algum dos valores menores que ele mesmo. Se for divisível, retorna 0.
4. Caso contrário, retorna 1 para indicar que o número é primo.
5. Em seguida, pedimos ao usuário que insira o número inicial e o número final do intervalo que ele deseja verificar.
6. Utilizamos `chomp` para remover a nova linha do final das entradas do usuário.
7. Depois disso, entramos em um laço `for` para iterar em cada número dentro do intervalo especificado.
8. Chamamos a função `eh_primo` para verificar se o número é primo.
9. Se for primo, imprimimos o número.
10. Por fim, exibimos uma nova linha.

Esse é apenas um exemplo de código complexo em Perl que exibe os números primos dentro de um intervalo específico. É importante ressaltar que a complexidade pode variar de acordo com a necessidade do projeto e dos requisitos específicos.