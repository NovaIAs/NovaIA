Claro! Aqui está um exemplo de código em Perl que utiliza expressões regulares para validar a formatação de números de telefone no formato brasileiro:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub validar_telefone {
  my $telefone = shift;

  # Remove caracteres não numéricos
  $telefone =~ s/\D//g;

  # Verifica se o número de telefone possui 11 dígitos
  if (length($telefone) != 11) {
    return "Número de telefone inválido";
  }

  # Verifica se o DDD é válido (dois primeiros dígitos)
  my $ddd = substr($telefone, 0, 2);
  unless ($ddd =~ /^(0[1-9]|1[1-9]|2[1-9]|3[1-9]|4[1-9]|5[1-9]|6[1-9]|7[1-9]|8[1-9]|9[1-9])$/) {
    return "DDD inválido";
  }

  # Verifica se o número do telefone possui apenas dígitos repetidos
  if ($telefone =~ /^(.)\1+$/) {
    return "Número de telefone inválido";
  }

  # Verifica se o número do telefone possui apenas dígitos sequenciais
  if ($telefone =~ /^(?:1234567890|0987654321|9876543210|0123456789)$/){
    return "Número de telefone inválido";
  }

  # Retorna o número de telefone válido
  return "Número de telefone válido";
}

# Exemplos de números de telefone para testar a função validar_telefone
my @numeros = (
  "11987654321",
  "21987654321",
  "31987654321",
  "41987654321",
  "51987654321",
  "61987654321",
  "71987654321",
  "81987654321",
  "91987654321",
  "99999999999",
  "11111111111",
  "12345678909",
  "09876543210",
  "98765432100",
  "01234567890"
);

# Testa a função validar_telefone para cada número de telefone
foreach my $numero (@numeros) {
  my $resultado = validar_telefone($numero);
  print "$numero: $resultado\n";
}
```

Neste código, a função `validar_telefone` recebe um número de telefone como entrada e retorna uma mensagem indicando se o número é válido ou inválido.

Primeiro, a função remove todos os caracteres não numéricos do número de telefone utilizando a expressão regular `$telefone =~ s/\D//g;`.

Em seguida, verifica-se se o número de telefone possui 11 dígitos. Caso contrário, a função retorna a mensagem "Número de telefone inválido".

A função também verifica se o DDD (dois primeiros dígitos) é válido. O padrão utilizado é `/^(0[1-9]|1[1-9]|2[1-9]|3[1-9]|4[1-9]|5[1-9]|6[1-9]|7[1-9]|8[1-9]|9[1-9])$/`, que corresponde a qualquer número entre 01 e 99. Caso o DDD não seja válido, a função retorna a mensagem "DDD inválido".

Em seguida, são feitas duas verificações adicionais:
- A função verifica se o número de telefone possui apenas dígitos repetidos utilizando a expressão regular `$telefone =~ /^(.)\1+$/`. Caso seja verdadeiro, a função retorna a mensagem "Número de telefone inválido".
- A função verifica se o número de telefone possui apenas dígitos sequenciais utilizando a expressão regular `$telefone =~ /^(?:1234567890|0987654321|9876543210|0123456789)$/`. Caso seja verdadeiro, a função retorna a mensagem "Número de telefone inválido".

Por fim, a função retorna a mensagem "Número de telefone válido" se todas as verificações forem bem-sucedidas.

O código também inclui uma série de exemplos de números de telefone para testar a função `validar_telefone`. Para cada número, a função é chamada e o resultado é exibido no console.