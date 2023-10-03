Claro! Aqui está um exemplo de um código complexo em Perl que utiliza expressões regulares para realizar uma tarefa específica:

```perl
# Este código verifica se uma string contém um número de telefone válido no formato (xx) xxxx-xxxx

# String de exemplo contendo um número de telefone
my $string = "(11) 1234-5678";

# Expressão regular que valida o formato do número de telefone
my $regex = qr/^\(\d{2}\)\s\d{4}-\d{4}$/;

# Verifica se a string corresponde ao formato esperado
if ($string =~ $regex) {
    print "O número de telefone é válido!\n";
} else {
    print "O número de telefone é inválido!\n";
}
```

Explicação do código:

1. Definimos uma string de exemplo contendo um número de telefone.
2. Em seguida, criamos uma expressão regular chamada `$regex` utilizando a função `qr//`. Esta expressão regular é responsável por definir o padrão que o número de telefone deve seguir.
3. A expressão regular `^\(\d{2}\)\s\d{4}-\d{4}$` é composta por vários elementos:
   - `^` indica o início da string.
   - `\(\d{2}\)` corresponde a dois dígitos entre parênteses, representando o código de área.
   - `\s` representa um espaço em branco.
   - `\d{4}` indica quatro dígitos seguidos, representando a primeira parte do número de telefone.
   - `-` representa um hífen.
   - `\d{4}` indica mais quatro dígitos seguidos, representando a segunda parte do número de telefone.
   - `$` indica o fim da string.
4. Utilizando o operador `=~`, verificamos se a string de exemplo corresponde ao formato definido pela expressão regular.
5. Se a correspondência for encontrada, a mensagem "O número de telefone é válido!" é exibida. Caso contrário, a mensagem "O número de telefone é inválido!" é exibida.

Espero que este exemplo tenha atendido às suas expectativas!