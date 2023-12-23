Claro! Aqui está um exemplo de um código complexo em Perl, que realiza uma tarefa específica:

```perl
# Este código em Perl é um script que recebe um arquivo de entrada contendo uma lista de números e calcula a média, o valor máximo e o valor mínimo dos números presentes no arquivo.

use strict;
use warnings;

# Solicita ao usuário o nome do arquivo de entrada
print "Digite o nome do arquivo de entrada: ";
my $nome_arquivo = <STDIN>;
chomp $nome_arquivo;

# Abre o arquivo de entrada
open(my $arquivo, '<', $nome_arquivo) or die "Não foi possível abrir o arquivo '$nome_arquivo': $!";

# Inicializa as variáveis para realizar os cálculos
my $soma = 0;
my $contador = 0;
my $maximo;
my $minimo;

# Lê cada linha do arquivo de entrada
while (my $linha = <$arquivo>) {
    chomp $linha;
    
    # Verifica se a linha contém um número válido
    if ($linha =~ /^\d+$/) {
        my $numero = int($linha);
        
        # Atualiza os valores de máximo e mínimo
        if (defined $maximo) {
            $maximo = $numero if $numero > $maximo;
        } else {
            $maximo = $numero;
        }
        
        if (defined $minimo) {
            $minimo = $numero if $numero < $minimo;
        } else {
            $minimo = $numero;
        }
        
        # Acumula a soma dos números
        $soma += $numero;
        
        # Incrementa o contador de números
        $contador++;
    }
}

# Fecha o arquivo de entrada
close($arquivo);

# Calcula a média
my $media = $soma / $contador;

# Imprime os resultados
print "Média: $media\n";
print "Valor máximo: $maximo\n";
print "Valor mínimo: $minimo\n";
```

Explicação do código:

1. Iniciamos o código importando os módulos `strict` e `warnings`, que ajudam a garantir a qualidade e a segurança do código.

2. Solicitamos ao usuário o nome do arquivo de entrada, onde cada linha deve conter um número.

3. Abrimos o arquivo de entrada usando a função `open()` e o operador de arquivo `<`. Caso ocorra algum erro, utilizamos `die` para encerrar o programa e exibir uma mensagem de erro.

4. Inicializamos as variáveis `$soma`, `$contador`, `$maximo` e `$minimo` para realizar os cálculos.

5. Em seguida, percorremos cada linha do arquivo utilizando um loop `while` e a função `<$arquivo>`. Utilizamos `chomp` para remover o caracter de quebra de linha.

6. Verificamos se a linha contém um número válido utilizando uma expressão regular (`$linha =~ /^\d+$/`). Se a linha for um número, realizamos as seguintes operações:
   - Convertemos o número para inteiro utilizando a função `int()`.
   - Atualizamos os valores de máximo e mínimo.
   - Acumulamos a soma dos números.
   - Incrementamos o contador.

7. Após percorrer todas as linhas do arquivo, fechamos o arquivo utilizando a função `close()`.

8. Calculamos a média dividindo a soma pelo contador.

9. Por fim, imprimimos os resultados na tela utilizando a função `print`.

Espero que este exemplo atenda às suas expectativas!