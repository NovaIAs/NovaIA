Certamente! Aqui está um código complexo em Perl que visa realizar uma tarefa específica: extrair informações de um arquivo de log e gerar um relatório detalhado.

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Abre o arquivo de log
open(my $log_file, "<", "arquivo_de_log.txt") or die "Erro ao abrir o arquivo de log: $!";

# Inicializa as variáveis para armazenar as estatísticas
my %estatisticas;
my $total_linhas = 0;

# Percorre cada linha do arquivo de log
while (my $linha = <$log_file>) {
    chomp $linha;

    # Extrai as informações relevantes da linha do log
    my ($data_hora, $tipo, $mensagem) = split /\t/, $linha;

    # Atualiza as estatísticas
    $estatisticas{$tipo}++;
    $total_linhas++;
}

# Fecha o arquivo de log
close($log_file);

# Imprime o relatório
print "Relatório de Log\n";
print "================\n\n";

print "Total de linhas no arquivo de log: $total_linhas\n\n";

print "Estatísticas:\n";
foreach my $tipo (sort keys %estatisticas) {
    my $ocorrencias = $estatisticas{$tipo};
    my $porcentagem = ($ocorrencias / $total_linhas) * 100;
    printf "%-20s: %d (%.2f%%)\n", $tipo, $ocorrencias, $porcentagem;
}
```

Explicação do código:

1. Começamos especificando que estamos usando o Perl (`#!/usr/bin/perl`).
2. Importamos os módulos `strict` e `warnings`, que são boas práticas para garantir um código mais robusto.
3. Abrimos o arquivo de log usando a função `open`, que recebe três argumentos: o arquivo de log, o modo de leitura (`<`) e a variável para armazenar o handle do arquivo (`$log_file`).
4. Verificamos se o arquivo foi aberto com sucesso. Caso contrário, exibimos uma mensagem de erro e encerramos o programa.
5. Inicializamos as variáveis `%estatisticas` para armazenar as estatísticas dos tipos de log e `$total_linhas` para contar o número total de linhas.
6. Iniciamos um loop `while` para percorrer cada linha do arquivo de log.
7. Utilizamos a função `chomp` para remover o caractere de quebra de linha do final da linha.
8. Utilizamos a função `split` para dividir a linha em três partes, separadas por tabulação (`\t`): a data e hora, o tipo de log e a mensagem.
9. Atualizamos as estatísticas dos tipos de log, incrementando o contador correspondente no hash `%estatisticas`.
10. Incrementamos também o contador `$total_linhas`.
11. Fechamos o arquivo de log com a função `close`.
12. Imprimimos o cabeçalho do relatório.
13. Imprimimos o total de linhas no arquivo de log.
14. Imprimimos as estatísticas, percorrendo cada tipo de log no hash `%estatisticas`, ordenados alfabeticamente.
15. Calculamos a porcentagem de ocorrências de cada tipo de log em relação ao total de linhas e imprimimos no formato adequado.

Espero que esse código complexo em Perl tenha atendido às suas expectativas! Se tiver mais alguma solicitação, estou à disposição.