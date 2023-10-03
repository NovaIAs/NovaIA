Claro! Aqui está um código complexo em Perl que utiliza expressões regulares para extrair informações de um arquivo de texto com dados de alunos de uma escola:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Função para extrair informações dos alunos
sub extrair_informacoes_alunos {
    my ($arquivo) = @_;
    
    open(my $fh, '<', $arquivo) or die "Não foi possível abrir o arquivo: $!";
    
    my %alunos;
    
    while (my $linha = <$fh>) {
        chomp $linha;
        
        if ($linha =~ /^(\d+)\s+(.*?)\s+(\d{2})\/(\d{2})\/(\d{4})\s+(\d{2})\/(\d{2})\/(\d{4})$/) {
            my $matricula = $1;
            my $nome = $2;
            my $data_nascimento = "$4/$3/$5";
            my $data_matricula = "$7/$6/$8";
            
            $alunos{$matricula} = {
                nome => $nome,
                data_nascimento => $data_nascimento,
                data_matricula => $data_matricula
            };
        }
    }
    
    close($fh);
    
    return \%alunos;
}

# Exemplo de uso
my $arquivo_dados_alunos = 'dados_alunos.txt';
my $alunos = extrair_informacoes_alunos($arquivo_dados_alunos);

foreach my $matricula (sort keys %$alunos) {
    my $aluno = $alunos->{$matricula};
    print "Matrícula: $matricula\n";
    print "Nome: $aluno->{nome}\n";
    print "Data de Nascimento: $aluno->{data_nascimento}\n";
    print "Data de Matrícula: $aluno->{data_matricula}\n";
    print "\n";
}
```

Neste código, começamos importando os módulos `strict` e `warnings` para garantir a escrita correta do código e identificar possíveis problemas.

Em seguida, temos a função `extrair_informacoes_alunos`, que recebe o nome de um arquivo como parâmetro. Dentro dela, abrimos o arquivo e criamos um hash vazio chamado `%alunos`.

Em um loop, lemos cada linha do arquivo e removemos o caractere de nova linha com `chomp`. Utilizamos uma expressão regular para extrair as informações desejadas de cada linha.

A expressão regular `^(\d+)\s+(.*?)\s+(\d{2})\/(\d{2})\/(\d{4})\s+(\d{2})\/(\d{2})\/(\d{4})$` é usada para capturar a matrícula, nome, data de nascimento e data de matrícula de cada aluno. Ela faz o seguinte:

- `^` indica o início da linha.
- `(\d+)` captura um ou mais dígitos (matrícula).
- `\s+` captura um ou mais espaços em branco.
- `(.*?)` captura o nome do aluno, sendo o ponto de interrogação usado para tornar o quantificador não-guloso.
- `\s+` captura um ou mais espaços em branco.
- `(\d{2})\/(\d{2})\/(\d{4})` captura a data de nascimento no formato dd/mm/aaaa.
- `\s+` captura um ou mais espaços em branco.
- `(\d{2})\/(\d{2})\/(\d{4})` captura a data de matrícula no formato dd/mm/aaaa.
- `$` indica o fim da linha.

A cada iteração, armazenamos as informações do aluno em uma estrutura de dados chamada hash, utilizando a matrícula como chave e um hash aninhado contendo os dados do aluno como valor.

Após o loop, fechamos o arquivo e retornamos o hash `%alunos`.

Em seguida, temos um exemplo de uso da função `extrair_informacoes_alunos`. Definimos o nome do arquivo de dados dos alunos e chamamos a função passando-o como argumento. O retorno da função é armazenado na variável `$alunos`.

Em seguida, percorremos o hash `%$alunos` ordenando as chaves (matrículas) e imprimimos as informações de cada aluno, incluindo a matrícula, nome, data de nascimento e data de matrícula.

Esse código é útil para extrair informações específicas de um arquivo de texto com dados de alunos, como matrícula, nome, data de nascimento e data de matrícula. Ele utiliza expressões regulares para identificar e capturar as informações desejadas, tornando-o mais flexível e robusto.