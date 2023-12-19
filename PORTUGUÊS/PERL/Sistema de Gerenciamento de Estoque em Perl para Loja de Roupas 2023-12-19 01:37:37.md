Claro! Aqui está um exemplo de código complexo em PERL. Este código é uma simulação de um sistema de gerenciamento de estoque para uma loja de roupas.

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Declaração do hash que armazena os produtos e suas informações
my %estoque = (
    "001" => {
        nome     => "Camiseta",
        tamanho  => "M",
        cor      => "Azul",
        quantidade => 10,
        preco    => 25.99
    },
    "002" => {
        nome     => "Calça",
        tamanho  => "G",
        cor      => "Preta",
        quantidade => 5,
        preco    => 59.99
    },
    "003" => {
        nome     => "Vestido",
        tamanho  => "P",
        cor      => "Vermelho",
        quantidade => 3,
        preco    => 79.99
    }
);

# Função para exibir o menu de opções
sub exibir_menu {
    print "===== MENU =====\n";
    print "1. Visualizar Estoque\n";
    print "2. Adicionar Produto\n";
    print "3. Remover Produto\n";
    print "4. Atualizar Quantidade\n";
    print "5. Sair\n";
    print "================\n";
    print "Escolha uma opção: ";
}

# Função para exibir o estoque atual
sub visualizar_estoque {
    print "\n===== ESTOQUE =====\n";
    foreach my $codigo (keys %estoque) {
        print "Código: $codigo\n";
        print "Nome: $estoque{$codigo}{nome}\n";
        print "Tamanho: $estoque{$codigo}{tamanho}\n";
        print "Cor: $estoque{$codigo}{cor}\n";
        print "Quantidade: $estoque{$codigo}{quantidade}\n";
        print "Preço: R$ $estoque{$codigo}{preco}\n";
        print "-----------------\n";
    }
    print "\n";
}

# Função para adicionar um novo produto ao estoque
sub adicionar_produto {
    print "\n===== ADICIONAR PRODUTO =====\n";
    print "Digite o código do produto: ";
    my $codigo = <STDIN>;
    chomp $codigo;
    if (exists $estoque{$codigo}) {
        print "Erro: Produto já existe no estoque.\n";
    }
    else {
        print "Digite o nome do produto: ";
        my $nome = <STDIN>;
        chomp $nome;
        print "Digite o tamanho do produto: ";
        my $tamanho = <STDIN>;
        chomp $tamanho;
        print "Digite a cor do produto: ";
        my $cor = <STDIN>;
        chomp $cor;
        print "Digite a quantidade do produto: ";
        my $quantidade = <STDIN>;
        chomp $quantidade;
        print "Digite o preço do produto: ";
        my $preco = <STDIN>;
        chomp $preco;

        $estoque{$codigo} = {
            nome       => $nome,
            tamanho    => $tamanho,
            cor        => $cor,
            quantidade => $quantidade,
            preco      => $preco
        };

        print "Produto adicionado com sucesso.\n";
    }
}

# Função para remover um produto do estoque
sub remover_produto {
    print "\n===== REMOVER PRODUTO =====\n";
    print "Digite o código do produto: ";
    my $codigo = <STDIN>;
    chomp $codigo;
    if (exists $estoque{$codigo}) {
        delete $estoque{$codigo};
        print "Produto removido com sucesso.\n";
    }
    else {
        print "Erro: Produto não encontrado no estoque.\n";
    }
}

# Função para atualizar a quantidade de um produto no estoque
sub atualizar_quantidade {
    print "\n===== ATUALIZAR QUANTIDADE =====\n";
    print "Digite o código do produto: ";
    my $codigo = <STDIN>;
    chomp $codigo;
    if (exists $estoque{$codigo}) {
        print "Digite a nova quantidade: ";
        my $quantidade = <STDIN>;
        chomp $quantidade;
        $estoque{$codigo}{quantidade} = $quantidade;
        print "Quantidade atualizada com sucesso.\n";
    }
    else {
        print "Erro: Produto não encontrado no estoque.\n";
    }
}

# Loop principal do programa
my $opcao;
do {
    exibir_menu();
    $opcao = <STDIN>;
    chomp $opcao;

    if ($opcao eq "1") {
        visualizar_estoque();
    }
    elsif ($opcao eq "2") {
        adicionar_produto();
    }
    elsif ($opcao eq "3") {
        remover_produto();
    }
    elsif ($opcao eq "4") {
        atualizar_quantidade();
    }
    elsif ($opcao eq "5") {
        print "Saindo do programa...\n";
    }
    else {
        print "Opção inválida.\n";
    }
} while ($opcao ne "5");
```

Neste código, utilizamos hashes para armazenar as informações dos produtos no estoque. Cada produto é identificado por um código único e possui informações como nome, tamanho, cor, quantidade e preço.

O programa inicia exibindo um menu com opções para o usuário escolher. As opções incluem visualizar o estoque, adicionar um novo produto, remover um produto existente e atualizar a quantidade de um produto. O programa continua apresentando o menu até que o usuário escolha a opção de sair.

Cada função implementada é responsável por executar a lógica relacionada à sua respectiva opção do menu. Por exemplo, a função `adicionar_produto` solicita ao usuário as informações necessárias para adicionar um novo produto ao estoque e, em seguida, atualiza o hash `%estoque` com os dados fornecidos.

Espero que este exemplo de código tenha atendido às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, por favor, me avise.