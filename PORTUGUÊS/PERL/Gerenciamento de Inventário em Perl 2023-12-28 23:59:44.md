Certamente! Aqui está um código complexo em Perl, que consiste em uma aplicação de gerenciamento de inventário de uma loja:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Definir uma classe para representar um item do inventário
package Item {
    sub new {
        my ($class, $codigo, $descricao, $preco) = @_;
        my $self = {
            codigo    => $codigo,
            descricao => $descricao,
            preco     => $preco,
        };
        bless $self, $class;
        return $self;
    }
    sub get_codigo {
        my ($self) = @_;
        return $self->{codigo};
    }
    sub get_descricao {
        my ($self) = @_;
        return $self->{descricao};
    }
    sub get_preco {
        my ($self) = @_;
        return $self->{preco};
    }
}

# Definir uma classe para representar o inventário
package Inventario {
    sub new {
        my ($class) = @_;
        my $self = {
            itens => [],
        };
        bless $self, $class;
        return $self;
    }
    sub adicionar_item {
        my ($self, $item) = @_;
        push @{$self->{itens}}, $item;
    }
    sub remover_item {
        my ($self, $codigo) = @_;
        my @itens = @{$self->{itens}};
        for (my $i = 0; $i < scalar @itens; $i++) {
            if ($itens[$i]->get_codigo() eq $codigo) {
                splice @{$self->{itens}}, $i, 1;
                last;
            }
        }
    }
    sub listar_itens {
        my ($self) = @_;
        my @itens = @{$self->{itens}};
        if (scalar @itens == 0) {
            print "O inventário está vazio.\n";
        } else {
            print "Itens no inventário:\n";
            foreach my $item (@itens) {
                printf "Código: %s, Descrição: %s, Preço: R\$%.2f\n",
                    $item->get_codigo(),
                    $item->get_descricao(),
                    $item->get_preco();
            }
        }
    }
}

# Exemplo de uso da aplicação
my $inventario = Inventario->new();

# Adicionar itens ao inventário
my $item1 = Item->new("001", "Camiseta", 29.99);
my $item2 = Item->new("002", "Calça", 59.99);
my $item3 = Item->new("003", "Tênis", 99.99);
$inventario->adicionar_item($item1);
$inventario->adicionar_item($item2);
$inventario->adicionar_item($item3);

# Listar itens no inventário
$inventario->listar_itens();

# Remover um item do inventário
$inventario->remover_item("002");

# Listar itens novamente após a remoção
$inventario->listar_itens();
```

Neste código, temos duas classes em Perl: `Item` e `Inventario`. A classe `Item` representa um item do inventário e possui atributos como código, descrição e preço. A classe `Inventario` representa o inventário em si e possui métodos para adicionar, remover e listar itens.

O exemplo de uso da aplicação cria um inventário, adiciona alguns itens, lista os itens e, em seguida, remove um item específico do inventário antes de listar os itens novamente.

Espero que este código atenda às suas expectativas e demonstre a complexidade que você solicitou!