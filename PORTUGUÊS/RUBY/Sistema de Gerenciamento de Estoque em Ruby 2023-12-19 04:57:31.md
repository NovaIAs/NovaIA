Claro! Vou gerar um código complexo em Ruby para você. Este código é um exemplo de um sistema de gerenciamento de estoque de uma loja virtual.

```ruby
class Produto
  attr_accessor :nome, :preco, :quantidade

  def initialize(nome, preco, quantidade)
    @nome = nome
    @preco = preco
    @quantidade = quantidade
  end

  def calcular_valor_total
    @preco * @quantidade
  end
end

class Estoque
  attr_accessor :produtos

  def initialize
    @produtos = []
  end

  def adicionar_produto(produto)
    @produtos << produto
  end

  def remover_produto(produto)
    @produtos.delete(produto)
  end

  def listar_produtos
    puts "Produtos em estoque:"
    @produtos.each do |produto|
      puts "Nome: #{produto.nome} | Preço: R$#{produto.preco} | Quantidade: #{produto.quantidade}"
    end
  end

  def calcular_valor_total_estoque
    valor_total = 0
    @produtos.each do |produto|
      valor_total += produto.calcular_valor_total
    end
    valor_total
  end
end

estoque = Estoque.new

while true
  puts "Sistema de Gerenciamento de Estoque"
  puts "1 - Adicionar produto"
  puts "2 - Remover produto"
  puts "3 - Listar produtos"
  puts "4 - Calcular valor total do estoque"
  puts "5 - Sair"
  print "Escolha uma opção: "
  opcao = gets.chomp.to_i

  case opcao
  when 1
    print "Digite o nome do produto: "
    nome = gets.chomp
    print "Digite o preço do produto: "
    preco = gets.chomp.to_f
    print "Digite a quantidade do produto: "
    quantidade = gets.chomp.to_i

    produto = Produto.new(nome, preco, quantidade)
    estoque.adicionar_produto(produto)
    puts "Produto adicionado com sucesso!"

  when 2
    print "Digite o nome do produto que deseja remover: "
    nome = gets.chomp

    produto = estoque.produtos.find { |p| p.nome == nome }
    if produto.nil?
      puts "Produto não encontrado!"
    else
      estoque.remover_produto(produto)
      puts "Produto removido com sucesso!"
    end

  when 3
    estoque.listar_produtos

  when 4
    valor_total_estoque = estoque.calcular_valor_total_estoque
    puts "O valor total do estoque é R$#{valor_total_estoque}"

  when 5
    puts "Saindo do sistema..."
    break

  else
    puts "Opção inválida! Tente novamente."
  end

  puts "\n"
end
```

Explicação do código:
- A classe `Produto` representa um produto no estoque e possui os atributos `nome`, `preco` e `quantidade`. Ela também possui um método `calcular_valor_total` que retorna o valor total do produto (preço multiplicado pela quantidade).
- A classe `Estoque` representa o estoque da loja e possui um array de `produtos`. Ela possui métodos para adicionar, remover e listar produtos, além de calcular o valor total do estoque.
- O código principal começa criando uma instância da classe `Estoque` e entra em um loop onde exibe um menu de opções para o usuário. Dependendo da opção escolhida, ele executa as ações correspondentes utilizando os métodos da classe `Estoque` e `Produto`.
- O loop continua até que o usuário escolha a opção "Sair" (opção 5).

Espero que este código atenda às suas expectativas! Lembre-se de adaptá-lo conforme suas necessidades específicas.