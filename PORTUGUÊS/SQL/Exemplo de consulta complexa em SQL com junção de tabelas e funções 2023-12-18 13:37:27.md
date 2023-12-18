Claro! Abaixo segue um exemplo de código complexo em SQL que realiza a junção de várias tabelas e utiliza diversas funções e cláusulas:

```sql
-- Criação da tabela "Clientes"
CREATE TABLE Clientes (
  id_cliente INT PRIMARY KEY,
  nome_cliente VARCHAR(100),
  endereco_cliente VARCHAR(200),
  cidade_cliente VARCHAR(100),
  telefone_cliente VARCHAR(20)
);

-- Criação da tabela "Pedidos"
CREATE TABLE Pedidos (
  id_pedido INT PRIMARY KEY,
  data_pedido DATE,
  id_cliente INT,
  FOREIGN KEY (id_cliente) REFERENCES Clientes(id_cliente)
);

-- Criação da tabela "ItensPedido"
CREATE TABLE ItensPedido (
  id_item INT PRIMARY KEY,
  id_pedido INT,
  id_produto INT,
  quantidade INT,
  FOREIGN KEY (id_pedido) REFERENCES Pedidos(id_pedido),
  FOREIGN KEY (id_produto) REFERENCES Produtos(id_produto)
);

-- Criação da tabela "Produtos"
CREATE TABLE Produtos (
  id_produto INT PRIMARY KEY,
  nome_produto VARCHAR(100),
  preco_produto DECIMAL(10,2),
  estoque_produto INT
);

-- Seleciona o nome do cliente, a data do pedido, o nome do produto e a quantidade de cada item de pedido
SELECT c.nome_cliente, p.data_pedido, pr.nome_produto, ip.quantidade
FROM Clientes c
INNER JOIN Pedidos p ON c.id_cliente = p.id_cliente
INNER JOIN ItensPedido ip ON p.id_pedido = ip.id_pedido
INNER JOIN Produtos pr ON ip.id_produto = pr.id_produto
WHERE p.data_pedido BETWEEN '2021-01-01' AND '2021-12-31'
ORDER BY c.nome_cliente, p.data_pedido;

-- Atualiza o estoque de produtos subtraindo a quantidade de itens de pedido vendidos
UPDATE Produtos
SET estoque_produto = estoque_produto - (
  SELECT SUM(ip.quantidade)
  FROM ItensPedido ip
  INNER JOIN Pedidos p ON ip.id_pedido = p.id_pedido
  WHERE ip.id_produto = Produtos.id_produto
  AND p.data_pedido BETWEEN '2021-01-01' AND '2021-12-31'
);

-- Inserção de um novo cliente e seus pedidos
INSERT INTO Clientes (id_cliente, nome_cliente, endereco_cliente, cidade_cliente, telefone_cliente)
VALUES (1, 'João Silva', 'Rua A, 123', 'São Paulo', '1234567890');

INSERT INTO Pedidos (id_pedido, data_pedido, id_cliente)
VALUES (1, '2021-01-05', 1);

INSERT INTO ItensPedido (id_item, id_pedido, id_produto, quantidade)
VALUES (1, 1, 1, 2);

INSERT INTO ItensPedido (id_item, id_pedido, id_produto, quantidade)
VALUES (2, 1, 2, 3);

-- Exclusão de um cliente e seus pedidos
DELETE FROM ItensPedido WHERE id_pedido IN (
  SELECT id_pedido
  FROM Pedidos
  WHERE id_cliente = 1
);

DELETE FROM Pedidos WHERE id_cliente = 1;

DELETE FROM Clientes WHERE id_cliente = 1;
```

Este código SQL cria quatro tabelas: "Clientes", "Pedidos", "Produtos" e "ItensPedido". Em seguida, realiza uma consulta que retorna o nome do cliente, a data do pedido, o nome do produto e a quantidade de cada item de pedido, filtrando apenas os pedidos realizados em 2021. Em seguida, atualiza o estoque dos produtos subtraindo a quantidade de itens de pedido vendidos. Por fim, insere um novo cliente e seus pedidos, e em seguida exclui o cliente e seus pedidos da base de dados.